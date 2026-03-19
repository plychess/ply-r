// chess_engine.cpp — Bitboard chess engine implementation.
// The move generator follows the same bitboard model as the original Solidity
// prototype, but this code is maintained as a native C++ engine for R.

#include "chess_engine.h"
#include <sstream>
#include <algorithm>
#include <stdexcept>

namespace chess {

// ============================================================
// Precomputed attack tables
// ============================================================

uint64_t KNIGHT_ATTACKS[64];
uint64_t KING_ATTACKS[64];

static bool tables_initialized = false;

void init_attack_tables() {
    if (tables_initialized) return;
    // Precompute knight attack masks.
    for (int sq = 0; sq < 64; sq++) {
        uint64_t bb = 1ULL << sq;
        constexpr uint64_t notA  = 0xFEFEFEFEFEFEFEFEULL;
        constexpr uint64_t notAB = 0xFCFCFCFCFCFCFCFCULL;
        constexpr uint64_t notH  = 0x7F7F7F7F7F7F7F7FULL;
        constexpr uint64_t notGH = 0x3F3F3F3F3F3F3F3FULL;
        KNIGHT_ATTACKS[sq] =
            ((bb << 17) & notA)  | ((bb << 15) & notH)  |
            ((bb << 10) & notAB) | ((bb << 6)  & notGH) |
            ((bb >> 17) & notH)  | ((bb >> 15) & notA)  |
            ((bb >> 10) & notGH) | ((bb >> 6)  & notAB);
    }
    // Precompute king attack masks.
    for (int sq = 0; sq < 64; sq++) {
        uint64_t bb = 1ULL << sq;
        constexpr uint64_t notA = 0xFEFEFEFEFEFEFEFEULL;
        constexpr uint64_t notH = 0x7F7F7F7F7F7F7F7FULL;
        KING_ATTACKS[sq] =
            (bb << 8) | (bb >> 8) |
            ((bb << 1) & notA) | ((bb >> 1) & notH) |
            ((bb << 9) & notA) | ((bb >> 9) & notH) |
            ((bb << 7) & notH) | ((bb >> 7) & notA);
    }
    tables_initialized = true;
}

// Auto-initialize on load
namespace { struct Init { Init() { init_attack_tables(); } } _init; }

// ============================================================
// Attack generation
// ============================================================

uint64_t knight_attacks(uint8_t sq) {
    return KNIGHT_ATTACKS[sq];
}

uint64_t king_attacks(uint8_t sq) {
    return KING_ATTACKS[sq];
}

// Sliding ray attacks
uint64_t ray_attacks(uint8_t sq, int8_t df, int8_t dr, uint64_t occ, uint64_t own_occ) {
    uint64_t mask = 0;
    int8_t file = sq & 7;
    int8_t rank = sq >> 3;
    while (true) {
        file += df;
        rank += dr;
        if (file < 0 || file > 7 || rank < 0 || rank > 7) break;
        uint8_t to = static_cast<uint8_t>(rank * 8 + file);
        uint64_t bit = 1ULL << to;
        if (own_occ & bit) break;
        mask |= bit;
        if (occ & bit) break;
    }
    return mask;
}

uint64_t rook_attacks(uint8_t sq, uint64_t occ, uint64_t own_occ) {
    return ray_attacks(sq, 0, 1, occ, own_occ) |   // north
           ray_attacks(sq, 0, -1, occ, own_occ) |  // south
           ray_attacks(sq, 1, 0, occ, own_occ) |   // east
           ray_attacks(sq, -1, 0, occ, own_occ);   // west
}

uint64_t bishop_attacks(uint8_t sq, uint64_t occ, uint64_t own_occ) {
    return ray_attacks(sq, 1, 1, occ, own_occ) |    // NE
           ray_attacks(sq, -1, 1, occ, own_occ) |   // NW
           ray_attacks(sq, 1, -1, occ, own_occ) |   // SE
           ray_attacks(sq, -1, -1, occ, own_occ);   // SW
}

// ============================================================
// Init starting position
// ============================================================

void init_game(GameState& state) {
    std::memset(&state, 0, sizeof(state));
    state.bitboards[WHITE_OFFSET + PIECE_PAWN]   = 0x000000000000FF00ULL;
    state.bitboards[WHITE_OFFSET + PIECE_KNIGHT] = 0x0000000000000042ULL;
    state.bitboards[WHITE_OFFSET + PIECE_BISHOP] = 0x0000000000000024ULL;
    state.bitboards[WHITE_OFFSET + PIECE_ROOK]   = 0x0000000000000081ULL;
    state.bitboards[WHITE_OFFSET + PIECE_QUEEN]  = 0x0000000000000008ULL;
    state.bitboards[WHITE_OFFSET + PIECE_KING]   = 0x0000000000000010ULL;
    state.bitboards[BLACK_OFFSET + PIECE_PAWN]   = 0x00FF000000000000ULL;
    state.bitboards[BLACK_OFFSET + PIECE_KNIGHT] = 0x4200000000000000ULL;
    state.bitboards[BLACK_OFFSET + PIECE_BISHOP] = 0x2400000000000000ULL;
    state.bitboards[BLACK_OFFSET + PIECE_ROOK]   = 0x8100000000000000ULL;
    state.bitboards[BLACK_OFFSET + PIECE_QUEEN]  = 0x0800000000000000ULL;
    state.bitboards[BLACK_OFFSET + PIECE_KING]   = 0x1000000000000000ULL;
    state.sideToMove = COLOR_WHITE;
    state.castlingRights = 0x0F;
    state.enPassantSquare = NO_EP;
    state.halfMoveClock = 0;
    state.fullMoveNumber = 1;
}

// ============================================================
// King finding
// ============================================================

uint8_t find_king_square(const GameState& state, uint8_t color) {
    uint8_t idx = (color == COLOR_WHITE) ? WHITE_OFFSET + PIECE_KING : BLACK_OFFSET + PIECE_KING;
    return lsb_index(state.bitboards[idx]);
}

// ============================================================
// Attack detection
// ============================================================

bool is_square_attacked_with_occ(const GameState& state, uint8_t square, uint8_t attacker_color, uint64_t occ) {
    uint8_t off = (attacker_color == COLOR_WHITE) ? WHITE_OFFSET : BLACK_OFFSET;

    // Pawn attacks (reverse lookup)
    if (attacker_color == COLOR_WHITE) {
        uint64_t pawn_atk = 0;
        if (square >= 9 && (square & 7) >= 1) pawn_atk |= 1ULL << (square - 9);
        if (square >= 7 && (square & 7) <= 6) pawn_atk |= 1ULL << (square - 7);
        if (pawn_atk & state.bitboards[off + PIECE_PAWN]) return true;
    } else {
        uint64_t pawn_atk = 0;
        if (square <= 54 && (square & 7) >= 1) pawn_atk |= 1ULL << (square + 7);
        if (square <= 56 && (square & 7) <= 6) pawn_atk |= 1ULL << (square + 9);
        if (pawn_atk & state.bitboards[off + PIECE_PAWN]) return true;
    }

    // Knight
    if (knight_attacks(square) & state.bitboards[off + PIECE_KNIGHT]) return true;

    // King
    if (king_attacks(square) & state.bitboards[off + PIECE_KING]) return true;

    // Diagonal sliders (bishop + queen)
    uint64_t diag = state.bitboards[off + PIECE_BISHOP] | state.bitboards[off + PIECE_QUEEN];
    if (diag && (bishop_attacks(square, occ, 0) & diag)) return true;

    // Orthogonal sliders (rook + queen)
    uint64_t ortho = state.bitboards[off + PIECE_ROOK] | state.bitboards[off + PIECE_QUEEN];
    if (ortho && (rook_attacks(square, occ, 0) & ortho)) return true;

    return false;
}

bool is_square_attacked(const GameState& state, uint8_t square, uint8_t attacker_color) {
    uint64_t occ = side_occupancy(state, COLOR_WHITE) | side_occupancy(state, COLOR_BLACK);
    return is_square_attacked_with_occ(state, square, attacker_color, occ);
}

bool in_check(const GameState& state, uint8_t color) {
    uint8_t king_sq = find_king_square(state, color);
    uint8_t attacker = color ^ 1;
    return is_square_attacked(state, king_sq, attacker);
}

// ============================================================
// Pawn move helpers
// ============================================================

static bool is_double_pawn_push(uint8_t color, uint8_t from, uint8_t to) {
    if (color == COLOR_WHITE && (from >> 3) == 1 && to == from + 16) return true;
    if (color == COLOR_BLACK && (from >> 3) == 6 && to + 16 == from) return true;
    return false;
}

static bool is_en_passant_capture(const GameState& state, uint8_t color, uint8_t from, uint8_t to) {
    if (state.enPassantSquare == NO_EP) return false;
    if (to != state.enPassantSquare) return false;
    // Must be diagonal pawn capture
    uint8_t from_file = from & 7, to_file = to & 7;
    if (from_file == 0 && to_file == 7) return false;
    if (from_file == 7 && to_file == 0) return false;
    if (color == COLOR_WHITE) return (to == from + 7 || to == from + 9);
    return (to + 7 == from || to + 9 == from);
}

static bool is_promotion_square(uint8_t color, uint8_t square) {
    return (color == COLOR_WHITE) ? (square >= 56) : (square <= 7);
}

static uint8_t promotion_piece_type(uint8_t promo) {
    if (promo == 1) return PIECE_KNIGHT;
    if (promo == 2) return PIECE_BISHOP;
    if (promo == 3) return PIECE_ROOK;
    return PIECE_QUEEN;
}

static bool is_diagonal_pawn_capture(uint8_t color, uint8_t from, uint8_t to) {
    uint8_t ff = from & 7, tf = to & 7;
    if (ff == 0 && tf == 7) return false;
    if (ff == 7 && tf == 0) return false;
    if (color == COLOR_WHITE) return (to == from + 7 || to == from + 9);
    return (to + 7 == from || to + 9 == from);
}

static bool is_occupied_by_opponent(const GameState& state, uint8_t color, uint8_t square) {
    uint64_t mask = 1ULL << square;
    uint8_t opp_off = (color == COLOR_WHITE) ? BLACK_OFFSET : WHITE_OFFSET;
    for (int i = 0; i < 6; i++) {
        if (state.bitboards[opp_off + i] & mask) return true;
    }
    return false;
}

static bool is_promotion_ok(uint8_t color, uint8_t to, uint8_t promo) {
    if (!is_promotion_square(color, to)) return promo == 0;
    return promo >= 1 && promo <= 4;
}

// ============================================================
// Piece manipulation helpers
// ============================================================

static void move_piece(GameState& state, uint8_t piece_index, uint8_t from, uint8_t to) {
    uint64_t mask_from = 1ULL << from;
    uint64_t mask_to = 1ULL << to;
    state.bitboards[piece_index] = (state.bitboards[piece_index] & ~mask_from) | mask_to;
}

static void add_piece(GameState& state, uint8_t color, uint8_t piece_type, uint8_t square) {
    uint8_t idx = (color == COLOR_WHITE) ? WHITE_OFFSET + piece_type : BLACK_OFFSET + piece_type;
    state.bitboards[idx] |= (1ULL << square);
}

static void remove_piece_at_side(GameState& state, uint8_t square, uint8_t side_offset) {
    uint64_t bit = 1ULL << square;
    for (int i = 0; i < 6; i++) {
        if (state.bitboards[side_offset + i] & bit) {
            state.bitboards[side_offset + i] &= ~bit;
            return;
        }
    }
}

// ============================================================
// Castling helpers
// ============================================================

static bool is_castle_move(uint8_t color, uint8_t from, uint8_t to) {
    if (color == COLOR_WHITE && from == 4 && (to == 6 || to == 2)) return true;
    if (color == COLOR_BLACK && from == 60 && (to == 62 || to == 58)) return true;
    return false;
}

static void apply_castle(GameState& state, uint8_t color, uint8_t from, uint8_t to) {
    if (color == COLOR_WHITE && to == 6) {
        move_piece(state, WHITE_OFFSET + PIECE_KING, from, 6);
        move_piece(state, WHITE_OFFSET + PIECE_ROOK, 7, 5);
    } else if (color == COLOR_WHITE && to == 2) {
        move_piece(state, WHITE_OFFSET + PIECE_KING, from, 2);
        move_piece(state, WHITE_OFFSET + PIECE_ROOK, 0, 3);
    } else if (color == COLOR_BLACK && to == 62) {
        move_piece(state, BLACK_OFFSET + PIECE_KING, from, 62);
        move_piece(state, BLACK_OFFSET + PIECE_ROOK, 63, 61);
    } else {
        move_piece(state, BLACK_OFFSET + PIECE_KING, from, 58);
        move_piece(state, BLACK_OFFSET + PIECE_ROOK, 56, 59);
    }
}

static bool can_castle(const GameState& state, uint8_t color, uint8_t from, uint8_t to, uint64_t all_occ) {
    uint8_t rights = state.castlingRights;
    if (color == COLOR_WHITE) {
        if (to == 6 && !(rights & 0x01)) return false;
        if (to == 2 && !(rights & 0x02)) return false;
    } else {
        if (to == 62 && !(rights & 0x04)) return false;
        if (to == 58 && !(rights & 0x08)) return false;
    }

    uint8_t pass1, pass2, rook_from;
    if (color == COLOR_WHITE && to == 6)        { rook_from = 7;  pass1 = 5; pass2 = 6; }
    else if (color == COLOR_WHITE && to == 2)   { rook_from = 0;  pass1 = 3; pass2 = 2; }
    else if (color == COLOR_BLACK && to == 62)  { rook_from = 63; pass1 = 61; pass2 = 62; }
    else                                         { rook_from = 56; pass1 = 59; pass2 = 58; }

    // Path must be clear
    uint64_t path_mask = (1ULL << pass1) | (1ULL << pass2);
    if (color == COLOR_WHITE && to == 2)  path_mask |= 2ULL;         // b1
    if (color == COLOR_BLACK && to == 58) path_mask |= (1ULL << 57); // b8
    if (all_occ & path_mask) return false;

    // Rook must be present
    if (!(all_occ & (1ULL << rook_from))) return false;

    // King and pass squares must not be attacked
    uint8_t opp = color ^ 1;
    if (is_square_attacked_with_occ(state, from, opp, all_occ)) return false;
    if (is_square_attacked_with_occ(state, pass1, opp, all_occ)) return false;
    if (is_square_attacked_with_occ(state, pass2, opp, all_occ)) return false;

    return true;
}

static uint8_t clear_castling_for_king(uint8_t rights, uint8_t color) {
    return (color == COLOR_WHITE) ? (rights & 0x0C) : (rights & 0x03);
}

static uint8_t clear_castling_for_rook_move(uint8_t rights, uint8_t color, uint8_t from) {
    if (color == COLOR_WHITE) {
        if (from == 0) return rights & 0x0D;
        if (from == 7) return rights & 0x0E;
    } else {
        if (from == 56) return rights & 0x07;
        if (from == 63) return rights & 0x0B;
    }
    return rights;
}

static uint8_t clear_castling_for_rook_capture(uint8_t rights, uint8_t to) {
    if (to == 0)  return rights & 0x0D;
    if (to == 7)  return rights & 0x0E;
    if (to == 56) return rights & 0x07;
    if (to == 63) return rights & 0x0B;
    return rights;
}

// ============================================================
// Pseudo-legal ply validation
// ============================================================

// Path checking helper
static bool is_clear_path(uint64_t all_occ, uint8_t from, uint8_t to, int8_t step) {
    int8_t sq = static_cast<int8_t>(from) + step;
    while (sq != static_cast<int8_t>(to)) {
        if (sq < 0 || sq > 63) return false;
        if (all_occ & (1ULL << static_cast<uint8_t>(sq))) return false;
        sq += step;
    }
    return true;
}

static int8_t diagonal_step(int8_t rank_diff, int8_t file_diff) {
    int8_t rs = (rank_diff > 0) ? 1 : -1;
    int8_t fs = (file_diff > 0) ? 1 : -1;
    return rs * 8 + fs;
}

static bool is_legal_pawn_move(const GameState& state, uint8_t color,
                               uint8_t from, uint8_t to, uint8_t promo, uint64_t all_occ) {
    uint8_t from_rank = from >> 3, from_file = from & 7;
    uint8_t to_rank = to >> 3, to_file = to & 7;
    int8_t dir = (color == COLOR_WHITE) ? 1 : -1;

    if (from_file == to_file) {
        // Forward one
        if (static_cast<int8_t>(to_rank) == static_cast<int8_t>(from_rank) + dir &&
            !(all_occ & (1ULL << to))) {
            return is_promotion_ok(color, to, promo);
        }
        // Forward two
        if (is_double_pawn_push(color, from, to)) {
            uint8_t mid = (color == COLOR_WHITE) ? from + 8 : from - 8;
            return !(all_occ & (1ULL << mid)) && !(all_occ & (1ULL << to));
        }
        return false;
    }

    if (is_diagonal_pawn_capture(color, from, to)) {
        if (is_occupied_by_opponent(state, color, to))
            return is_promotion_ok(color, to, promo);
        if (is_en_passant_capture(state, color, from, to))
            return is_promotion_ok(color, to, promo);
    }
    return false;
}

static bool is_legal_knight_move(uint8_t from, uint8_t to) {
    uint8_t rd = abs_diff(from >> 3, to >> 3);
    uint8_t fd = abs_diff(from & 7, to & 7);
    return (rd == 2 && fd == 1) || (rd == 1 && fd == 2);
}

static bool is_legal_bishop_move(uint64_t all_occ, uint8_t from, uint8_t to) {
    int8_t rd = static_cast<int8_t>(to >> 3) - static_cast<int8_t>(from >> 3);
    int8_t fd = static_cast<int8_t>(to & 7) - static_cast<int8_t>(from & 7);
    if (abs8(rd) != abs8(fd)) return false;
    return is_clear_path(all_occ, from, to, diagonal_step(rd, fd));
}

static bool is_legal_rook_move(uint64_t all_occ, uint8_t from, uint8_t to) {
    if ((from >> 3) == (to >> 3)) {
        int8_t step = (to > from) ? 1 : -1;
        return is_clear_path(all_occ, from, to, step);
    }
    if ((from & 7) == (to & 7)) {
        int8_t step = (to > from) ? 8 : -8;
        return is_clear_path(all_occ, from, to, step);
    }
    return false;
}

static bool is_legal_king_move(const GameState& state, uint8_t color,
                               uint8_t from, uint8_t to, uint64_t all_occ) {
    uint8_t rd = abs_diff(from >> 3, to >> 3);
    uint8_t fd = abs_diff(from & 7, to & 7);
    if (rd <= 1 && fd <= 1) return true;
    if (is_castle_move(color, from, to))
        return can_castle(state, color, from, to, all_occ);
    return false;
}

bool is_pseudo_legal_ply(const GameState& state, uint32_t ply) {
    uint8_t from, to, promo;
    decode_ply(ply, from, to, promo);
    if (from > 63 || to > 63) return false;

    uint8_t color = state.sideToMove;
    uint8_t offset = (color == COLOR_WHITE) ? WHITE_OFFSET : BLACK_OFFSET;
    uint64_t from_mask = 1ULL << from;
    uint64_t own_occ = side_occupancy(state, color);

    if (!(own_occ & from_mask)) return false;

    // Find piece type
    uint8_t piece_type = 0;
    for (int i = 0; i < 6; i++) {
        if (state.bitboards[offset + i] & from_mask) {
            piece_type = static_cast<uint8_t>(i);
            break;
        }
    }

    uint64_t to_mask = 1ULL << to;
    if (own_occ & to_mask) return false;

    // Can't capture opponent king
    uint8_t opp_off = (color == COLOR_WHITE) ? BLACK_OFFSET : WHITE_OFFSET;
    if (state.bitboards[opp_off + PIECE_KING] & to_mask) return false;

    uint64_t opp_occ = side_occupancy(state, color ^ 1);
    uint64_t all_occ = own_occ | opp_occ;

    switch (piece_type) {
        case PIECE_PAWN:   return is_legal_pawn_move(state, color, from, to, promo, all_occ);
        case PIECE_KNIGHT: return is_legal_knight_move(from, to);
        case PIECE_BISHOP: return is_legal_bishop_move(all_occ, from, to);
        case PIECE_ROOK:   return is_legal_rook_move(all_occ, from, to);
        case PIECE_QUEEN:  return is_legal_bishop_move(all_occ, from, to) ||
                                  is_legal_rook_move(all_occ, from, to);
        case PIECE_KING:   return is_legal_king_move(state, color, from, to, all_occ);
        default: return false;
    }
}

// ============================================================
// Apply a ply to an in-memory copy of the position
// ============================================================

GameState apply_ply_to_memory(const GameState& state, uint32_t ply) {
    GameState next = state; // copy
    uint8_t from, to, promo;
    decode_ply(ply, from, to, promo);

    uint8_t color = next.sideToMove;
    uint8_t offset = (color == COLOR_WHITE) ? WHITE_OFFSET : BLACK_OFFSET;
    uint8_t opp_offset = (color == COLOR_WHITE) ? BLACK_OFFSET : WHITE_OFFSET;

    // Find mover
    uint64_t from_mask = 1ULL << from;
    uint8_t piece_type = 0;
    bool has_piece = false;
    for (int i = 0; i < 6; i++) {
        if (next.bitboards[offset + i] & from_mask) {
            piece_type = static_cast<uint8_t>(i);
            has_piece = true;
            break;
        }
    }
    if (!has_piece) return next;

    // Check dest for capture
    uint64_t to_mask = 1ULL << to;
    uint64_t opp_occ = side_occupancy(next, color ^ 1);
    bool dst_has_piece = (opp_occ & to_mask) != 0;
    bool is_capture = dst_has_piece;
    uint8_t piece_index = offset + piece_type;

    if (piece_type == PIECE_PAWN) {
        bool is_ep = is_en_passant_capture(next, color, from, to);
        next.enPassantSquare = NO_EP;

        if (is_ep) {
            uint8_t captured_sq = (color == COLOR_WHITE) ? to - 8 : to + 8;
            next.bitboards[opp_offset + PIECE_PAWN] &= ~(1ULL << captured_sq);
            is_capture = true;
        }

        if (is_double_pawn_push(color, from, to)) {
            next.enPassantSquare = (color == COLOR_WHITE) ? from + 8 : from - 8;
        }

        if (is_promotion_square(color, to)) {
            uint8_t promoted = promotion_piece_type(promo);
            next.bitboards[offset + PIECE_PAWN] &= ~(1ULL << from);
            if (dst_has_piece) remove_piece_at_side(next, to, opp_offset);
            add_piece(next, color, promoted, to);
        } else {
            if (dst_has_piece) remove_piece_at_side(next, to, opp_offset);
            move_piece(next, piece_index, from, to);
        }
    } else if (piece_type == PIECE_KING) {
        next.enPassantSquare = NO_EP;
        if (is_castle_move(color, from, to)) {
            apply_castle(next, color, from, to);
        } else {
            if (dst_has_piece) remove_piece_at_side(next, to, opp_offset);
            move_piece(next, piece_index, from, to);
        }
        next.castlingRights = clear_castling_for_king(next.castlingRights, color);
    } else {
        next.enPassantSquare = NO_EP;
        if (dst_has_piece) remove_piece_at_side(next, to, opp_offset);
        move_piece(next, piece_index, from, to);
        if (piece_type == PIECE_ROOK) {
            next.castlingRights = clear_castling_for_rook_move(next.castlingRights, color, from);
        }
    }

    if (dst_has_piece) {
        next.castlingRights = clear_castling_for_rook_capture(next.castlingRights, to);
    }

    if (piece_type == PIECE_PAWN || is_capture) {
        next.halfMoveClock = 0;
    } else {
        next.halfMoveClock++;
    }

    if (next.sideToMove == COLOR_BLACK) {
        next.fullMoveNumber++;
    }

    next.sideToMove ^= 1;
    return next;
}

// ============================================================
// Legal ply validation
// ============================================================

bool is_legal_ply(const GameState& state, uint32_t ply) {
    if (!is_pseudo_legal_ply(state, ply)) return false;
    uint8_t mover = state.sideToMove;
    GameState next = apply_ply_to_memory(state, ply);
    return !in_check(next, mover);
}

GameState apply_ply_memory(const GameState& state, uint32_t ply) {
    if (!is_pseudo_legal_ply(state, ply))
        throw std::runtime_error("Illegal ply: not pseudo-legal");
    uint8_t mover = state.sideToMove;
    GameState next = apply_ply_to_memory(state, ply);
    if (in_check(next, mover))
        throw std::runtime_error("Illegal ply: leaves king in check");
    return next;
}

// ============================================================
// Legal ply existence + check status
// ============================================================

LegalPlyResult has_any_legal_ply_with_check(const GameState& state) {
    uint8_t side = state.sideToMove;
    uint64_t own_occ = side_occupancy(state, side);
    uint64_t opp_occ = side_occupancy(state, side ^ 1);
    uint64_t occ = own_occ | opp_occ;
    uint8_t offset = (side == COLOR_WHITE) ? WHITE_OFFSET : BLACK_OFFSET;

    bool is_in_check = in_check(state, side);

    // Try all pieces — generate pseudo-legal moves and test for legality
    for (int pt = 0; pt < 6; pt++) {
        uint64_t pieces = state.bitboards[offset + pt];
        while (pieces) {
            uint8_t from = pop_lsb(pieces);
            uint64_t targets = 0;

            switch (pt) {
                case PIECE_PAWN: {
                    // Forward pushes
                    if (side == COLOR_WHITE) {
                        if (from < 56) {
                            uint8_t fwd = from + 8;
                            if (!(occ & (1ULL << fwd))) {
                                targets |= 1ULL << fwd;
                                if ((from >> 3) == 1 && !(occ & (1ULL << (from + 16))))
                                    targets |= 1ULL << (from + 16);
                            }
                            if ((from & 7) > 0) targets |= 1ULL << (from + 7);
                            if ((from & 7) < 7) targets |= 1ULL << (from + 9);
                        }
                    } else {
                        if (from >= 8) {
                            uint8_t fwd = from - 8;
                            if (!(occ & (1ULL << fwd))) {
                                targets |= 1ULL << fwd;
                                if ((from >> 3) == 6 && !(occ & (1ULL << (from - 16))))
                                    targets |= 1ULL << (from - 16);
                            }
                            if ((from & 7) > 0) targets |= 1ULL << (from - 9);
                            if ((from & 7) < 7) targets |= 1ULL << (from - 7);
                        }
                    }
                    targets &= ~own_occ;
                    break;
                }
                case PIECE_KNIGHT:
                    targets = knight_attacks(from) & ~own_occ;
                    break;
                case PIECE_BISHOP:
                    targets = bishop_attacks(from, occ, own_occ);
                    break;
                case PIECE_ROOK:
                    targets = rook_attacks(from, occ, own_occ);
                    break;
                case PIECE_QUEEN:
                    targets = bishop_attacks(from, occ, own_occ) | rook_attacks(from, occ, own_occ);
                    break;
                case PIECE_KING: {
                    targets = king_attacks(from) & ~own_occ;
                    // Castling
                    if (side == COLOR_WHITE) {
                        if (state.castlingRights & 0x01) targets |= 1ULL << 6;
                        if (state.castlingRights & 0x02) targets |= 1ULL << 2;
                    } else {
                        if (state.castlingRights & 0x04) targets |= 1ULL << 62;
                        if (state.castlingRights & 0x08) targets |= 1ULL << 58;
                    }
                    break;
                }
            }

            while (targets) {
                uint8_t to = pop_lsb(targets);
                // For pawns reaching promotion, test with queen promo
                uint8_t promo = 0;
                if (pt == PIECE_PAWN && is_promotion_square(side, to)) promo = 4;
                uint32_t ply = encode_ply(from, to, promo);
                if (is_legal_ply(state, ply)) {
                    return {true, is_in_check};
                }
            }
        }
    }

    return {false, is_in_check};
}

bool has_any_legal_ply(const GameState& state) {
    return has_any_legal_ply_with_check(state).hasLegal;
}

// ============================================================
// Insufficient material detection
// ============================================================

static uint8_t bishop_color_mask(uint64_t bb) {
    uint8_t mask = 0;
    while (bb) {
        uint8_t sq = pop_lsb(bb);
        uint8_t file = sq & 7;
        uint8_t rank = sq >> 3;
        uint8_t color_bit = (((file + rank) & 1) == 0) ? 1 : 2;
        mask |= color_bit;
        if (mask == 3) return mask;
    }
    return mask;
}

static uint8_t bishop_index_parity_mask(uint64_t bb) {
    uint8_t mask = 0;
    while (bb) {
        uint8_t sq = pop_lsb(bb);
        uint8_t color_bit = ((sq & 1) == 0) ? 1 : 2;
        mask |= color_bit;
        if (mask == 3) return mask;
    }
    return mask;
}

bool is_insufficient_material(const GameState& state) {
    if (state.bitboards[WHITE_OFFSET + PIECE_PAWN] |
        state.bitboards[WHITE_OFFSET + PIECE_ROOK] |
        state.bitboards[WHITE_OFFSET + PIECE_QUEEN] |
        state.bitboards[BLACK_OFFSET + PIECE_PAWN] |
        state.bitboards[BLACK_OFFSET + PIECE_ROOK] |
        state.bitboards[BLACK_OFFSET + PIECE_QUEEN]) {
        return false;
    }

    uint8_t wN = popcount(state.bitboards[WHITE_OFFSET + PIECE_KNIGHT]);
    uint8_t bN = popcount(state.bitboards[BLACK_OFFSET + PIECE_KNIGHT]);
    uint8_t wB = popcount(state.bitboards[WHITE_OFFSET + PIECE_BISHOP]);
    uint8_t bB = popcount(state.bitboards[BLACK_OFFSET + PIECE_BISHOP]);
    uint8_t total = wN + bN + wB + bB;

    if (total == 0) return true;
    if (total == 1) return true;

    if (total == 2 && wB == 0 && bB == 0) {
        if ((wN == 2 && bN == 0) || (bN == 2 && wN == 0)) return true;
    }

    if (wN == 0 && bN == 0 && total == 2 && wB == 1 && bB == 1) {
        uint8_t wc = bishop_color_mask(state.bitboards[WHITE_OFFSET + PIECE_BISHOP]);
        uint8_t bc = bishop_color_mask(state.bitboards[BLACK_OFFSET + PIECE_BISHOP]);
        if (wc == bc) return true;
    }

    if (total == 2 && bN == 0 && wN == 0) {
        if (wB == 2 &&
            bishop_color_mask(state.bitboards[WHITE_OFFSET + PIECE_BISHOP]) < 3 &&
            bishop_index_parity_mask(state.bitboards[WHITE_OFFSET + PIECE_BISHOP]) < 3) {
            return true;
        }
        if (bB == 2 &&
            bishop_color_mask(state.bitboards[BLACK_OFFSET + PIECE_BISHOP]) < 3 &&
            bishop_index_parity_mask(state.bitboards[BLACK_OFFSET + PIECE_BISHOP]) < 3) {
            return true;
        }
    }

    return false;
}

// ============================================================
// Position validation
// ============================================================

bool validate_position(const GameState& state) {
    // 1. Exactly one king per side
    if (popcount(state.bitboards[WHITE_OFFSET + PIECE_KING]) != 1) return false;
    if (popcount(state.bitboards[BLACK_OFFSET + PIECE_KING]) != 1) return false;

    // 2. No pawns on rank 1 or rank 8
    if ((state.bitboards[WHITE_OFFSET + PIECE_PAWN] | state.bitboards[BLACK_OFFSET + PIECE_PAWN]) & 0xFF000000000000FFULL)
        return false;

    // 3. Max 8 pawns per side
    if (popcount(state.bitboards[WHITE_OFFSET + PIECE_PAWN]) > 8) return false;
    if (popcount(state.bitboards[BLACK_OFFSET + PIECE_PAWN]) > 8) return false;

    // 4. Max 16 pieces per side
    uint64_t white_occ = 0, black_occ = 0;
    uint8_t wc = 0, bc = 0;
    for (int i = 0; i < 6; i++) {
        white_occ |= state.bitboards[WHITE_OFFSET + i];
        black_occ |= state.bitboards[BLACK_OFFSET + i];
        wc += popcount(state.bitboards[WHITE_OFFSET + i]);
        bc += popcount(state.bitboards[BLACK_OFFSET + i]);
    }
    if (wc > 16 || bc > 16) return false;

    // 5. No overlapping
    if (white_occ & black_occ) return false;
    {
        uint64_t wcheck = 0, bcheck = 0;
        for (int i = 0; i < 6; i++) {
            if (wcheck & state.bitboards[WHITE_OFFSET + i]) return false;
            if (bcheck & state.bitboards[BLACK_OFFSET + i]) return false;
            wcheck |= state.bitboards[WHITE_OFFSET + i];
            bcheck |= state.bitboards[BLACK_OFFSET + i];
        }
    }

    // 6. Side not-to-move not in check
    if (state.sideToMove > 1) return false;
    uint8_t opp = (state.sideToMove == COLOR_WHITE) ? COLOR_BLACK : COLOR_WHITE;
    if (in_check(state, opp)) return false;

    // 7. Castling rights consistency
    uint8_t cr = state.castlingRights;
    if (cr & 0x01) {
        if (!(state.bitboards[WHITE_OFFSET + PIECE_KING] & (1ULL << 4)) ||
            !(state.bitboards[WHITE_OFFSET + PIECE_ROOK] & (1ULL << 7))) return false;
    }
    if (cr & 0x02) {
        if (!(state.bitboards[WHITE_OFFSET + PIECE_KING] & (1ULL << 4)) ||
            !(state.bitboards[WHITE_OFFSET + PIECE_ROOK] & (1ULL << 0))) return false;
    }
    if (cr & 0x04) {
        if (!(state.bitboards[BLACK_OFFSET + PIECE_KING] & (1ULL << 60)) ||
            !(state.bitboards[BLACK_OFFSET + PIECE_ROOK] & (1ULL << 63))) return false;
    }
    if (cr & 0x08) {
        if (!(state.bitboards[BLACK_OFFSET + PIECE_KING] & (1ULL << 60)) ||
            !(state.bitboards[BLACK_OFFSET + PIECE_ROOK] & (1ULL << 56))) return false;
    }

    // 8. En passant validity
    if (state.enPassantSquare != NO_EP) {
        uint8_t ep = state.enPassantSquare;
        if (ep >= 64) return false;
        uint8_t ep_rank = ep / 8;
        if (state.sideToMove == COLOR_WHITE) {
            if (ep_rank != 5) return false;
            if (!(state.bitboards[BLACK_OFFSET + PIECE_PAWN] & (1ULL << (ep - 8)))) return false;
        } else {
            if (ep_rank != 2) return false;
            if (!(state.bitboards[WHITE_OFFSET + PIECE_PAWN] & (1ULL << (ep + 8)))) return false;
        }
    }

    // 9. Full move number >= 1
    if (state.fullMoveNumber < 1) return false;

    return true;
}

// ============================================================
// Full legal move generation
// ============================================================

std::vector<uint32_t> generate_legal_moves(const GameState& state) {
    std::vector<uint32_t> moves;
    moves.reserve(256);

    uint8_t side = state.sideToMove;
    uint64_t own_occ = side_occupancy(state, side);
    uint64_t opp_occ = side_occupancy(state, side ^ 1);
    uint64_t occ = own_occ | opp_occ;
    uint8_t offset = (side == COLOR_WHITE) ? WHITE_OFFSET : BLACK_OFFSET;

    for (int pt = 0; pt < 6; pt++) {
        uint64_t pieces = state.bitboards[offset + pt];
        while (pieces) {
            uint8_t from = pop_lsb(pieces);
            uint64_t targets = 0;

            switch (pt) {
                case PIECE_PAWN: {
                    if (side == COLOR_WHITE) {
                        if (from < 56) {
                            uint8_t fwd = from + 8;
                            if (!(occ & (1ULL << fwd))) {
                                targets |= 1ULL << fwd;
                                if ((from >> 3) == 1 && !(occ & (1ULL << (from + 16))))
                                    targets |= 1ULL << (from + 16);
                            }
                            if ((from & 7) > 0) targets |= 1ULL << (from + 7);
                            if ((from & 7) < 7) targets |= 1ULL << (from + 9);
                        }
                    } else {
                        if (from >= 8) {
                            uint8_t fwd = from - 8;
                            if (!(occ & (1ULL << fwd))) {
                                targets |= 1ULL << fwd;
                                if ((from >> 3) == 6 && !(occ & (1ULL << (from - 16))))
                                    targets |= 1ULL << (from - 16);
                            }
                            if ((from & 7) > 0) targets |= 1ULL << (from - 9);
                            if ((from & 7) < 7) targets |= 1ULL << (from - 7);
                        }
                    }
                    targets &= ~own_occ;
                    break;
                }
                case PIECE_KNIGHT:
                    targets = knight_attacks(from) & ~own_occ;
                    break;
                case PIECE_BISHOP:
                    targets = bishop_attacks(from, occ, own_occ);
                    break;
                case PIECE_ROOK:
                    targets = rook_attacks(from, occ, own_occ);
                    break;
                case PIECE_QUEEN:
                    targets = bishop_attacks(from, occ, own_occ) |
                              rook_attacks(from, occ, own_occ);
                    break;
                case PIECE_KING: {
                    targets = king_attacks(from) & ~own_occ;
                    if (side == COLOR_WHITE) {
                        if (state.castlingRights & 0x01) targets |= 1ULL << 6;
                        if (state.castlingRights & 0x02) targets |= 1ULL << 2;
                    } else {
                        if (state.castlingRights & 0x04) targets |= 1ULL << 62;
                        if (state.castlingRights & 0x08) targets |= 1ULL << 58;
                    }
                    break;
                }
            }

            while (targets) {
                uint8_t to = pop_lsb(targets);
                if (pt == PIECE_PAWN && is_promotion_square(side, to)) {
                    // Generate all 4 promotion types
                    for (uint8_t p = 1; p <= 4; p++) {
                        uint32_t ply = encode_ply(from, to, p);
                        if (is_legal_ply(state, ply))
                            moves.push_back(ply);
                    }
                } else {
                    uint32_t ply = encode_ply(from, to, 0);
                    if (is_legal_ply(state, ply))
                        moves.push_back(ply);
                }
            }
        }
    }
    return moves;
}

// ============================================================
// FEN parsing / output
// ============================================================

GameState parse_fen(const std::string& fen) {
    GameState state;
    std::memset(&state, 0, sizeof(state));

    std::istringstream ss(fen);
    std::string pieces, turn, castling, ep, half_str, full_str;
    ss >> pieces >> turn >> castling >> ep;
    if (ss >> half_str) state.halfMoveClock = static_cast<uint16_t>(std::stoi(half_str));
    if (ss >> full_str) state.fullMoveNumber = static_cast<uint16_t>(std::stoi(full_str));
    else state.fullMoveNumber = 1;

    // Parse piece placement (rank 8 first in FEN = rank 7 in 0-indexed)
    int rank = 7, file = 0;
    for (char c : pieces) {
        if (c == '/') { rank--; file = 0; }
        else if (c >= '1' && c <= '8') { file += (c - '0'); }
        else {
            int s = rank * 8 + file;
            uint8_t co, pt;
            switch (c) {
                case 'P': co = WHITE_OFFSET; pt = PIECE_PAWN; break;
                case 'N': co = WHITE_OFFSET; pt = PIECE_KNIGHT; break;
                case 'B': co = WHITE_OFFSET; pt = PIECE_BISHOP; break;
                case 'R': co = WHITE_OFFSET; pt = PIECE_ROOK; break;
                case 'Q': co = WHITE_OFFSET; pt = PIECE_QUEEN; break;
                case 'K': co = WHITE_OFFSET; pt = PIECE_KING; break;
                case 'p': co = BLACK_OFFSET; pt = PIECE_PAWN; break;
                case 'n': co = BLACK_OFFSET; pt = PIECE_KNIGHT; break;
                case 'b': co = BLACK_OFFSET; pt = PIECE_BISHOP; break;
                case 'r': co = BLACK_OFFSET; pt = PIECE_ROOK; break;
                case 'q': co = BLACK_OFFSET; pt = PIECE_QUEEN; break;
                case 'k': co = BLACK_OFFSET; pt = PIECE_KING; break;
                default: co = 0; pt = 0; break;
            }
            state.bitboards[co + pt] |= 1ULL << s;
            file++;
        }
    }

    state.sideToMove = (turn == "b") ? COLOR_BLACK : COLOR_WHITE;

    state.castlingRights = 0;
    for (char c : castling) {
        switch (c) {
            case 'K': state.castlingRights |= 0x01; break;
            case 'Q': state.castlingRights |= 0x02; break;
            case 'k': state.castlingRights |= 0x04; break;
            case 'q': state.castlingRights |= 0x08; break;
        }
    }

    if (ep == "-") {
        state.enPassantSquare = NO_EP;
    } else {
        int ef = ep[0] - 'a';
        int er = ep[1] - '1';
        state.enPassantSquare = static_cast<uint8_t>(er * 8 + ef);
    }

    return state;
}

std::string state_to_fen(const GameState& state) {
    std::string fen;
    // Piece placement
    for (int rank = 7; rank >= 0; rank--) {
        int empty = 0;
        for (int file = 0; file < 8; file++) {
            int sq = rank * 8 + file;
            uint64_t bit = 1ULL << sq;
            char c = 0;
            if      (state.bitboards[WHITE_OFFSET + PIECE_PAWN]   & bit) c = 'P';
            else if (state.bitboards[WHITE_OFFSET + PIECE_KNIGHT] & bit) c = 'N';
            else if (state.bitboards[WHITE_OFFSET + PIECE_BISHOP] & bit) c = 'B';
            else if (state.bitboards[WHITE_OFFSET + PIECE_ROOK]   & bit) c = 'R';
            else if (state.bitboards[WHITE_OFFSET + PIECE_QUEEN]  & bit) c = 'Q';
            else if (state.bitboards[WHITE_OFFSET + PIECE_KING]   & bit) c = 'K';
            else if (state.bitboards[BLACK_OFFSET + PIECE_PAWN]   & bit) c = 'p';
            else if (state.bitboards[BLACK_OFFSET + PIECE_KNIGHT] & bit) c = 'n';
            else if (state.bitboards[BLACK_OFFSET + PIECE_BISHOP] & bit) c = 'b';
            else if (state.bitboards[BLACK_OFFSET + PIECE_ROOK]   & bit) c = 'r';
            else if (state.bitboards[BLACK_OFFSET + PIECE_QUEEN]  & bit) c = 'q';
            else if (state.bitboards[BLACK_OFFSET + PIECE_KING]   & bit) c = 'k';

            if (c) {
                if (empty) { fen += std::to_string(empty); empty = 0; }
                fen += c;
            } else {
                empty++;
            }
        }
        if (empty) fen += std::to_string(empty);
        if (rank > 0) fen += '/';
    }

    fen += (state.sideToMove == COLOR_WHITE) ? " w " : " b ";

    // Castling
    std::string cast;
    if (state.castlingRights & 0x01) cast += 'K';
    if (state.castlingRights & 0x02) cast += 'Q';
    if (state.castlingRights & 0x04) cast += 'k';
    if (state.castlingRights & 0x08) cast += 'q';
    fen += cast.empty() ? "-" : cast;

    // EP
    if (state.enPassantSquare == NO_EP) {
        fen += " -";
    } else {
        fen += ' ';
        fen += static_cast<char>('a' + (state.enPassantSquare & 7));
        fen += static_cast<char>('1' + (state.enPassantSquare >> 3));
    }

    fen += " " + std::to_string(state.halfMoveClock);
    fen += " " + std::to_string(state.fullMoveNumber);
    return fen;
}

// ============================================================
// UCI move conversion
// ============================================================

uint32_t uci_to_ply(const GameState& state, const std::string& uci) {
    if (uci.size() < 4) return 0;
    uint8_t from_f = uci[0] - 'a';
    uint8_t from_r = uci[1] - '1';
    uint8_t to_f   = uci[2] - 'a';
    uint8_t to_r   = uci[3] - '1';
    uint8_t from = from_r * 8 + from_f;
    uint8_t to   = to_r * 8 + to_f;
    uint8_t promo = 0;
    if (uci.size() >= 5) {
        switch (uci[4]) {
            case 'n': promo = 1; break;
            case 'b': promo = 2; break;
            case 'r': promo = 3; break;
            case 'q': promo = 4; break;
        }
    }
    return encode_ply(from, to, promo);
}

std::string ply_to_uci(uint32_t ply) {
    uint8_t from, to, promo;
    decode_ply(ply, from, to, promo);
    std::string s;
    s += static_cast<char>('a' + (from & 7));
    s += static_cast<char>('1' + (from >> 3));
    s += static_cast<char>('a' + (to & 7));
    s += static_cast<char>('1' + (to >> 3));
    if (promo) {
        const char promo_chars[] = {0, 'n', 'b', 'r', 'q'};
        s += promo_chars[promo];
    }
    return s;
}

// ============================================================
// Position hashing (for repetition detection)
// ============================================================

uint64_t position_hash(const GameState& state) {
    // Simple hash combining bitboards, castling, ep, side
    // Uses FNV-1a rather than the earlier keccak-based prototype hash
    uint64_t h = 14695981039346656037ULL; // FNV offset basis
    for (int i = 0; i < 12; i++) {
        h ^= state.bitboards[i];
        h *= 1099511628211ULL; // FNV prime
    }
    h ^= static_cast<uint64_t>(state.sideToMove);
    h *= 1099511628211ULL;
    h ^= static_cast<uint64_t>(state.castlingRights);
    h *= 1099511628211ULL;
    h ^= static_cast<uint64_t>(state.enPassantSquare);
    h *= 1099511628211ULL;
    return h;
}

} // namespace chess
