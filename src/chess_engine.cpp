// chess_engine.cpp — Bitboard chess engine implementation.
// The move generator uses a native bitboard model maintained for the R package.

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
uint64_t PAWN_ATTACKS[2][64];

static bool tables_initialized = false;
static void init_magic_tables();

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
    // Precompute pawn attack masks.
    constexpr uint64_t notA = 0xFEFEFEFEFEFEFEFEULL;
    constexpr uint64_t notH = 0x7F7F7F7F7F7F7F7FULL;
    for (int sq = 0; sq < 64; sq++) {
        uint64_t bb = 1ULL << sq;
        // White pawns attack NW and NE
        PAWN_ATTACKS[COLOR_WHITE][sq] = ((bb << 7) & notH) | ((bb << 9) & notA);
        // Black pawns attack SW and SE
        PAWN_ATTACKS[COLOR_BLACK][sq] = ((bb >> 9) & notH) | ((bb >> 7) & notA);
    }
    tables_initialized = true;
    init_magic_tables();
}

// Auto-initialize on load
namespace { struct Init { Init() { init_attack_tables(); init_zobrist(); } } _init; }

// ============================================================
// Zobrist hashing
// ============================================================

static uint64_t ZOBRIST_PIECE[12][64];
static uint64_t ZOBRIST_SIDE;
static uint64_t ZOBRIST_CASTLING[16];
static uint64_t ZOBRIST_EP[65]; // 0-63 squares + 64 for NO_EP

static uint64_t zobrist_xorshift(uint64_t& s) {
    s ^= s << 13; s ^= s >> 7; s ^= s << 17; return s;
}

void init_zobrist() {
    uint64_t seed = 0x12345678ABCDEF01ULL;
    for (int p = 0; p < 12; p++)
        for (int sq = 0; sq < 64; sq++)
            ZOBRIST_PIECE[p][sq] = zobrist_xorshift(seed);
    ZOBRIST_SIDE = zobrist_xorshift(seed);
    for (int i = 0; i < 16; i++)
        ZOBRIST_CASTLING[i] = zobrist_xorshift(seed);
    for (int i = 0; i < 65; i++)
        ZOBRIST_EP[i] = zobrist_xorshift(seed);
}

uint64_t compute_zobrist(const GameState& state) {
    uint64_t h = 0;
    for (int p = 0; p < 12; p++) {
        uint64_t bb = state.bitboards[p];
        while (bb) { h ^= ZOBRIST_PIECE[p][pop_lsb(bb)]; }
    }
    if (state.sideToMove == COLOR_BLACK) h ^= ZOBRIST_SIDE;
    h ^= ZOBRIST_CASTLING[state.castlingRights & 0x0F];
    h ^= ZOBRIST_EP[state.enPassantSquare == NO_EP ? 64 : state.enPassantSquare];
    return h;
}

// ============================================================
// Attack generation
// ============================================================

uint64_t knight_attacks(uint8_t sq) {
    return KNIGHT_ATTACKS[sq];
}

uint64_t king_attacks(uint8_t sq) {
    return KING_ATTACKS[sq];
}

// Sliding ray attacks (loop-based, used for magic table initialization)
uint64_t ray_attacks_slow(uint8_t sq, int8_t df, int8_t dr, uint64_t occ, uint64_t own_occ) {
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

// Helper: compute rook attacks using ray_attacks_slow (no own_occ exclusion)
static uint64_t rook_attacks_slow(uint8_t sq, uint64_t occ) {
    return ray_attacks_slow(sq, 0, 1, occ, 0) |
           ray_attacks_slow(sq, 0, -1, occ, 0) |
           ray_attacks_slow(sq, 1, 0, occ, 0) |
           ray_attacks_slow(sq, -1, 0, occ, 0);
}

static uint64_t bishop_attacks_slow(uint8_t sq, uint64_t occ) {
    return ray_attacks_slow(sq, 1, 1, occ, 0) |
           ray_attacks_slow(sq, -1, 1, occ, 0) |
           ray_attacks_slow(sq, 1, -1, occ, 0) |
           ray_attacks_slow(sq, -1, -1, occ, 0);
}

// ============================================================
// Magic Bitboards
// ============================================================

struct MagicEntry {
    uint64_t mask;
    uint64_t magic;
    int shift;
    uint64_t* table;
};

static MagicEntry ROOK_MAGICS[64];
static MagicEntry BISHOP_MAGICS[64];

// Max table sizes: rook up to 4096 entries per square, bishop up to 512
static uint64_t ROOK_TABLE[64 * 4096];
static uint64_t BISHOP_TABLE[64 * 512];

static uint64_t rook_mask(int sq) {
    uint64_t mask = 0;
    int r = sq / 8, f = sq % 8;
    for (int i = r + 1; i < 7; i++) mask |= 1ULL << (i * 8 + f);
    for (int i = r - 1; i > 0; i--) mask |= 1ULL << (i * 8 + f);
    for (int i = f + 1; i < 7; i++) mask |= 1ULL << (r * 8 + i);
    for (int i = f - 1; i > 0; i--) mask |= 1ULL << (r * 8 + i);
    return mask;
}

static uint64_t bishop_mask(int sq) {
    uint64_t mask = 0;
    int r = sq / 8, f = sq % 8;
    for (int dr = -1; dr <= 1; dr += 2) {
        for (int df = -1; df <= 1; df += 2) {
            int rr = r + dr, ff = f + df;
            while (rr > 0 && rr < 7 && ff > 0 && ff < 7) {
                mask |= 1ULL << (rr * 8 + ff);
                rr += dr; ff += df;
            }
        }
    }
    return mask;
}

// Enumerate subsets of mask using Carry-Rippler
static uint64_t next_subset(uint64_t subset, uint64_t mask) {
    return (subset - mask) & mask;
}

// Simple PRNG for magic number search (fixed seed for determinism)
static uint64_t magic_rand_state = 0x12345678ABCDEF01ULL;
static uint64_t magic_rand64() {
    // xorshift64
    magic_rand_state ^= magic_rand_state << 13;
    magic_rand_state ^= magic_rand_state >> 7;
    magic_rand_state ^= magic_rand_state << 17;
    return magic_rand_state;
}

static uint64_t sparse_rand64() {
    return magic_rand64() & magic_rand64() & magic_rand64();
}

static void init_magic_for_square(int sq, bool is_rook) {
    uint64_t mask = is_rook ? rook_mask(sq) : bishop_mask(sq);
    int bits = popcount(mask);
    int table_size = 1 << bits;
    int shift = 64 - bits;

    // Enumerate all subsets and compute attacks
    std::vector<uint64_t> occupancies(table_size);
    std::vector<uint64_t> attacks(table_size);

    uint64_t subset = 0;
    for (int i = 0; i < table_size; i++) {
        occupancies[i] = subset;
        attacks[i] = is_rook ? rook_attacks_slow(static_cast<uint8_t>(sq), subset)
                             : bishop_attacks_slow(static_cast<uint8_t>(sq), subset);
        subset = next_subset(subset, mask);
    }

    // Get table pointer
    uint64_t* table = is_rook ? &ROOK_TABLE[sq * 4096] : &BISHOP_TABLE[sq * 512];

    // Find magic number
    while (true) {
        uint64_t magic = sparse_rand64();

        // Quick rejection: check if high bits have enough set bits
        if (popcount((mask * magic) & 0xFF00000000000000ULL) < 6) continue;

        // Clear table
        std::memset(table, 0xFF, sizeof(uint64_t) * table_size);

        bool collision = false;
        for (int i = 0; i < table_size; i++) {
            uint64_t idx = (occupancies[i] * magic) >> shift;
            if (table[idx] == 0xFFFFFFFFFFFFFFFFULL) {
                table[idx] = attacks[i];
            } else if (table[idx] != attacks[i]) {
                collision = true;
                break;
            }
        }

        if (!collision) {
            MagicEntry& entry = is_rook ? ROOK_MAGICS[sq] : BISHOP_MAGICS[sq];
            entry.mask = mask;
            entry.magic = magic;
            entry.shift = shift;
            entry.table = table;
            return;
        }
    }
}

static bool magic_tables_initialized = false;

static void init_magic_tables() {
    if (magic_tables_initialized) return;
    magic_rand_state = 0x12345678ABCDEF01ULL; // reset seed for determinism
    for (int sq = 0; sq < 64; sq++) {
        init_magic_for_square(sq, true);   // rook
        init_magic_for_square(sq, false);  // bishop
    }
    magic_tables_initialized = true;
}

uint64_t rook_attacks(uint8_t sq, uint64_t occ) {
    const MagicEntry& e = ROOK_MAGICS[sq];
    return e.table[((occ & e.mask) * e.magic) >> e.shift];
}

uint64_t bishop_attacks(uint8_t sq, uint64_t occ) {
    const MagicEntry& e = BISHOP_MAGICS[sq];
    return e.table[((occ & e.mask) * e.magic) >> e.shift];
}

// ============================================================
// Init starting position
// ============================================================

static void rebuild_mailbox(GameState& state) {
    std::memset(state.mailbox, NO_PIECE, 64);
    for (int p = 0; p < 12; p++) {
        uint64_t bb = state.bitboards[p];
        while (bb) {
            uint8_t sq = pop_lsb(bb);
            state.mailbox[sq] = static_cast<uint8_t>(p);
        }
    }
}

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
    rebuild_mailbox(state);
    state.hash = compute_zobrist(state);
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

    // Pawn attacks (reverse lookup using precomputed table)
    // If white attacks square, a white pawn must be on a square that attacks it.
    // That's the same as: from the square, black's pawn attacks hit a white pawn.
    if (PAWN_ATTACKS[attacker_color ^ 1][square] & state.bitboards[off + PIECE_PAWN]) return true;

    // Knight
    if (knight_attacks(square) & state.bitboards[off + PIECE_KNIGHT]) return true;

    // King
    if (king_attacks(square) & state.bitboards[off + PIECE_KING]) return true;

    // Diagonal sliders (bishop + queen)
    uint64_t diag = state.bitboards[off + PIECE_BISHOP] | state.bitboards[off + PIECE_QUEEN];
    if (diag && (bishop_attacks(square, occ) & diag)) return true;

    // Orthogonal sliders (rook + queen)
    uint64_t ortho = state.bitboards[off + PIECE_ROOK] | state.bitboards[off + PIECE_QUEEN];
    if (ortho && (rook_attacks(square, occ) & ortho)) return true;

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
    state.mailbox[to] = piece_index;
    state.mailbox[from] = NO_PIECE;
}

static void add_piece(GameState& state, uint8_t color, uint8_t piece_type, uint8_t square) {
    uint8_t idx = (color == COLOR_WHITE) ? WHITE_OFFSET + piece_type : BLACK_OFFSET + piece_type;
    state.bitboards[idx] |= (1ULL << square);
    state.mailbox[square] = idx;
}

static void remove_piece_at_side(GameState& state, uint8_t square, uint8_t /*side_offset*/) {
    uint8_t pi = state.mailbox[square];
    if (pi != NO_PIECE) {
        state.bitboards[pi] &= ~(1ULL << square);
        state.mailbox[square] = NO_PIECE;
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

    // Hash: XOR out old castling and EP (will XOR in new values at end)
    uint64_t h = next.hash;
    h ^= ZOBRIST_CASTLING[next.castlingRights & 0x0F];
    h ^= ZOBRIST_EP[next.enPassantSquare == NO_EP ? 64 : next.enPassantSquare];

    // Find mover via mailbox (O(1))
    uint8_t piece_index = next.mailbox[from];
    if (piece_index == NO_PIECE) return next;
    uint8_t piece_type = piece_index - offset;

    // Check dest for capture via mailbox (O(1))
    uint8_t captured_index = next.mailbox[to];
    bool dst_has_piece = (captured_index != NO_PIECE);
    bool is_capture = dst_has_piece;
    uint8_t captured_type = dst_has_piece ? (captured_index - opp_offset) : 255;

    if (piece_type == PIECE_PAWN) {
        bool is_ep = is_en_passant_capture(next, color, from, to);
        next.enPassantSquare = NO_EP;

        if (is_ep) {
            uint8_t captured_sq = (color == COLOR_WHITE) ? to - 8 : to + 8;
            next.bitboards[opp_offset + PIECE_PAWN] &= ~(1ULL << captured_sq);
            next.mailbox[captured_sq] = NO_PIECE;
            h ^= ZOBRIST_PIECE[opp_offset + PIECE_PAWN][captured_sq];
            is_capture = true;
        }

        if (is_double_pawn_push(color, from, to)) {
            next.enPassantSquare = (color == COLOR_WHITE) ? from + 8 : from - 8;
        }

        if (is_promotion_square(color, to)) {
            uint8_t promoted = promotion_piece_type(promo);
            next.bitboards[offset + PIECE_PAWN] &= ~(1ULL << from);
            if (dst_has_piece) {
                remove_piece_at_side(next, to, opp_offset);
                h ^= ZOBRIST_PIECE[opp_offset + captured_type][to];
            }
            add_piece(next, color, promoted, to);
            h ^= ZOBRIST_PIECE[offset + PIECE_PAWN][from];
            h ^= ZOBRIST_PIECE[offset + promoted][to];
        } else {
            if (dst_has_piece) {
                remove_piece_at_side(next, to, opp_offset);
                h ^= ZOBRIST_PIECE[opp_offset + captured_type][to];
            }
            move_piece(next, piece_index, from, to);
            h ^= ZOBRIST_PIECE[piece_index][from];
            h ^= ZOBRIST_PIECE[piece_index][to];
        }
    } else if (piece_type == PIECE_KING) {
        next.enPassantSquare = NO_EP;
        if (is_castle_move(color, from, to)) {
            apply_castle(next, color, from, to);
            h ^= ZOBRIST_PIECE[offset + PIECE_KING][from];
            h ^= ZOBRIST_PIECE[offset + PIECE_KING][to];
            uint8_t rf, rt;
            if (color == COLOR_WHITE && to == 6)       { rf = 7; rt = 5; }
            else if (color == COLOR_WHITE && to == 2)  { rf = 0; rt = 3; }
            else if (color == COLOR_BLACK && to == 62) { rf = 63; rt = 61; }
            else                                       { rf = 56; rt = 59; }
            h ^= ZOBRIST_PIECE[offset + PIECE_ROOK][rf];
            h ^= ZOBRIST_PIECE[offset + PIECE_ROOK][rt];
        } else {
            if (dst_has_piece) {
                remove_piece_at_side(next, to, opp_offset);
                h ^= ZOBRIST_PIECE[opp_offset + captured_type][to];
            }
            move_piece(next, piece_index, from, to);
            h ^= ZOBRIST_PIECE[offset + PIECE_KING][from];
            h ^= ZOBRIST_PIECE[offset + PIECE_KING][to];
        }
        next.castlingRights = clear_castling_for_king(next.castlingRights, color);
    } else {
        next.enPassantSquare = NO_EP;
        if (dst_has_piece) {
            remove_piece_at_side(next, to, opp_offset);
            h ^= ZOBRIST_PIECE[opp_offset + captured_type][to];
        }
        move_piece(next, piece_index, from, to);
        h ^= ZOBRIST_PIECE[piece_index][from];
        h ^= ZOBRIST_PIECE[piece_index][to];
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

    // Hash: XOR in new castling, EP, and side change
    h ^= ZOBRIST_SIDE;
    h ^= ZOBRIST_CASTLING[next.castlingRights & 0x0F];
    h ^= ZOBRIST_EP[next.enPassantSquare == NO_EP ? 64 : next.enPassantSquare];
    next.hash = h;

    return next;
}

// ============================================================
// Legal ply validation
// ============================================================

// Skip pseudo-legal check for moves we already know are pseudo-legal
// (move generation already ensures piece exists, target is valid, etc.)
// But we still need to validate castling and check that king isn't left in check.
static bool is_legal_generated(const GameState& state, uint32_t ply, bool is_castle = false) {
    if (is_castle) {
        uint8_t from, to, promo;
        decode_ply(ply, from, to, promo);
        uint64_t occ = side_occupancy(state, state.sideToMove) | side_occupancy(state, state.sideToMove ^ 1);
        if (!can_castle(state, state.sideToMove, from, to, occ)) return false;
    }
    GameState next = apply_ply_to_memory(state, ply);
    return !in_check(next, state.sideToMove);
}

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
                    targets = bishop_attacks(from, occ) & ~own_occ;
                    break;
                case PIECE_ROOK:
                    targets = rook_attacks(from, occ) & ~own_occ;
                    break;
                case PIECE_QUEEN:
                    targets = (bishop_attacks(from, occ) | rook_attacks(from, occ)) & ~own_occ;
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
// Pin-aware move generation helpers
// ============================================================

// Bitboard of squares strictly between two aligned squares
static uint64_t between_bb(uint8_t sq1, uint8_t sq2) {
    int r1 = sq1 / 8, f1 = sq1 % 8;
    int r2 = sq2 / 8, f2 = sq2 % 8;
    int dr = 0, df = 0;

    if (r1 == r2) {
        df = (f2 > f1) ? 1 : -1;
    } else if (f1 == f2) {
        dr = (r2 > r1) ? 1 : -1;
    } else if (abs(r2 - r1) == abs(f2 - f1)) {
        dr = (r2 > r1) ? 1 : -1;
        df = (f2 > f1) ? 1 : -1;
    } else {
        return 0; // not aligned
    }

    uint64_t bb = 0;
    int r = r1 + dr, f = f1 + df;
    while (r != r2 || f != f2) {
        bb |= 1ULL << (r * 8 + f);
        r += dr; f += df;
    }
    return bb;
}

struct MoveGenInfo {
    uint64_t checkers;     // enemy pieces giving check
    uint64_t pinned;       // our pieces pinned to our king
    uint64_t check_mask;   // valid destination mask for non-king moves
    uint64_t pin_ray[64];  // for each pinned square, the ray it must stay on
    uint64_t opp_attacks;  // all squares attacked by opponent (king removed from occ)
    uint8_t king_sq;
    int num_checkers;
};

static MoveGenInfo compute_movegen_info(const GameState& state) {
    MoveGenInfo info;
    std::memset(&info, 0, sizeof(info));

    uint8_t side = state.sideToMove;
    uint8_t opp = side ^ 1;
    uint8_t opp_off = (opp == COLOR_WHITE) ? WHITE_OFFSET : BLACK_OFFSET;

    info.king_sq = find_king_square(state, side);
    uint64_t king_bb = 1ULL << info.king_sq;

    uint64_t own_occ = side_occupancy(state, side);
    uint64_t opp_occ = side_occupancy(state, opp);
    uint64_t all_occ = own_occ | opp_occ;

    // --- Compute checkers ---
    info.checkers = 0;

    // Pawn checkers
    info.checkers |= PAWN_ATTACKS[side][info.king_sq] & state.bitboards[opp_off + PIECE_PAWN];

    // Knight checkers
    info.checkers |= KNIGHT_ATTACKS[info.king_sq] & state.bitboards[opp_off + PIECE_KNIGHT];

    // Bishop/Queen checkers (diagonal)
    uint64_t diag_sliders = state.bitboards[opp_off + PIECE_BISHOP] | state.bitboards[opp_off + PIECE_QUEEN];
    info.checkers |= bishop_attacks(info.king_sq, all_occ) & diag_sliders;

    // Rook/Queen checkers (orthogonal)
    uint64_t ortho_sliders = state.bitboards[opp_off + PIECE_ROOK] | state.bitboards[opp_off + PIECE_QUEEN];
    info.checkers |= rook_attacks(info.king_sq, all_occ) & ortho_sliders;

    info.num_checkers = popcount(info.checkers);

    // --- Compute check_mask ---
    if (info.num_checkers == 0) {
        info.check_mask = ~0ULL;
    } else if (info.num_checkers == 1) {
        uint8_t checker_sq = lsb_index(info.checkers);
        uint64_t checker_bb = 1ULL << checker_sq;
        // If the checker is a slider, include blocking squares
        if (checker_bb & (diag_sliders | ortho_sliders)) {
            info.check_mask = checker_bb | between_bb(info.king_sq, checker_sq);
        } else {
            // Knight or pawn: can only capture the checker
            info.check_mask = checker_bb;
        }
    } else {
        // Double check: only king moves
        info.check_mask = 0;
    }

    // --- Compute pins ---
    // For each direction from king, look for pinned pieces
    static const int8_t dirs[8][2] = {
        {0,1},{0,-1},{1,0},{-1,0},{1,1},{1,-1},{-1,1},{-1,-1}
    };

    for (int d = 0; d < 8; d++) {
        int8_t df = dirs[d][0], dr = dirs[d][1];
        // Determine which enemy sliders attack along this direction
        uint64_t attackers;
        if (d < 4) {
            // Orthogonal
            attackers = ortho_sliders;
        } else {
            // Diagonal
            attackers = diag_sliders;
        }
        if (!attackers) continue;

        int8_t r = info.king_sq / 8, f = info.king_sq % 8;
        uint8_t candidate = 255;
        bool found_own = false;

        r += dr; f += df;
        while (r >= 0 && r < 8 && f >= 0 && f < 8) {
            uint8_t sq = static_cast<uint8_t>(r * 8 + f);
            uint64_t sq_bb = 1ULL << sq;

            if (own_occ & sq_bb) {
                if (found_own) break; // second own piece, no pin
                candidate = sq;
                found_own = true;
            } else if (opp_occ & sq_bb) {
                if (found_own && (attackers & sq_bb)) {
                    // candidate is pinned by this attacker
                    info.pinned |= 1ULL << candidate;
                    info.pin_ray[candidate] = (1ULL << sq) | between_bb(info.king_sq, sq) | (1ULL << candidate);
                }
                break;
            }

            r += dr; f += df;
        }
    }

    // --- Compute opp_attacks (with our king removed from occupancy) ---
    uint64_t occ_no_king = all_occ & ~king_bb;

    info.opp_attacks = 0;

    // Opponent pawns
    uint64_t opp_pawns = state.bitboards[opp_off + PIECE_PAWN];
    if (opp == COLOR_WHITE) {
        constexpr uint64_t notA = 0xFEFEFEFEFEFEFEFEULL;
        constexpr uint64_t notH = 0x7F7F7F7F7F7F7F7FULL;
        info.opp_attacks |= ((opp_pawns << 7) & notH) | ((opp_pawns << 9) & notA);
    } else {
        constexpr uint64_t notA = 0xFEFEFEFEFEFEFEFEULL;
        constexpr uint64_t notH = 0x7F7F7F7F7F7F7F7FULL;
        info.opp_attacks |= ((opp_pawns >> 9) & notH) | ((opp_pawns >> 7) & notA);
    }

    // Opponent knights
    uint64_t opp_knights = state.bitboards[opp_off + PIECE_KNIGHT];
    while (opp_knights) {
        uint8_t sq = pop_lsb(opp_knights);
        info.opp_attacks |= KNIGHT_ATTACKS[sq];
    }

    // Opponent king
    uint8_t opp_king_sq = find_king_square(state, opp);
    info.opp_attacks |= KING_ATTACKS[opp_king_sq];

    // Opponent bishops + queen (diagonal) - with our king removed
    uint64_t opp_diag = state.bitboards[opp_off + PIECE_BISHOP] | state.bitboards[opp_off + PIECE_QUEEN];
    while (opp_diag) {
        uint8_t sq = pop_lsb(opp_diag);
        info.opp_attacks |= bishop_attacks(sq, occ_no_king);
    }

    // Opponent rooks + queen (orthogonal) - with our king removed
    uint64_t opp_ortho = state.bitboards[opp_off + PIECE_ROOK] | state.bitboards[opp_off + PIECE_QUEEN];
    while (opp_ortho) {
        uint8_t sq = pop_lsb(opp_ortho);
        info.opp_attacks |= rook_attacks(sq, occ_no_king);
    }

    return info;
}

// ============================================================
// Full legal move generation (pin-aware)
// ============================================================

static inline void add_pawn_moves(std::vector<uint32_t>& moves, uint8_t from, uint8_t to, uint8_t side) {
    if (is_promotion_square(side, to)) {
        for (uint8_t p = 1; p <= 4; p++)
            moves.push_back(encode_ply(from, to, p));
    } else {
        moves.push_back(encode_ply(from, to, 0));
    }
}

std::vector<uint32_t> generate_legal_moves(const GameState& state) {
    std::vector<uint32_t> moves;
    moves.reserve(256);

    uint8_t side = state.sideToMove;
    uint8_t opp = side ^ 1;
    uint8_t offset = (side == COLOR_WHITE) ? WHITE_OFFSET : BLACK_OFFSET;
    uint8_t opp_off = (opp == COLOR_WHITE) ? WHITE_OFFSET : BLACK_OFFSET;

    uint64_t own_occ = side_occupancy(state, side);
    uint64_t opp_occ = side_occupancy(state, opp);
    uint64_t all_occ = own_occ | opp_occ;

    MoveGenInfo info = compute_movegen_info(state);
    uint8_t king_sq = info.king_sq;

    // --- King moves ---
    {
        uint64_t king_targets = KING_ATTACKS[king_sq] & ~own_occ & ~info.opp_attacks;
        // Also can't capture opponent's king
        king_targets &= ~state.bitboards[opp_off + PIECE_KING];
        while (king_targets) {
            uint8_t to = pop_lsb(king_targets);
            moves.push_back(encode_ply(king_sq, to, 0));
        }

        // Castling (only when not in check)
        if (info.num_checkers == 0) {
            if (side == COLOR_WHITE) {
                if (state.castlingRights & 0x01) {
                    if (can_castle(state, side, king_sq, 6, all_occ))
                        moves.push_back(encode_ply(king_sq, 6, 0));
                }
                if (state.castlingRights & 0x02) {
                    if (can_castle(state, side, king_sq, 2, all_occ))
                        moves.push_back(encode_ply(king_sq, 2, 0));
                }
            } else {
                if (state.castlingRights & 0x04) {
                    if (can_castle(state, side, king_sq, 62, all_occ))
                        moves.push_back(encode_ply(king_sq, 62, 0));
                }
                if (state.castlingRights & 0x08) {
                    if (can_castle(state, side, king_sq, 58, all_occ))
                        moves.push_back(encode_ply(king_sq, 58, 0));
                }
            }
        }
    }

    // Double check: only king moves
    if (info.num_checkers >= 2) {
        return moves;
    }

    // --- Non-king pieces ---
    uint64_t check_mask = info.check_mask;
    uint64_t ep_bb = (state.enPassantSquare != NO_EP) ? (1ULL << state.enPassantSquare) : 0;

    // Pawns
    {
        uint64_t pawns = state.bitboards[offset + PIECE_PAWN];
        while (pawns) {
            uint8_t from = pop_lsb(pawns);
            bool is_pinned = (info.pinned & (1ULL << from)) != 0;
            uint64_t pin_mask = is_pinned ? info.pin_ray[from] : ~0ULL;

            // Push moves
            uint64_t push_targets = 0;
            if (side == COLOR_WHITE) {
                uint8_t fwd = from + 8;
                if (fwd < 64 && !(all_occ & (1ULL << fwd))) {
                    push_targets |= 1ULL << fwd;
                    if ((from >> 3) == 1 && !(all_occ & (1ULL << (from + 16))))
                        push_targets |= 1ULL << (from + 16);
                }
            } else {
                if (from >= 8) {
                    uint8_t fwd = from - 8;
                    if (!(all_occ & (1ULL << fwd))) {
                        push_targets |= 1ULL << fwd;
                        if ((from >> 3) == 6 && !(all_occ & (1ULL << (from - 16))))
                            push_targets |= 1ULL << (from - 16);
                    }
                }
            }
            push_targets &= check_mask & pin_mask;

            while (push_targets) {
                uint8_t to = pop_lsb(push_targets);
                add_pawn_moves(moves, from, to, side);
            }

            // Capture moves (excluding EP)
            uint64_t capture_targets = PAWN_ATTACKS[side][from] & opp_occ & ~state.bitboards[opp_off + PIECE_KING];
            capture_targets &= check_mask & pin_mask;

            while (capture_targets) {
                uint8_t to = pop_lsb(capture_targets);
                add_pawn_moves(moves, from, to, side);
            }

            // En passant - ALWAYS use full apply+check
            if (ep_bb && (PAWN_ATTACKS[side][from] & ep_bb)) {
                uint8_t ep_to = state.enPassantSquare;
                uint32_t ply = encode_ply(from, ep_to, 0);
                if (is_legal_generated(state, ply))
                    moves.push_back(ply);
            }
        }
    }

    // Knights
    {
        uint64_t knights = state.bitboards[offset + PIECE_KNIGHT];
        while (knights) {
            uint8_t from = pop_lsb(knights);
            // Pinned knight can never move legally
            if (info.pinned & (1ULL << from)) continue;

            uint64_t targets = KNIGHT_ATTACKS[from] & ~own_occ & ~state.bitboards[opp_off + PIECE_KING] & check_mask;
            while (targets) {
                uint8_t to = pop_lsb(targets);
                moves.push_back(encode_ply(from, to, 0));
            }
        }
    }

    // Bishops
    {
        uint64_t bishops = state.bitboards[offset + PIECE_BISHOP];
        while (bishops) {
            uint8_t from = pop_lsb(bishops);
            bool is_pinned = (info.pinned & (1ULL << from)) != 0;
            uint64_t pin_mask = is_pinned ? info.pin_ray[from] : ~0ULL;

            uint64_t targets = bishop_attacks(from, all_occ) & ~own_occ & ~state.bitboards[opp_off + PIECE_KING] & check_mask & pin_mask;
            while (targets) {
                uint8_t to = pop_lsb(targets);
                moves.push_back(encode_ply(from, to, 0));
            }
        }
    }

    // Rooks
    {
        uint64_t rooks = state.bitboards[offset + PIECE_ROOK];
        while (rooks) {
            uint8_t from = pop_lsb(rooks);
            bool is_pinned = (info.pinned & (1ULL << from)) != 0;
            uint64_t pin_mask = is_pinned ? info.pin_ray[from] : ~0ULL;

            uint64_t targets = rook_attacks(from, all_occ) & ~own_occ & ~state.bitboards[opp_off + PIECE_KING] & check_mask & pin_mask;
            while (targets) {
                uint8_t to = pop_lsb(targets);
                moves.push_back(encode_ply(from, to, 0));
            }
        }
    }

    // Queens
    {
        uint64_t queens = state.bitboards[offset + PIECE_QUEEN];
        while (queens) {
            uint8_t from = pop_lsb(queens);
            bool is_pinned = (info.pinned & (1ULL << from)) != 0;
            uint64_t pin_mask = is_pinned ? info.pin_ray[from] : ~0ULL;

            uint64_t targets = (bishop_attacks(from, all_occ) | rook_attacks(from, all_occ)) & ~own_occ & ~state.bitboards[opp_off + PIECE_KING] & check_mask & pin_mask;
            while (targets) {
                uint8_t to = pop_lsb(targets);
                moves.push_back(encode_ply(from, to, 0));
            }
        }
    }

    return moves;
}

// ============================================================
// Fast move generation into caller-provided array
// ============================================================

static inline int add_pawn_moves_fast(uint32_t* out, int count, uint8_t from, uint8_t to, uint8_t side) {
    if (is_promotion_square(side, to)) {
        for (uint8_t p = 1; p <= 4; p++)
            out[count++] = encode_ply(from, to, p);
    } else {
        out[count++] = encode_ply(from, to, 0);
    }
    return count;
}

int generate_legal_moves_fast(const GameState& state, uint32_t* out) {
    int count = 0;

    uint8_t side = state.sideToMove;
    uint8_t opp = side ^ 1;
    uint8_t offset = (side == COLOR_WHITE) ? WHITE_OFFSET : BLACK_OFFSET;
    uint8_t opp_off = (opp == COLOR_WHITE) ? WHITE_OFFSET : BLACK_OFFSET;

    uint64_t own_occ = side_occupancy(state, side);
    uint64_t opp_occ = side_occupancy(state, opp);
    uint64_t all_occ = own_occ | opp_occ;

    MoveGenInfo info = compute_movegen_info(state);
    uint8_t king_sq = info.king_sq;

    // King moves
    {
        uint64_t king_targets = KING_ATTACKS[king_sq] & ~own_occ & ~info.opp_attacks;
        king_targets &= ~state.bitboards[opp_off + PIECE_KING];
        while (king_targets) {
            uint8_t to = pop_lsb(king_targets);
            out[count++] = encode_ply(king_sq, to, 0);
        }
        if (info.num_checkers == 0) {
            if (side == COLOR_WHITE) {
                if ((state.castlingRights & 0x01) && can_castle(state, side, king_sq, 6, all_occ))
                    out[count++] = encode_ply(king_sq, 6, 0);
                if ((state.castlingRights & 0x02) && can_castle(state, side, king_sq, 2, all_occ))
                    out[count++] = encode_ply(king_sq, 2, 0);
            } else {
                if ((state.castlingRights & 0x04) && can_castle(state, side, king_sq, 62, all_occ))
                    out[count++] = encode_ply(king_sq, 62, 0);
                if ((state.castlingRights & 0x08) && can_castle(state, side, king_sq, 58, all_occ))
                    out[count++] = encode_ply(king_sq, 58, 0);
            }
        }
    }

    if (info.num_checkers >= 2) return count;

    uint64_t check_mask = info.check_mask;
    uint64_t ep_bb = (state.enPassantSquare != NO_EP) ? (1ULL << state.enPassantSquare) : 0;

    // Pawns
    {
        uint64_t pawns = state.bitboards[offset + PIECE_PAWN];
        while (pawns) {
            uint8_t from = pop_lsb(pawns);
            bool is_pinned = (info.pinned & (1ULL << from)) != 0;
            uint64_t pin_mask = is_pinned ? info.pin_ray[from] : ~0ULL;

            uint64_t push_targets = 0;
            if (side == COLOR_WHITE) {
                uint8_t fwd = from + 8;
                if (fwd < 64 && !(all_occ & (1ULL << fwd))) {
                    push_targets |= 1ULL << fwd;
                    if ((from >> 3) == 1 && !(all_occ & (1ULL << (from + 16))))
                        push_targets |= 1ULL << (from + 16);
                }
            } else {
                if (from >= 8) {
                    uint8_t fwd = from - 8;
                    if (!(all_occ & (1ULL << fwd))) {
                        push_targets |= 1ULL << fwd;
                        if ((from >> 3) == 6 && !(all_occ & (1ULL << (from - 16))))
                            push_targets |= 1ULL << (from - 16);
                    }
                }
            }
            push_targets &= check_mask & pin_mask;
            while (push_targets) {
                uint8_t to = pop_lsb(push_targets);
                count = add_pawn_moves_fast(out, count, from, to, side);
            }

            uint64_t capture_targets = PAWN_ATTACKS[side][from] & opp_occ & ~state.bitboards[opp_off + PIECE_KING];
            capture_targets &= check_mask & pin_mask;
            while (capture_targets) {
                uint8_t to = pop_lsb(capture_targets);
                count = add_pawn_moves_fast(out, count, from, to, side);
            }

            if (ep_bb && (PAWN_ATTACKS[side][from] & ep_bb)) {
                uint8_t ep_to = state.enPassantSquare;
                uint32_t ply = encode_ply(from, ep_to, 0);
                if (is_legal_generated(state, ply))
                    out[count++] = ply;
            }
        }
    }

    // Knights
    {
        uint64_t knights = state.bitboards[offset + PIECE_KNIGHT];
        while (knights) {
            uint8_t from = pop_lsb(knights);
            if (info.pinned & (1ULL << from)) continue;
            uint64_t targets = KNIGHT_ATTACKS[from] & ~own_occ & ~state.bitboards[opp_off + PIECE_KING] & check_mask;
            while (targets) {
                uint8_t to = pop_lsb(targets);
                out[count++] = encode_ply(from, to, 0);
            }
        }
    }

    // Bishops
    {
        uint64_t bishops = state.bitboards[offset + PIECE_BISHOP];
        while (bishops) {
            uint8_t from = pop_lsb(bishops);
            bool is_pinned = (info.pinned & (1ULL << from)) != 0;
            uint64_t pin_mask = is_pinned ? info.pin_ray[from] : ~0ULL;
            uint64_t targets = bishop_attacks(from, all_occ) & ~own_occ & ~state.bitboards[opp_off + PIECE_KING] & check_mask & pin_mask;
            while (targets) { out[count++] = encode_ply(from, pop_lsb(targets), 0); }
        }
    }

    // Rooks
    {
        uint64_t rooks = state.bitboards[offset + PIECE_ROOK];
        while (rooks) {
            uint8_t from = pop_lsb(rooks);
            bool is_pinned = (info.pinned & (1ULL << from)) != 0;
            uint64_t pin_mask = is_pinned ? info.pin_ray[from] : ~0ULL;
            uint64_t targets = rook_attacks(from, all_occ) & ~own_occ & ~state.bitboards[opp_off + PIECE_KING] & check_mask & pin_mask;
            while (targets) { out[count++] = encode_ply(from, pop_lsb(targets), 0); }
        }
    }

    // Queens
    {
        uint64_t queens = state.bitboards[offset + PIECE_QUEEN];
        while (queens) {
            uint8_t from = pop_lsb(queens);
            bool is_pinned = (info.pinned & (1ULL << from)) != 0;
            uint64_t pin_mask = is_pinned ? info.pin_ray[from] : ~0ULL;
            uint64_t targets = (bishop_attacks(from, all_occ) | rook_attacks(from, all_occ)) & ~own_occ & ~state.bitboards[opp_off + PIECE_KING] & check_mask & pin_mask;
            while (targets) { out[count++] = encode_ply(from, pop_lsb(targets), 0); }
        }
    }

    return count;
}

// ============================================================
// Count legal moves without building list (bulk counting)
// ============================================================

int count_legal_moves(const GameState& state) {
    int count = 0;

    uint8_t side = state.sideToMove;
    uint8_t opp = side ^ 1;
    uint8_t offset = (side == COLOR_WHITE) ? WHITE_OFFSET : BLACK_OFFSET;
    uint8_t opp_off = (opp == COLOR_WHITE) ? WHITE_OFFSET : BLACK_OFFSET;

    uint64_t own_occ = side_occupancy(state, side);
    uint64_t opp_occ = side_occupancy(state, opp);
    uint64_t all_occ = own_occ | opp_occ;

    MoveGenInfo info = compute_movegen_info(state);
    uint8_t king_sq = info.king_sq;

    // King moves — popcount the legal king targets
    {
        uint64_t king_targets = KING_ATTACKS[king_sq] & ~own_occ & ~info.opp_attacks;
        king_targets &= ~state.bitboards[opp_off + PIECE_KING];
        count += popcount(king_targets);

        if (info.num_checkers == 0) {
            if (side == COLOR_WHITE) {
                if ((state.castlingRights & 0x01) && can_castle(state, side, king_sq, 6, all_occ)) count++;
                if ((state.castlingRights & 0x02) && can_castle(state, side, king_sq, 2, all_occ)) count++;
            } else {
                if ((state.castlingRights & 0x04) && can_castle(state, side, king_sq, 62, all_occ)) count++;
                if ((state.castlingRights & 0x08) && can_castle(state, side, king_sq, 58, all_occ)) count++;
            }
        }
    }

    if (info.num_checkers >= 2) return count;

    uint64_t check_mask = info.check_mask;
    uint64_t ep_bb = (state.enPassantSquare != NO_EP) ? (1ULL << state.enPassantSquare) : 0;

    // Pawns — need special handling for promotions (4 moves per square)
    {
        uint64_t pawns = state.bitboards[offset + PIECE_PAWN];
        uint64_t promo_rank = (side == COLOR_WHITE) ? 0xFF00000000000000ULL : 0x00000000000000FFULL;

        while (pawns) {
            uint8_t from = pop_lsb(pawns);
            bool is_pinned = (info.pinned & (1ULL << from)) != 0;
            uint64_t pin_mask = is_pinned ? info.pin_ray[from] : ~0ULL;

            uint64_t push_targets = 0;
            if (side == COLOR_WHITE) {
                uint8_t fwd = from + 8;
                if (fwd < 64 && !(all_occ & (1ULL << fwd))) {
                    push_targets |= 1ULL << fwd;
                    if ((from >> 3) == 1 && !(all_occ & (1ULL << (from + 16))))
                        push_targets |= 1ULL << (from + 16);
                }
            } else {
                if (from >= 8) {
                    uint8_t fwd = from - 8;
                    if (!(all_occ & (1ULL << fwd))) {
                        push_targets |= 1ULL << fwd;
                        if ((from >> 3) == 6 && !(all_occ & (1ULL << (from - 16))))
                            push_targets |= 1ULL << (from - 16);
                    }
                }
            }
            push_targets &= check_mask & pin_mask;

            uint64_t capture_targets = PAWN_ATTACKS[side][from] & opp_occ & ~state.bitboards[opp_off + PIECE_KING];
            capture_targets &= check_mask & pin_mask;

            uint64_t all_targets = push_targets | capture_targets;
            uint64_t promo_targets = all_targets & promo_rank;
            uint64_t non_promo = all_targets & ~promo_rank;
            count += popcount(non_promo) + popcount(promo_targets) * 4;

            // EP
            if (ep_bb && (PAWN_ATTACKS[side][from] & ep_bb)) {
                uint32_t ply = encode_ply(from, state.enPassantSquare, 0);
                if (is_legal_generated(state, ply)) count++;
            }
        }
    }

    // Knights — pure popcount
    {
        uint64_t knights = state.bitboards[offset + PIECE_KNIGHT];
        while (knights) {
            uint8_t from = pop_lsb(knights);
            if (info.pinned & (1ULL << from)) continue;
            count += popcount(KNIGHT_ATTACKS[from] & ~own_occ & ~state.bitboards[opp_off + PIECE_KING] & check_mask);
        }
    }

    // Bishops — popcount
    {
        uint64_t bishops = state.bitboards[offset + PIECE_BISHOP];
        while (bishops) {
            uint8_t from = pop_lsb(bishops);
            uint64_t pin_mask = (info.pinned & (1ULL << from)) ? info.pin_ray[from] : ~0ULL;
            count += popcount(bishop_attacks(from, all_occ) & ~own_occ & ~state.bitboards[opp_off + PIECE_KING] & check_mask & pin_mask);
        }
    }

    // Rooks — popcount
    {
        uint64_t rooks = state.bitboards[offset + PIECE_ROOK];
        while (rooks) {
            uint8_t from = pop_lsb(rooks);
            uint64_t pin_mask = (info.pinned & (1ULL << from)) ? info.pin_ray[from] : ~0ULL;
            count += popcount(rook_attacks(from, all_occ) & ~own_occ & ~state.bitboards[opp_off + PIECE_KING] & check_mask & pin_mask);
        }
    }

    // Queens — popcount
    {
        uint64_t queens = state.bitboards[offset + PIECE_QUEEN];
        while (queens) {
            uint8_t from = pop_lsb(queens);
            uint64_t pin_mask = (info.pinned & (1ULL << from)) ? info.pin_ray[from] : ~0ULL;
            count += popcount((bishop_attacks(from, all_occ) | rook_attacks(from, all_occ)) & ~own_occ & ~state.bitboards[opp_off + PIECE_KING] & check_mask & pin_mask);
        }
    }

    return count;
}

// ============================================================
// Make / unmake moves (in-place mutation for perft)
// ============================================================

static uint8_t find_piece_at(const GameState& state, uint8_t sq, uint8_t side_offset) {
    uint64_t bit = 1ULL << sq;
    for (int i = 0; i < 6; i++) {
        if (state.bitboards[side_offset + i] & bit) return static_cast<uint8_t>(i);
    }
    return 255;
}

UndoInfo make_move(GameState& state, uint32_t ply) {
    uint8_t from, to, promo;
    decode_ply(ply, from, to, promo);

    uint8_t color = state.sideToMove;
    uint8_t offset = (color == COLOR_WHITE) ? WHITE_OFFSET : BLACK_OFFSET;
    uint8_t opp_offset = (color == COLOR_WHITE) ? BLACK_OFFSET : WHITE_OFFSET;

    // Save undo info
    UndoInfo undo;
    undo.hash = state.hash;
    undo.castlingRights = state.castlingRights;
    undo.enPassantSquare = state.enPassantSquare;
    undo.halfMoveClock = state.halfMoveClock;
    undo.capturedPiece = 255;
    undo.capturedSide = opp_offset;

    uint64_t from_mask = 1ULL << from;
    uint64_t to_mask = 1ULL << to;

    // Find mover piece type
    uint8_t piece_type = find_piece_at(state, from, offset);

    // Find captured piece (if any)
    uint64_t opp_occ = side_occupancy(state, color ^ 1);
    bool dst_has_piece = (opp_occ & to_mask) != 0;
    bool is_capture = dst_has_piece;

    if (piece_type == PIECE_PAWN) {
        bool is_ep = is_en_passant_capture(state, color, from, to);
        state.enPassantSquare = NO_EP;

        if (is_ep) {
            uint8_t captured_sq = (color == COLOR_WHITE) ? to - 8 : to + 8;
            undo.capturedPiece = PIECE_PAWN;
            state.bitboards[opp_offset + PIECE_PAWN] &= ~(1ULL << captured_sq);
            // Hash: remove captured pawn
            state.hash ^= ZOBRIST_PIECE[opp_offset + PIECE_PAWN][captured_sq];
            is_capture = true;
        }

        if (is_double_pawn_push(color, from, to)) {
            state.enPassantSquare = (color == COLOR_WHITE) ? from + 8 : from - 8;
        }

        if (is_promotion_square(color, to)) {
            uint8_t promoted = promotion_piece_type(promo);
            if (dst_has_piece) {
                undo.capturedPiece = find_piece_at(state, to, opp_offset);
                state.bitboards[opp_offset + undo.capturedPiece] &= ~to_mask;
                state.hash ^= ZOBRIST_PIECE[opp_offset + undo.capturedPiece][to];
            }
            state.bitboards[offset + PIECE_PAWN] &= ~from_mask;
            state.bitboards[offset + promoted] |= to_mask;
            // Hash: remove pawn from 'from', add promoted at 'to'
            state.hash ^= ZOBRIST_PIECE[offset + PIECE_PAWN][from];
            state.hash ^= ZOBRIST_PIECE[offset + promoted][to];
        } else {
            if (dst_has_piece) {
                undo.capturedPiece = find_piece_at(state, to, opp_offset);
                state.bitboards[opp_offset + undo.capturedPiece] &= ~to_mask;
                state.hash ^= ZOBRIST_PIECE[opp_offset + undo.capturedPiece][to];
            }
            move_piece(state, offset + piece_type, from, to);
            state.hash ^= ZOBRIST_PIECE[offset + piece_type][from];
            state.hash ^= ZOBRIST_PIECE[offset + piece_type][to];
        }
    } else if (piece_type == PIECE_KING) {
        state.enPassantSquare = NO_EP;
        if (is_castle_move(color, from, to)) {
            apply_castle(state, color, from, to);
            // Hash: move king and rook
            state.hash ^= ZOBRIST_PIECE[offset + PIECE_KING][from];
            state.hash ^= ZOBRIST_PIECE[offset + PIECE_KING][to];
            uint8_t rook_from, rook_to;
            if (color == COLOR_WHITE && to == 6)       { rook_from = 7; rook_to = 5; }
            else if (color == COLOR_WHITE && to == 2)  { rook_from = 0; rook_to = 3; }
            else if (color == COLOR_BLACK && to == 62) { rook_from = 63; rook_to = 61; }
            else                                       { rook_from = 56; rook_to = 59; }
            state.hash ^= ZOBRIST_PIECE[offset + PIECE_ROOK][rook_from];
            state.hash ^= ZOBRIST_PIECE[offset + PIECE_ROOK][rook_to];
        } else {
            if (dst_has_piece) {
                undo.capturedPiece = find_piece_at(state, to, opp_offset);
                state.bitboards[opp_offset + undo.capturedPiece] &= ~to_mask;
                state.hash ^= ZOBRIST_PIECE[opp_offset + undo.capturedPiece][to];
            }
            move_piece(state, offset + PIECE_KING, from, to);
            state.hash ^= ZOBRIST_PIECE[offset + PIECE_KING][from];
            state.hash ^= ZOBRIST_PIECE[offset + PIECE_KING][to];
        }
        state.castlingRights = clear_castling_for_king(state.castlingRights, color);
    } else {
        state.enPassantSquare = NO_EP;
        if (dst_has_piece) {
            undo.capturedPiece = find_piece_at(state, to, opp_offset);
            state.bitboards[opp_offset + undo.capturedPiece] &= ~to_mask;
            state.hash ^= ZOBRIST_PIECE[opp_offset + undo.capturedPiece][to];
        }
        move_piece(state, offset + piece_type, from, to);
        state.hash ^= ZOBRIST_PIECE[offset + piece_type][from];
        state.hash ^= ZOBRIST_PIECE[offset + piece_type][to];
        if (piece_type == PIECE_ROOK) {
            state.castlingRights = clear_castling_for_rook_move(state.castlingRights, color, from);
        }
    }

    if (dst_has_piece) {
        state.castlingRights = clear_castling_for_rook_capture(state.castlingRights, to);
    }

    if (piece_type == PIECE_PAWN || is_capture) {
        state.halfMoveClock = 0;
    } else {
        state.halfMoveClock++;
    }

    if (state.sideToMove == COLOR_BLACK) state.fullMoveNumber++;
    state.sideToMove ^= 1;

    // Update hash for side, castling, EP changes
    state.hash ^= ZOBRIST_SIDE;
    state.hash ^= ZOBRIST_CASTLING[undo.castlingRights & 0x0F];
    state.hash ^= ZOBRIST_CASTLING[state.castlingRights & 0x0F];
    state.hash ^= ZOBRIST_EP[undo.enPassantSquare == NO_EP ? 64 : undo.enPassantSquare];
    state.hash ^= ZOBRIST_EP[state.enPassantSquare == NO_EP ? 64 : state.enPassantSquare];

    return undo;
}

void unmake_move(GameState& state, uint32_t ply, const UndoInfo& undo) {
    uint8_t from, to, promo;
    decode_ply(ply, from, to, promo);

    // Restore side first
    state.sideToMove ^= 1;
    uint8_t color = state.sideToMove;
    uint8_t offset = (color == COLOR_WHITE) ? WHITE_OFFSET : BLACK_OFFSET;
    uint8_t opp_offset = undo.capturedSide;

    uint64_t from_mask = 1ULL << from;
    uint64_t to_mask = 1ULL << to;

    uint8_t piece_type = find_piece_at(state, to, offset);

    if (promo != 0) {
        // Undo promotion: remove promoted piece from 'to', put pawn back on 'from'
        uint8_t promoted = promotion_piece_type(promo);
        state.bitboards[offset + promoted] &= ~to_mask;
        state.bitboards[offset + PIECE_PAWN] |= from_mask;
    } else if (piece_type == PIECE_KING && is_castle_move(color, from, to)) {
        // Undo castling: move king back, move rook back
        move_piece(state, offset + PIECE_KING, to, from);
        uint8_t rook_from, rook_to;
        if (color == COLOR_WHITE && to == 6)       { rook_from = 7; rook_to = 5; }
        else if (color == COLOR_WHITE && to == 2)  { rook_from = 0; rook_to = 3; }
        else if (color == COLOR_BLACK && to == 62) { rook_from = 63; rook_to = 61; }
        else                                       { rook_from = 56; rook_to = 59; }
        move_piece(state, offset + PIECE_ROOK, rook_to, rook_from);
    } else {
        // Normal move: move piece back
        move_piece(state, offset + piece_type, to, from);
    }

    // Restore captured piece
    if (undo.capturedPiece != 255) {
        if (undo.enPassantSquare != NO_EP &&
            to == undo.enPassantSquare &&
            find_piece_at(state, from, offset) == PIECE_PAWN) {
            // EP capture: restore pawn at captured square
            uint8_t captured_sq = (color == COLOR_WHITE) ? to - 8 : to + 8;
            state.bitboards[opp_offset + PIECE_PAWN] |= (1ULL << captured_sq);
        } else {
            state.bitboards[opp_offset + undo.capturedPiece] |= to_mask;
        }
    }

    // Restore state
    state.hash = undo.hash;
    state.castlingRights = undo.castlingRights;
    state.enPassantSquare = undo.enPassantSquare;
    state.halfMoveClock = undo.halfMoveClock;
    if (color == COLOR_BLACK) state.fullMoveNumber--;
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

    rebuild_mailbox(state);
    state.hash = compute_zobrist(state);
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
    // Uses FNV-1a for stable position hashing.
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
