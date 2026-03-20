// chess_engine.h — Bitboard chess engine core.
// This implementation provides the standalone C++ engine used by the R package.
//
// Square mapping:
//   sq = rank * 8 + file
//   a1=0, b1=1, ..., h1=7, a2=8, ..., h8=63
//
// Bitboards: uint64_t, bit i set => piece on square i
// 12 boards: [0..5] = white P,N,B,R,Q,K; [6..11] = black P,N,B,R,Q,K
//
// Move encoding (uint32_t): from[5:0] | to[11:6] | promo[14:12]
//   promo: 0=none, 1=N, 2=B, 3=R, 4=Q

#ifndef CHESS_ENGINE_H
#define CHESS_ENGINE_H

#include <cstdint>
#include <cstring>
#include <string>
#include <vector>

namespace chess {

// ============================================================
// Constants
// ============================================================

constexpr uint8_t COLOR_WHITE = 0;
constexpr uint8_t COLOR_BLACK = 1;

constexpr uint8_t PIECE_PAWN   = 0;
constexpr uint8_t PIECE_KNIGHT = 1;
constexpr uint8_t PIECE_BISHOP = 2;
constexpr uint8_t PIECE_ROOK   = 3;
constexpr uint8_t PIECE_QUEEN  = 4;
constexpr uint8_t PIECE_KING   = 5;

constexpr uint8_t WHITE_OFFSET = 0;
constexpr uint8_t BLACK_OFFSET = 6;

constexpr uint8_t NO_EP = 255;

// ============================================================
// GameState — internal bitboard position representation
// ============================================================

constexpr uint8_t NO_PIECE = 255;

struct GameState {
    uint64_t bitboards[12];
    uint64_t hash;             // incremental Zobrist hash
    uint8_t  mailbox[64];      // piece_index (0-11) per square, NO_PIECE if empty
    uint8_t  sideToMove;
    uint8_t  castlingRights;   // bits: 0=WK, 1=WQ, 2=BK, 3=BQ
    uint8_t  enPassantSquare;  // 0-63, 255 = none
    uint16_t halfMoveClock;
    uint16_t fullMoveNumber;
};

// Undo info for make/unmake
struct UndoInfo {
    uint64_t hash;
    uint8_t  castlingRights;
    uint8_t  enPassantSquare;
    uint16_t halfMoveClock;
    uint8_t  capturedPiece;    // piece type (0-5) or 255 if no capture
    uint8_t  capturedSide;     // offset of captured piece's side
};

// Zobrist hashing
void init_zobrist();
uint64_t compute_zobrist(const GameState& state);

// Make/unmake moves (mutate in place)
UndoInfo make_move(GameState& state, uint32_t ply);
void unmake_move(GameState& state, uint32_t ply, const UndoInfo& undo);

// ============================================================
// Bit manipulation (hardware intrinsics — portable via #ifdef)
// ============================================================
#ifdef _MSC_VER
  #include <intrin.h>
  #pragma intrinsic(_BitScanForward64, __popcnt64)
  inline uint8_t lsb_index(uint64_t bb) {
      unsigned long idx;
      _BitScanForward64(&idx, bb);
      return static_cast<uint8_t>(idx);
  }
  inline uint8_t popcount(uint64_t bb) {
      return static_cast<uint8_t>(__popcnt64(bb));
  }
#else
inline uint8_t lsb_index(uint64_t bb) {
    return static_cast<uint8_t>(__builtin_ctzll(bb));
}

inline uint8_t popcount(uint64_t bb) {
    return static_cast<uint8_t>(__builtin_popcountll(bb));
}
#endif

inline uint64_t lsb(uint64_t bb) {
    return bb & (~bb + 1);
}

inline uint8_t pop_lsb(uint64_t& bb) {
    uint8_t idx = lsb_index(bb);
    bb &= bb - 1;
    return idx;
}

// ============================================================
// Move encoding / decoding
// ============================================================

inline uint32_t encode_ply(uint8_t from, uint8_t to, uint8_t promo) {
    return static_cast<uint32_t>(from) |
           (static_cast<uint32_t>(to) << 6) |
           (static_cast<uint32_t>(promo) << 12);
}

inline void decode_ply(uint32_t ply, uint8_t& from, uint8_t& to, uint8_t& promo) {
    from  = ply & 0x3F;
    to    = (ply >> 6) & 0x3F;
    promo = (ply >> 12) & 0x07;
}

// ============================================================
// Square helpers
// ============================================================

inline uint8_t sq_rank(uint8_t sq) { return sq >> 3; }
inline uint8_t sq_file(uint8_t sq) { return sq & 7; }
inline uint8_t abs_diff(uint8_t a, uint8_t b) { return a > b ? a - b : b - a; }
inline int8_t  abs8(int8_t x) { return x >= 0 ? x : -x; }

// ============================================================
// Occupancy helpers
// ============================================================

inline uint64_t side_occupancy(const GameState& s, uint8_t side) {
    uint8_t off = (side == COLOR_WHITE) ? WHITE_OFFSET : BLACK_OFFSET;
    return s.bitboards[off] | s.bitboards[off+1] | s.bitboards[off+2] |
           s.bitboards[off+3] | s.bitboards[off+4] | s.bitboards[off+5];
}

// ============================================================
// Attack tables (precomputed at startup)
// ============================================================

extern uint64_t KNIGHT_ATTACKS[64];
extern uint64_t KING_ATTACKS[64];
extern uint64_t PAWN_ATTACKS[2][64];

void init_attack_tables();

// ============================================================
// Attack generation
// ============================================================

uint64_t knight_attacks(uint8_t sq);
uint64_t king_attacks(uint8_t sq);
uint64_t rook_attacks(uint8_t sq, uint64_t occ);
uint64_t bishop_attacks(uint8_t sq, uint64_t occ);
uint64_t ray_attacks_slow(uint8_t sq, int8_t df, int8_t dr, uint64_t occ, uint64_t own_occ);

// ============================================================
// Core engine functions
// ============================================================

// Init starting position
void init_game(GameState& state);

// Find king square
uint8_t find_king_square(const GameState& state, uint8_t color);

// Check detection
bool in_check(const GameState& state, uint8_t color);
bool is_square_attacked(const GameState& state, uint8_t square, uint8_t attacker_color);
bool is_square_attacked_with_occ(const GameState& state, uint8_t square, uint8_t attacker_color, uint64_t occ);

// Move validation + application
bool is_pseudo_legal_ply(const GameState& state, uint32_t ply);
GameState apply_ply_to_memory(const GameState& state, uint32_t ply);
bool is_legal_ply(const GameState& state, uint32_t ply);
GameState apply_ply_memory(const GameState& state, uint32_t ply); // validates + applies

// Legal move detection
bool has_any_legal_ply(const GameState& state);
struct LegalPlyResult { bool hasLegal; bool isInCheck; };
LegalPlyResult has_any_legal_ply_with_check(const GameState& state);

// Draw detection
bool is_insufficient_material(const GameState& state);

// Position validation
bool validate_position(const GameState& state);

// Legal move generation (full list)
std::vector<uint32_t> generate_legal_moves(const GameState& state);

// Fast move generation into caller-provided array (max 256 moves). Returns count.
int generate_legal_moves_fast(const GameState& state, uint32_t* out);

// Count legal moves without building list (for perft leaf nodes)
int count_legal_moves(const GameState& state);

// FEN parsing
GameState parse_fen(const std::string& fen);
std::string state_to_fen(const GameState& state);

// UCI move conversion
uint32_t uci_to_ply(const GameState& state, const std::string& uci);
std::string ply_to_uci(uint32_t ply);

// Position hashing (for repetition)
uint64_t position_hash(const GameState& state);

} // namespace chess

#endif // CHESS_ENGINE_H
