#include "../src/chess_engine.h"
#include <cstdio>
using namespace chess;

static uint64_t perft(const GameState& state, int depth) {
    if (depth == 0) return 1ULL;
    if (depth == 1) return static_cast<uint64_t>(count_legal_moves(state));
    uint32_t moves[256];
    int count = generate_legal_moves_fast(state, moves);
    uint64_t total = 0;
    for (int i = 0; i < count; i++) {
        GameState child = apply_ply_to_memory(state, moves[i]);
        total += perft(child, depth - 1);
    }
    return total;
}

int main() {
    init_attack_tables();
    init_zobrist();
    
    const char* fen5 = "rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8";
    const char* fen6 = "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10";
    
    GameState s5 = parse_fen(fen5);
    GameState s6 = parse_fen(fen6);
    
    printf("Position 5 depth 6: %llu (expected 3048196529)\n", perft(s5, 6));
    printf("Position 6 depth 6: %llu (expected 6923051137)\n", perft(s6, 6));
    
    printf("Position 6 depth 7 (single-threaded, no TT)... ");
    fflush(stdout);
    printf("%llu\n", perft(s6, 7));
    
    return 0;
}
