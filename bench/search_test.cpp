// search_test.cpp — Quick test of the search function
// Compile: g++ -O3 -std=c++17 -march=native -o search_test bench/search_test.cpp src/chess_engine.cpp
#include "../src/chess_engine.h"
#include <cstdio>
#include <chrono>

using namespace chess;
using Clock = std::chrono::high_resolution_clock;

int main() {
    init_attack_tables();
    init_zobrist();

    printf("=== Search Test ===\n\n");

    // Test 1: Starting position
    {
        GameState state;
        init_game(state);
        printf("Starting position:\n");
        for (int d = 1; d <= 8; d++) {
            auto t0 = Clock::now();
            SearchResult r = find_best_move(state, d);
            auto t1 = Clock::now();
            double elapsed = std::chrono::duration<double>(t1 - t0).count();
            printf("  depth %d: %s  score=%+d  nodes=%llu  %.3fs\n",
                   d, ply_to_uci(r.best_move).c_str(), r.score, r.nodes, elapsed);
            fflush(stdout);
        }
    }

    printf("\n");

    // Test 2: Mate in 1 (Qh5-f7#)
    {
        const char* fen = "rnbqkbnr/pppp1ppp/8/4p3/2B1P3/5Q2/PPPP1PPP/RNB1K1NR w KQkq - 0 3";
        GameState state = parse_fen(fen);
        printf("Scholar's mate (Qf7#):\n");
        SearchResult r = find_best_move(state, 4);
        printf("  best: %s  score=%+d  nodes=%llu\n",
               ply_to_uci(r.best_move).c_str(), r.score, r.nodes);
    }

    printf("\n");

    // Test 3: Mate in 2
    {
        const char* fen = "2bqkbn1/2pppp2/np2N3/r3P1p1/p2N2B1/5Q2/PPPPPP1P/RNB1K2R w KQ - 0 1";
        GameState state = parse_fen(fen);
        printf("Mate in 2:\n");
        SearchResult r = find_best_move(state, 6);
        printf("  best: %s  score=%+d  nodes=%llu\n",
               ply_to_uci(r.best_move).c_str(), r.score, r.nodes);
    }

    printf("\n");

    // Test 4: Tactical position — should find winning capture
    {
        const char* fen = "r1bqkb1r/pppppppp/2n2n2/4N3/4P3/8/PPPP1PPP/RNBQKB1R w KQkq - 0 3";
        GameState state = parse_fen(fen);
        printf("Tactical (Nxc6 wins material):\n");
        for (int d = 1; d <= 6; d++) {
            SearchResult r = find_best_move(state, d);
            printf("  depth %d: %s  score=%+d  nodes=%llu\n",
                   d, ply_to_uci(r.best_move).c_str(), r.score, r.nodes);
        }
    }

    printf("\n");

    // Test 5: Middlegame position
    {
        const char* fen = "r1bq1rk1/ppp2ppp/2np1n2/2b1p3/2B1P3/2NP1N2/PPP2PPP/R1BQ1RK1 w - - 0 7";
        GameState state = parse_fen(fen);
        printf("Italian Game middlegame:\n");
        for (int d = 1; d <= 8; d++) {
            auto t0 = Clock::now();
            SearchResult r = find_best_move(state, d);
            auto t1 = Clock::now();
            double elapsed = std::chrono::duration<double>(t1 - t0).count();
            printf("  depth %d: %s  score=%+d  nodes=%llu  %.3fs\n",
                   d, ply_to_uci(r.best_move).c_str(), r.score, r.nodes, elapsed);
            fflush(stdout);
            if (elapsed > 10.0) break;
        }
    }

    return 0;
}
