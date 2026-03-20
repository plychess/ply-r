// profile_breakdown.cpp — Measure where time is spent per node
#include "../src/chess_engine.h"
#include <cstdio>
#include <chrono>

using namespace chess;
using Clock = std::chrono::high_resolution_clock;

static uint64_t g_nodes = 0;
static uint64_t g_movegen_ns = 0;
static uint64_t g_apply_ns = 0;
static uint64_t g_check_ns = 0;

static uint64_t perft_profile(const GameState& state, int depth) {
    if (depth == 0) return 1ULL;

    auto t0 = Clock::now();
    std::vector<uint32_t> moves = generate_legal_moves(state);
    auto t1 = Clock::now();
    g_movegen_ns += std::chrono::duration_cast<std::chrono::nanoseconds>(t1 - t0).count();

    if (depth == 1) { g_nodes += moves.size(); return moves.size(); }

    uint64_t total = 0;
    for (uint32_t m : moves) {
        auto t2 = Clock::now();
        GameState child = apply_ply_to_memory(state, m);
        auto t3 = Clock::now();
        g_apply_ns += std::chrono::duration_cast<std::chrono::nanoseconds>(t3 - t2).count();
        g_nodes++;

        total += perft_profile(child, depth - 1);
    }
    return total;
}

// Measure raw operation costs
static void microbench() {
    GameState state;
    init_game(state);
    auto moves = generate_legal_moves(state);

    printf("=== Micro-benchmarks (1M iterations) ===\n\n");
    int N = 1000000;

    // 1. generate_legal_moves
    {
        auto t0 = Clock::now();
        for (int i = 0; i < N; i++) {
            auto m = generate_legal_moves(state);
        }
        auto t1 = Clock::now();
        double ns = std::chrono::duration_cast<std::chrono::nanoseconds>(t1 - t0).count() / (double)N;
        printf("  generate_legal_moves:  %6.0f ns/call  (%.1fM calls/sec)\n", ns, 1e9/ns/1e6);
    }

    // 2. apply_ply_to_memory
    {
        auto t0 = Clock::now();
        for (int i = 0; i < N; i++) {
            GameState child = apply_ply_to_memory(state, moves[i % moves.size()]);
        }
        auto t1 = Clock::now();
        double ns = std::chrono::duration_cast<std::chrono::nanoseconds>(t1 - t0).count() / (double)N;
        printf("  apply_ply_to_memory:   %6.0f ns/call  (%.1fM calls/sec)\n", ns, 1e9/ns/1e6);
    }

    // 3. in_check
    {
        auto t0 = Clock::now();
        for (int i = 0; i < N; i++) {
            in_check(state, COLOR_WHITE);
        }
        auto t1 = Clock::now();
        double ns = std::chrono::duration_cast<std::chrono::nanoseconds>(t1 - t0).count() / (double)N;
        printf("  in_check:              %6.0f ns/call  (%.1fM calls/sec)\n", ns, 1e9/ns/1e6);
    }

    // 4. is_square_attacked
    {
        auto t0 = Clock::now();
        for (int i = 0; i < N; i++) {
            is_square_attacked(state, 4, COLOR_BLACK);
        }
        auto t1 = Clock::now();
        double ns = std::chrono::duration_cast<std::chrono::nanoseconds>(t1 - t0).count() / (double)N;
        printf("  is_square_attacked:    %6.0f ns/call  (%.1fM calls/sec)\n", ns, 1e9/ns/1e6);
    }

    // 5. GameState copy
    {
        auto t0 = Clock::now();
        for (int i = 0; i < N; i++) {
            GameState copy = state;
            (void)copy;
        }
        auto t1 = Clock::now();
        double ns = std::chrono::duration_cast<std::chrono::nanoseconds>(t1 - t0).count() / (double)N;
        printf("  GameState copy:        %6.0f ns/call  (%.1fM copies/sec)  [%zu bytes]\n",
               ns, 1e9/ns/1e6, sizeof(GameState));
    }

    // 6. Magic lookup alone
    {
        uint64_t occ = side_occupancy(state, COLOR_WHITE) | side_occupancy(state, COLOR_BLACK);
        auto t0 = Clock::now();
        volatile uint64_t sink = 0;
        for (int i = 0; i < N; i++) {
            sink = rook_attacks(0, occ) | bishop_attacks(27, occ);
        }
        auto t1 = Clock::now();
        double ns = std::chrono::duration_cast<std::chrono::nanoseconds>(t1 - t0).count() / (double)N;
        printf("  magic lookup (r+b):    %6.0f ns/call  (%.1fM calls/sec)\n", ns, 1e9/ns/1e6);
    }
}

int main() {
    init_attack_tables();

    microbench();

    printf("\n=== Perft(5) profiled breakdown ===\n\n");
    GameState start;
    init_game(start);

    g_nodes = 0; g_movegen_ns = 0; g_apply_ns = 0; g_check_ns = 0;

    auto t0 = Clock::now();
    uint64_t nodes = perft_profile(start, 5);
    auto t1 = Clock::now();
    double total_ns = std::chrono::duration_cast<std::chrono::nanoseconds>(t1 - t0).count();

    double movegen_pct = 100.0 * g_movegen_ns / total_ns;
    double apply_pct = 100.0 * g_apply_ns / total_ns;
    double other_pct = 100.0 - movegen_pct - apply_pct;

    printf("  Total:     %.3f ms  (%llu nodes, %.1fM NPS)\n", total_ns/1e6, nodes, nodes/(total_ns/1e9)/1e6);
    printf("  Movegen:   %.3f ms  (%.1f%%)\n", g_movegen_ns/1e6, movegen_pct);
    printf("  Apply:     %.3f ms  (%.1f%%)\n", g_apply_ns/1e6, apply_pct);
    printf("  Other:     %.1f%%  (recursion, control flow)\n", other_pct);

    return 0;
}
