// perft_bench.cpp — Standalone C++ perft benchmark
// Single-threaded vs multi-threaded comparison with bulk counting + deeper splitting.
//
// Compile:  g++ -O3 -std=c++17 -march=native -flto -pthread -o perft_bench bench/perft_bench.cpp src/chess_engine.cpp
// Run:      ./perft_bench

#include "../src/chess_engine.h"
#include <cstdio>
#include <chrono>
#include <thread>
#include <vector>
#include <numeric>

using namespace chess;
using Clock = std::chrono::high_resolution_clock;

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

static uint64_t perft_mt(const GameState& state, int depth, int n_threads) {
    if (depth <= 2) return perft(state, depth);

    struct WorkItem { GameState state; int remaining_depth; };
    std::vector<WorkItem> work;
    auto root_moves = generate_legal_moves(state);
    for (uint32_t m : root_moves) {
        GameState child1 = apply_ply_to_memory(state, m);
        auto child_moves = generate_legal_moves(child1);
        for (uint32_t m2 : child_moves) {
            GameState child2 = apply_ply_to_memory(child1, m2);
            work.push_back({child2, depth - 2});
        }
    }

    std::vector<uint64_t> results(work.size(), 0);
    auto worker = [&](int tid) {
        for (size_t i = tid; i < work.size(); i += n_threads) {
            results[i] = perft(work[i].state, work[i].remaining_depth);
        }
    };

    std::vector<std::thread> threads;
    for (int t = 0; t < n_threads; t++) threads.emplace_back(worker, t);
    for (auto& t : threads) t.join();
    return std::accumulate(results.begin(), results.end(), 0ULL);
}

int main() {
    init_attack_tables();
    unsigned hw_threads = std::thread::hardware_concurrency();
    printf("Hardware threads: %u\n\n", hw_threads);

    GameState start;
    init_game(start);

    printf("=== SINGLE-THREADED (starting position) ===\n");
    printf("%-8s %15s %10s %15s\n", "Depth", "Nodes", "Time (s)", "NPS");
    printf("------------------------------------------------------\n");

    for (int d = 1; d <= 7; d++) {
        auto t0 = Clock::now();
        uint64_t nodes = perft(start, d);
        auto t1 = Clock::now();
        double elapsed = std::chrono::duration<double>(t1 - t0).count();
        double nps = (elapsed > 0) ? nodes / elapsed : 0;
        printf("%-8d %15llu %10.3f %15.0f\n", d, nodes, elapsed, nps);
        fflush(stdout);
        if (elapsed > 60.0) { printf("(stopping)\n"); break; }
    }

    int thread_counts[] = {2, 4, 6, 8, 12};
    for (int nt : thread_counts) {
        if (nt > (int)hw_threads) break;
        printf("\n=== %d THREADS (starting position, depth 6) ===\n", nt);
        auto t0 = Clock::now();
        uint64_t nodes = perft_mt(start, 6, nt);
        auto t1 = Clock::now();
        double elapsed = std::chrono::duration<double>(t1 - t0).count();
        double nps = (elapsed > 0) ? nodes / elapsed : 0;
        printf("  %llu nodes in %.3fs = %.1fM NPS\n", nodes, elapsed, nps/1e6);
        fflush(stdout);
    }

    return 0;
}
