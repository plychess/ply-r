// perft_bench.cpp — Standalone C++ perft benchmark
// Single-threaded vs multi-threaded comparison (std::thread, no OpenMP)
//
// Compile:  g++ -O3 -std=c++17 -pthread -o perft_bench bench/perft_bench.cpp src/chess_engine.cpp
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
    std::vector<uint32_t> moves = generate_legal_moves(state);
    if (depth == 1) return moves.size();
    uint64_t total = 0;
    for (uint32_t m : moves) {
        GameState child = apply_ply_to_memory(state, m);
        total += perft(child, depth - 1);
    }
    return total;
}

static uint64_t perft_mt(const GameState& state, int depth, int n_threads) {
    if (depth <= 1) return perft(state, depth);

    std::vector<uint32_t> moves = generate_legal_moves(state);
    int n_moves = static_cast<int>(moves.size());
    std::vector<uint64_t> results(n_moves, 0);

    // Each thread processes a slice of root moves
    auto worker = [&](int thread_id) {
        for (int i = thread_id; i < n_moves; i += n_threads) {
            GameState child = apply_ply_to_memory(state, moves[i]);
            results[i] = perft(child, depth - 1);
        }
    };

    std::vector<std::thread> threads;
    for (int t = 0; t < n_threads; t++) {
        threads.emplace_back(worker, t);
    }
    for (auto& t : threads) {
        t.join();
    }

    return std::accumulate(results.begin(), results.end(), 0ULL);
}

int main() {
    init_attack_tables();

    unsigned hw_threads = std::thread::hardware_concurrency();
    printf("Hardware threads: %u\n\n", hw_threads);

    GameState start;
    init_game(start);

    // --- Single-threaded ---
    printf("=== SINGLE-THREADED (starting position) ===\n");
    printf("%-8s %15s %10s %15s\n", "Depth", "Nodes", "Time (s)", "NPS");
    printf("------------------------------------------------------\n");

    double st_time_d6 = 0;
    for (int d = 1; d <= 6; d++) {
        auto t0 = Clock::now();
        uint64_t nodes = perft(start, d);
        auto t1 = Clock::now();
        double elapsed = std::chrono::duration<double>(t1 - t0).count();
        double nps = (elapsed > 0) ? nodes / elapsed : 0;
        printf("%-8d %15llu %10.3f %15.0f\n", d, nodes, elapsed, nps);
        fflush(stdout);
        if (d == 6) st_time_d6 = elapsed;
    }

    // --- Multi-threaded ---
    int thread_counts[] = {2, 4, 6, 8};
    for (int nt : thread_counts) {
        if (nt > (int)hw_threads) break;

        printf("\n=== %d THREADS (starting position) ===\n", nt);
        printf("%-8s %15s %10s %15s %10s\n", "Depth", "Nodes", "Time (s)", "NPS", "Speedup");
        printf("--------------------------------------------------------------\n");

        for (int d = 4; d <= 6; d++) {
            auto t0 = Clock::now();
            uint64_t nodes = perft_mt(start, d, nt);
            auto t1 = Clock::now();
            double elapsed = std::chrono::duration<double>(t1 - t0).count();
            double nps = (elapsed > 0) ? nodes / elapsed : 0;

            // Speedup vs single-thread at same depth
            double st_elapsed = 0;
            {
                auto s0 = Clock::now();
                perft(start, d);
                auto s1 = Clock::now();
                st_elapsed = std::chrono::duration<double>(s1 - s0).count();
            }
            double speedup = (elapsed > 0) ? st_elapsed / elapsed : 0;

            printf("%-8d %15llu %10.3f %15.0f %9.1fx\n", d, nodes, elapsed, nps, speedup);
            fflush(stdout);
        }
    }

    return 0;
}
