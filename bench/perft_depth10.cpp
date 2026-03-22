// perft_depth10.cpp — Depth 10 perft with large TT
#include "../src/chess_engine.h"
#include <cstdio>
#include <chrono>
#include <thread>
#include <vector>
#include <numeric>
#include <atomic>
#include <cstring>

using namespace chess;
using Clock = std::chrono::high_resolution_clock;

struct TTEntry { uint64_t key; uint64_t nodes; uint8_t depth; };
static constexpr size_t TT_SIZE = 1 << 26;  // 64M entries (~1.1GB)
static constexpr size_t TT_MASK = TT_SIZE - 1;

static uint64_t perft(const GameState& state, int depth, TTEntry* tt) {
    if (depth == 0) return 1ULL;
    if (depth == 1) return static_cast<uint64_t>(count_legal_moves(state));
    uint64_t h = state.hash;
    size_t idx = h & TT_MASK;
    TTEntry e = tt[idx];
    if (e.depth == depth && (e.key ^ e.nodes) == h) return e.nodes;
    uint32_t moves[256];
    int count = generate_legal_moves_fast(state, moves);
    uint64_t total = 0;
    for (int i = 0; i < count; i++) {
        GameState child = apply_ply_to_memory(state, moves[i]);
        __builtin_prefetch(&tt[child.hash & TT_MASK], 0, 1);
        total += perft(child, depth - 1, tt);
    }
    if (e.depth <= depth) tt[idx] = {h ^ total, total, static_cast<uint8_t>(depth)};
    return total;
}

int main() {
    init_attack_tables();
    init_zobrist();
    int hw = std::thread::hardware_concurrency();

    GameState start;
    init_game(start);

    uint64_t expected = 69352859712417ULL;
    printf("Perft(10) — Starting position — %d threads, TT=%zuM entries\n", hw, TT_SIZE / (1024*1024));
    printf("Expected: 69,352,859,712,417 nodes (69.4 trillion)\n\n");

    // Split at depth 3 for more parallelism
    struct WorkItem { GameState state; int remaining_depth; };
    std::vector<WorkItem> work;
    auto root_moves = generate_legal_moves(start);
    for (uint32_t m : root_moves) {
        GameState c1 = apply_ply_to_memory(start, m);
        auto c1_moves = generate_legal_moves(c1);
        for (uint32_t m2 : c1_moves) {
            GameState c2 = apply_ply_to_memory(c1, m2);
            auto c2_moves = generate_legal_moves(c2);
            for (uint32_t m3 : c2_moves) {
                work.push_back({apply_ply_to_memory(c2, m3), 7}); // depth 10 - 3 = 7
            }
        }
    }
    printf("Work items: %zu (split at depth 3)\n", work.size());
    printf("Allocating TT (%.0f MB)... ", TT_SIZE * sizeof(TTEntry) / 1e6);
    fflush(stdout);

    TTEntry* shared_tt = new TTEntry[TT_SIZE]();
    printf("done\n\n");
    fflush(stdout);

    std::vector<uint64_t> results(work.size(), 0);
    std::atomic<size_t> next_item{0};

    auto t0 = Clock::now();
    auto worker = [&]() {
        while (true) {
            size_t i = next_item.fetch_add(1, std::memory_order_relaxed);
            if (i >= work.size()) break;
            results[i] = perft(work[i].state, work[i].remaining_depth, shared_tt);
        }
    };
    std::vector<std::thread> threads;
    for (int i = 0; i < hw; i++) threads.emplace_back(worker);
    for (auto& th : threads) th.join();
    auto t1 = Clock::now();

    delete[] shared_tt;

    uint64_t nodes = std::accumulate(results.begin(), results.end(), 0ULL);
    double elapsed = std::chrono::duration<double>(t1 - t0).count();
    double nps = nodes / elapsed;

    printf("Nodes:    %llu\n", nodes);
    printf("Expected: %llu\n", expected);
    printf("Time:     %.2f seconds (%.1f min)\n", elapsed, elapsed / 60.0);
    printf("Speed:    %.1fB NPS\n", nps / 1e9);
    printf("Result:   [%s]\n", nodes == expected ? "PASS" : "FAIL");

    return 0;
}
