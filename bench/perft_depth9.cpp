// perft_depth9.cpp — Multi-threaded depth 9 perft
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
static constexpr size_t TT_SIZE = 1 << 22;
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

    uint64_t expected = 2439530234167ULL;
    printf("Perft(9) — Starting position — %d threads\n", hw);
    printf("Expected: 2,439,530,234,167 nodes (2.44 trillion)\n\n");

    // Build work items at depth 2
    struct WorkItem { GameState state; int remaining_depth; };
    std::vector<WorkItem> work;
    auto root_moves = generate_legal_moves(start);
    for (uint32_t m : root_moves) {
        GameState c1 = apply_ply_to_memory(start, m);
        auto c1_moves = generate_legal_moves(c1);
        for (uint32_t m2 : c1_moves) {
            work.push_back({apply_ply_to_memory(c1, m2), 7}); // depth 9 - 2 = 7
        }
    }
    printf("Work items: %zu (split at depth 2)\n\n", work.size());

    TTEntry* shared_tt = new TTEntry[TT_SIZE]();
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
    printf("Time:     %.2f seconds\n", elapsed);
    printf("Speed:    %.1fB NPS\n", nps / 1e9);
    printf("Result:   [%s]\n", nodes == expected ? "PASS" : "FAIL");

    return 0;
}
