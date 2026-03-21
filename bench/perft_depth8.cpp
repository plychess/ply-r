// perft_depth8.cpp — Depth 8 tests for positions with published values
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

static uint64_t perft_mt(const GameState& state, int depth, int n_threads) {
    struct WorkItem { GameState state; int rem; };
    std::vector<WorkItem> work;
    auto r = generate_legal_moves(state);
    for (auto m : r) {
        GameState c1 = apply_ply_to_memory(state, m);
        auto r2 = generate_legal_moves(c1);
        for (auto m2 : r2) work.push_back({apply_ply_to_memory(c1, m2), depth - 2});
    }
    TTEntry* tt = new TTEntry[TT_SIZE]();
    std::vector<uint64_t> results(work.size(), 0);
    std::atomic<size_t> next{0};
    auto worker = [&]() {
        while (true) {
            size_t i = next.fetch_add(1, std::memory_order_relaxed);
            if (i >= work.size()) break;
            results[i] = perft(work[i].state, work[i].rem, tt);
        }
    };
    std::vector<std::thread> threads;
    for (int t = 0; t < n_threads; t++) threads.emplace_back(worker);
    for (auto& t : threads) t.join();
    delete[] tt;
    return std::accumulate(results.begin(), results.end(), 0ULL);
}

struct Test { const char* name; const char* fen; int depth; uint64_t expected; };

int main() {
    init_attack_tables();
    init_zobrist();
    int hw = std::thread::hardware_concurrency();

    Test tests[] = {
        {"Starting position d8", "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", 8, 84998978956ULL},
        {"Position 3 d8",        "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1",                 8, 3009794393ULL},
    };

    printf("Perft Depth 8 — Published Positions (%d threads)\n\n", hw);
    printf("%-22s %18s %18s %10s %10s %8s\n", "Position", "Nodes", "Expected", "Time(s)", "NPS", "Status");
    printf("──────────────────────────────────────────────────────────────────────────────────\n");

    int passed = 0, failed = 0;
    uint64_t total_nodes = 0;
    double total_time = 0;

    for (auto& t : tests) {
        GameState state = parse_fen(t.fen);
        auto t0 = Clock::now();
        uint64_t nodes = perft_mt(state, t.depth, hw);
        auto t1 = Clock::now();
        double elapsed = std::chrono::duration<double>(t1 - t0).count();
        double nps = nodes / elapsed;
        bool ok = (nodes == t.expected);
        if (ok) passed++; else failed++;
        total_nodes += nodes;
        total_time += elapsed;

        printf("%-22s %18llu %18llu %10.2f %8.1fB  [%s]\n",
               t.name, nodes, t.expected, elapsed, nps/1e9, ok ? "PASS" : "FAIL");
        fflush(stdout);
    }

    printf("──────────────────────────────────────────────────────────────────────────────────\n");
    printf("%-22s %18llu %18s %10.2f %8.1fB\n",
           "TOTAL", total_nodes, "", total_time, total_nodes/total_time/1e9);
    printf("\n%d passed, %d failed\n", passed, failed);
    return 0;
}
