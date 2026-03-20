// full_summary.cpp — Complete perft summary across all depths and thread counts
// With TT + bulk counting + work-stealing + prefetching.
#include "../src/chess_engine.h"
#include <cstdio>
#include <chrono>
#include <thread>
#include <vector>
#include <numeric>
#include <cstring>
#include <atomic>

using namespace chess;
using Clock = std::chrono::high_resolution_clock;

// TT entry: 24 bytes
struct TTEntry {
    uint64_t hash;
    uint64_t nodes;
    uint8_t  depth;
};

static constexpr size_t TT_SIZE = 1 << 22; // 4M entries = ~96MB
static constexpr size_t TT_MASK = TT_SIZE - 1;

static uint64_t perft(const GameState& state, int depth, TTEntry* tt) {
    if (depth == 0) return 1ULL;
    if (depth == 1) return static_cast<uint64_t>(count_legal_moves(state));

    // TT probe (use incremental hash from GameState)
    uint64_t h = state.hash;
    size_t idx = h & TT_MASK;
    if (tt[idx].hash == h && tt[idx].depth == depth) {
        return tt[idx].nodes;
    }

    uint32_t moves[256];
    int count = generate_legal_moves_fast(state, moves);
    uint64_t total = 0;
    for (int i = 0; i < count; i++) {
        GameState child = apply_ply_to_memory(state, moves[i]);
        // Prefetch the child's TT entry while we recurse
        __builtin_prefetch(&tt[child.hash & TT_MASK], 0, 1);
        total += perft(child, depth - 1, tt);
    }

    // TT store (depth-preferred: keep deeper entries)
    if (tt[idx].depth <= depth)
        tt[idx] = {h, total, static_cast<uint8_t>(depth)};
    return total;
}

// Wrapper that allocates TT once
static TTEntry g_tt[TT_SIZE]; // global TT for single-threaded

static uint64_t perft_st(const GameState& state, int depth) {
    std::memset(g_tt, 0, sizeof(g_tt));
    return perft(state, depth, g_tt);
}

static uint64_t perft_mt(const GameState& state, int depth, int n_threads) {
    if (depth <= 2) return perft_st(state, depth);

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
    std::atomic<size_t> next_item{0};

    auto worker = [&]() {
        TTEntry* local_tt = new TTEntry[TT_SIZE]();
        while (true) {
            size_t i = next_item.fetch_add(1, std::memory_order_relaxed);
            if (i >= work.size()) break;
            results[i] = perft(work[i].state, work[i].remaining_depth, local_tt);
        }
        delete[] local_tt;
    };

    std::vector<std::thread> threads;
    for (int t = 0; t < n_threads; t++) threads.emplace_back(worker);
    for (auto& t : threads) t.join();
    return std::accumulate(results.begin(), results.end(), 0ULL);
}

struct Position {
    const char* name;
    const char* fen;
    int max_depth;
    uint64_t expected[8];
};

int main() {
    init_attack_tables();
    int hw = std::thread::hardware_concurrency();

    Position positions[] = {
        {"Starting position",
         "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", 7,
         {20, 400, 8902, 197281, 4865609, 119060324, 3195901860ULL}},
        {"Kiwipete",
         "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1", 6,
         {48, 2039, 97862, 4085603, 193690690, 8031647685ULL, 0}},
        {"Position 3 (EP+promo)",
         "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1", 7,
         {14, 191, 2812, 43238, 674624, 11030083, 178633661}},
        {"Position 4 (discovered checks)",
         "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1", 6,
         {6, 264, 9467, 422333, 15833292, 706045033, 0}},
        {"Position 5 (promotions)",
         "rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8", 5,
         {44, 1486, 62379, 2103487, 89941194, 0, 0}},
        {"Position 6 (Edwards)",
         "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10", 5,
         {46, 2079, 89890, 3894594, 164075551, 0, 0}},
    };

    printf("=== SINGLE-THREADED ===\n");
    for (auto& pos : positions) {
        GameState state = parse_fen(pos.fen);
        printf("\n  %s\n", pos.name);
        printf("  %-8s %15s %10s %12s %8s\n", "Depth", "Nodes", "Time(s)", "NPS", "Status");
        printf("  -----------------------------------------------------------\n");
        for (int d = 1; d <= pos.max_depth; d++) {
            if (pos.expected[d-1] == 0) break;
            auto t0 = Clock::now();
            uint64_t nodes = perft_st(state, d);
            auto t1 = Clock::now();
            double elapsed = std::chrono::duration<double>(t1 - t0).count();
            double nps = (elapsed > 0.0001) ? nodes / elapsed : 0;
            bool ok = (nodes == pos.expected[d-1]);
            printf("  %-8d %15llu %10.3f %11.1fM  [%s]\n",
                   d, nodes, elapsed, nps/1e6, ok ? "PASS" : "FAIL");
            fflush(stdout);
            if (elapsed > 30.0) { printf("  (stopping)\n"); break; }
        }
    }

    printf("\n=== MULTI-THREADED (%d threads) ===\n\n", hw);
    printf("  %-20s %5s %15s %10s %12s %8s\n", "Position", "Depth", "Nodes", "Time(s)", "NPS", "Status");
    printf("  --------------------------------------------------------------------------\n");

    struct MTTest { const char* name; const char* fen; int depth; uint64_t expected; };
    MTTest mt[] = {
        {"Starting pos", "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", 6, 119060324},
        {"Starting pos", "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", 7, 3195901860ULL},
        {"Kiwipete",     "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1", 5, 193690690},
        {"Kiwipete",     "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1", 6, 8031647685ULL},
        {"Position 4",   "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1", 6, 706045033},
        {"Position 5",   "rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8", 5, 89941194},
        {"Position 6",   "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10", 5, 164075551},
    };

    for (auto& t : mt) {
        GameState state = parse_fen(t.fen);
        auto t0 = Clock::now();
        uint64_t nodes = perft_mt(state, t.depth, hw);
        auto t1 = Clock::now();
        double elapsed = std::chrono::duration<double>(t1 - t0).count();
        double nps = (elapsed > 0.0001) ? nodes / elapsed : 0;
        bool ok = (nodes == t.expected);
        printf("  %-20s %5d %15llu %10.3f %11.1fM  [%s]\n",
               t.name, t.depth, nodes, elapsed, nps/1e6, ok ? "PASS" : "FAIL");
        fflush(stdout);
    }

    return 0;
}
