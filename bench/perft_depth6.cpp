// perft_depth6.cpp — Perft depth 6 correctness test for all standard positions
// Uses bulk counting at depth 1 + depth-2 thread splitting.
//
// Compile:  g++ -O3 -std=c++17 -march=native -flto -pthread -o perft_depth6 bench/perft_depth6.cpp src/chess_engine.cpp
// Run:      ./perft_depth6

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
    if (e.depth <= depth)
        tt[idx] = {h ^ total, total, static_cast<uint8_t>(depth)};
    return total;
}

static TTEntry* g_shared_tt = nullptr;

static uint64_t perft_mt(const GameState& state, int depth, int n_threads) {
    if (depth <= 2) {
        TTEntry* tt = new TTEntry[TT_SIZE]();
        uint64_t r = perft(state, depth, tt);
        delete[] tt;
        return r;
    }

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

    if (!g_shared_tt) g_shared_tt = new TTEntry[TT_SIZE]();
    std::memset(g_shared_tt, 0, TT_SIZE * sizeof(TTEntry));

    std::vector<uint64_t> results(work.size(), 0);
    std::atomic<size_t> next_item{0};
    auto worker = [&]() {
        while (true) {
            size_t i = next_item.fetch_add(1, std::memory_order_relaxed);
            if (i >= work.size()) break;
            results[i] = perft(work[i].state, work[i].remaining_depth, g_shared_tt);
        }
    };

    std::vector<std::thread> threads;
    for (int t = 0; t < n_threads; t++) threads.emplace_back(worker);
    for (auto& t : threads) t.join();
    return std::accumulate(results.begin(), results.end(), 0ULL);
}

struct PerftTest {
    const char* name;
    const char* fen;
    int depth;
    uint64_t expected;
};

int main() {
    init_attack_tables();
    int n_threads = static_cast<int>(std::thread::hardware_concurrency());
    printf("Running perft depth 6 tests (%d threads)\n\n", n_threads);

    PerftTest tests[] = {
        {"Starting pos depth 5", "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", 5, 4865609ULL},
        {"Starting pos depth 6", "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", 6, 119060324ULL},
        {"Kiwipete depth 4",     "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1", 4, 4085603ULL},
        {"Kiwipete depth 5",     "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1", 5, 193690690ULL},
        {"Position 3 depth 5",   "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1", 5, 674624ULL},
        {"Position 3 depth 6",   "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1", 6, 11030083ULL},
        {"Position 4 depth 5",   "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1", 5, 15833292ULL},
        {"Position 4 depth 6",   "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1", 6, 706045033ULL},
        {"Position 5 depth 4",   "rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8", 4, 2103487ULL},
        {"Position 5 depth 5",   "rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8", 5, 89941194ULL},
        {"Position 6 depth 4",   "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10", 4, 3894594ULL},
        {"Position 6 depth 5",   "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10", 5, 164075551ULL},
    };

    int passed = 0, failed = 0;
    int n_tests = sizeof(tests) / sizeof(tests[0]);

    for (int i = 0; i < n_tests; i++) {
        auto& t = tests[i];
        GameState state = parse_fen(t.fen);
        auto t0 = Clock::now();
        uint64_t nodes = perft_mt(state, t.depth, n_threads);
        auto t1 = Clock::now();
        double elapsed = std::chrono::duration<double>(t1 - t0).count();
        double nps = (elapsed > 0) ? nodes / elapsed : 0;
        bool ok = (nodes == t.expected);
        printf("  %s%-30s%s  depth %d  nodes %15llu  expected %15llu  %6.2fs  %8.1fM NPS  [%s]\n",
               ok ? "" : "\033[31m", t.name, ok ? "" : "\033[0m",
               t.depth, nodes, t.expected, elapsed, nps / 1e6,
               ok ? "PASS" : "FAIL");
        fflush(stdout);
        if (ok) passed++; else failed++;
    }

    printf("\n%d passed, %d failed out of %d tests\n", passed, failed, n_tests);
    return failed > 0 ? 1 : 0;
}
