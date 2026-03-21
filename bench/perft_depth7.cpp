// perft_depth7.cpp — Depth 7 perft for all positions with known values
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
    struct WorkItem { GameState state; int remaining_depth; };
    std::vector<WorkItem> work;
    auto root_moves = generate_legal_moves(state);
    for (uint32_t m : root_moves) {
        GameState c1 = apply_ply_to_memory(state, m);
        auto c1_moves = generate_legal_moves(c1);
        for (uint32_t m2 : c1_moves) {
            work.push_back({apply_ply_to_memory(c1, m2), depth - 2});
        }
    }
    TTEntry* shared_tt = new TTEntry[TT_SIZE]();
    std::vector<uint64_t> results(work.size(), 0);
    std::atomic<size_t> next_item{0};
    auto worker = [&]() {
        while (true) {
            size_t i = next_item.fetch_add(1, std::memory_order_relaxed);
            if (i >= work.size()) break;
            results[i] = perft(work[i].state, work[i].remaining_depth, shared_tt);
        }
    };
    std::vector<std::thread> threads;
    for (int t = 0; t < n_threads; t++) threads.emplace_back(worker);
    for (auto& t : threads) t.join();
    delete[] shared_tt;
    return std::accumulate(results.begin(), results.end(), 0ULL);
}

struct Test { const char* name; const char* fen; uint64_t expected; };

int main() {
    init_attack_tables();
    init_zobrist();
    int hw = std::thread::hardware_concurrency();

    // All positions with known depth 7 values
    // Source: https://www.chessprogramming.org/Perft_Results
    Test tests[] = {
        {"Starting position",       "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1",              3195901860ULL},
        {"Kiwipete",                "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1", 374190009323ULL},
        {"Position 3 (EP+promo)",   "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1",                             178633661ULL},
        {"Position 4 (disc. check)","r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1",     15833292ULL * 0 + 706045033ULL}, // d6 only, d7 unknown for this
        {"Position 5 (promotions)", "rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8",            3048196529ULL},
        {"Position 6 (Edwards)",    "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10", 6923051137ULL},
    };

    printf("Perft Depth 7 — All Standard Positions (%d threads)\n\n", hw);
    printf("%-25s %18s %18s %10s %10s %8s\n", "Position", "Nodes", "Expected", "Time(s)", "NPS", "Status");
    printf("─────────────────────────────────────────────────────────────────────────────────────────\n");

    int passed = 0, failed = 0;
    uint64_t total_nodes = 0;
    double total_time = 0;

    for (auto& t : tests) {
        // Skip Position 4 (no reliable depth 7 value)
        if (t.expected == 706045033ULL) {
            printf("%-25s %18s %18s %10s %10s %8s\n", t.name, "—", "—", "—", "—", "SKIP(d6)");
            continue;
        }

        GameState state = parse_fen(t.fen);
        auto t0 = Clock::now();
        uint64_t nodes = perft_mt(state, 7, hw);
        auto t1 = Clock::now();
        double elapsed = std::chrono::duration<double>(t1 - t0).count();
        double nps = nodes / elapsed;
        bool ok = (nodes == t.expected);
        if (ok) passed++; else failed++;
        total_nodes += nodes;
        total_time += elapsed;

        const char* unit = "B"; double nps_val = nps / 1e9;
        if (nps_val < 1) { nps_val = nps / 1e6; unit = "M"; }

        printf("%-25s %18llu %18llu %10.2f %8.1f%s  [%s]\n",
               t.name, nodes, t.expected, elapsed, nps_val, unit, ok ? "PASS" : "FAIL");
        fflush(stdout);
    }

    printf("─────────────────────────────────────────────────────────────────────────────────────────\n");
    printf("%-25s %18llu %18s %10.2f %8.1fB\n",
           "TOTAL", total_nodes, "", total_time, total_nodes / total_time / 1e9);
    printf("\n%d passed, %d failed\n", passed, failed);

    return 0;
}
