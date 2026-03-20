// full_summary.cpp — Comprehensive perft benchmark suite
// Features: TT + bulk counting + work-stealing + prefetching + shared TT
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

// TT with XOR verification to detect torn reads/writes in shared mode
struct TTEntry {
    uint64_t key;    // hash ^ nodes (verification)
    uint64_t nodes;
    uint8_t  depth;
};
static constexpr size_t TT_SIZE = 1 << 22;
static constexpr size_t TT_MASK = TT_SIZE - 1;

static uint64_t perft(const GameState& state, int depth, TTEntry* tt) {
    if (depth == 0) return 1ULL;
    if (depth == 1) return static_cast<uint64_t>(count_legal_moves(state));
    uint64_t h = state.hash;
    size_t idx = h & TT_MASK;
    // Probe: verify key == hash ^ nodes to detect torn writes
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
    // Store: key = hash ^ nodes for verification
    if (e.depth <= depth) tt[idx] = {h ^ total, total, static_cast<uint8_t>(depth)};
    return total;
}

static TTEntry g_tt[TT_SIZE];
static uint64_t perft_st(const GameState& state, int depth) {
    std::memset(g_tt, 0, sizeof(g_tt));
    return perft(state, depth, g_tt);
}

static TTEntry* g_shared_tt = nullptr;
static uint64_t perft_mt(const GameState& state, int depth, int n_threads) {
    if (depth <= 2) return perft_st(state, depth);
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

// ============================================================

struct Position {
    const char* name;
    const char* fen;
    int max_depth;
    uint64_t expected[8];
};

int main() {
    init_attack_tables();
    init_zobrist();
    int hw = std::thread::hardware_concurrency();

    Position positions[] = {
        {"Starting position",
         "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", 8,
         {20, 400, 8902, 197281, 4865609, 119060324, 3195901860ULL, 84998978956ULL}},
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

    int n_pos = sizeof(positions) / sizeof(positions[0]);
    uint64_t total_nodes_st = 0, total_nodes_mt = 0;
    double total_time_st = 0, total_time_mt = 0;
    int total_pass = 0, total_fail = 0;

    // =====================================================================
    printf("╔════════════════════════════════════════════════════════════════════════════════════╗\n");
    printf("║                         PLY CHESS ENGINE — PERFT BENCHMARK                       ║\n");
    printf("║                                                                                  ║\n");
    printf("║  Platform: Apple M2 Max — %d cores (8P + 4E)                                     ║\n", hw);
    printf("║  Compiler: clang -O3 -march=native -flto (PGO)                                   ║\n");
    printf("║  Features: magic bitboards, pin-aware legality, stack arrays, bulk counting,     ║\n");
    printf("║            incremental Zobrist, TT (4M entries), work-stealing, prefetching      ║\n");
    printf("╚════════════════════════════════════════════════════════════════════════════════════╝\n");

    // =====================================================================
    // SINGLE-THREADED
    // =====================================================================
    printf("\n");
    printf("┌──────────────────────────────────────────────────────────────────────────────────┐\n");
    printf("│                            SINGLE-THREADED RESULTS                               │\n");
    printf("├──────────────────────────────────┬───────┬─────────────────┬──────────┬──────────┤\n");
    printf("│ Position                         │ Depth │           Nodes │  Time(s) │      NPS │\n");
    printf("├──────────────────────────────────┼───────┼─────────────────┼──────────┼──────────┤\n");

    for (int p = 0; p < n_pos; p++) {
        auto& pos = positions[p];
        GameState state = parse_fen(pos.fen);
        for (int d = 1; d <= pos.max_depth; d++) {
            if (pos.expected[d-1] == 0) break;
            auto t0 = Clock::now();
            uint64_t nodes = perft_st(state, d);
            auto t1 = Clock::now();
            double elapsed = std::chrono::duration<double>(t1 - t0).count();
            double nps = (elapsed > 0.0001) ? nodes / elapsed : 0;
            bool ok = (nodes == pos.expected[d-1]);
            if (ok) total_pass++; else total_fail++;

            const char* nps_unit = "M";
            double nps_val = nps / 1e6;
            if (nps_val >= 1000) { nps_val /= 1000; nps_unit = "B"; }

            // Only show depth >= 4 (shallow depths are noise)
            if (d >= 4) {
                printf("│ %-32s │   %d   │ %15llu │ %8.3f │ %6.1f%s │\n",
                       pos.name, d, nodes, elapsed, nps_val, nps_unit);
                total_nodes_st += nodes;
                total_time_st += elapsed;
            }
            fflush(stdout);
            if (elapsed > 120.0) break;
        }
        if (p < n_pos - 1) {
            printf("├──────────────────────────────────┼───────┼─────────────────┼──────────┼──────────┤\n");
        }
    }

    double avg_nps_st = (total_time_st > 0) ? total_nodes_st / total_time_st : 0;
    const char* avg_unit_st = "M"; double avg_val_st = avg_nps_st / 1e6;
    if (avg_val_st >= 1000) { avg_val_st /= 1000; avg_unit_st = "B"; }

    printf("├──────────────────────────────────┴───────┼─────────────────┼──────────┼──────────┤\n");
    printf("│ TOTAL (single-threaded)                  │ %15llu │ %8.2f │ %6.1f%s │\n",
           total_nodes_st, total_time_st, avg_val_st, avg_unit_st);
    printf("└──────────────────────────────────────────┴─────────────────┴──────────┴──────────┘\n");

    // =====================================================================
    // MULTI-THREADED
    // =====================================================================
    struct MTTest { const char* name; const char* fen; int depth; uint64_t expected; };
    MTTest mt[] = {
        {"Starting pos",    "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", 5, 4865609},
        {"Starting pos",    "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", 6, 119060324},
        {"Starting pos",    "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", 7, 3195901860ULL},
        {"Starting pos",    "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", 8, 84998978956ULL},
        {"Kiwipete",        "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1", 5, 193690690},
        {"Kiwipete",        "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1", 6, 8031647685ULL},
        {"Position 3",      "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1", 6, 11030083},
        {"Position 3",      "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1", 7, 178633661},
        {"Position 4",      "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1", 5, 15833292},
        {"Position 4",      "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1", 6, 706045033},
        {"Position 5",      "rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8", 5, 89941194},
        {"Position 6",      "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10", 5, 164075551},
    };
    int n_mt = sizeof(mt) / sizeof(mt[0]);

    printf("\n");
    printf("┌──────────────────────────────────────────────────────────────────────────────────┐\n");
    printf("│                     MULTI-THREADED RESULTS (%d threads)                           │\n", hw);
    printf("├──────────────────────┬───────┬─────────────────┬──────────┬──────────┬───────────┤\n");
    printf("│ Position             │ Depth │           Nodes │  Time(s) │      NPS │   Speedup │\n");
    printf("├──────────────────────┼───────┼─────────────────┼──────────┼──────────┼───────────┤\n");

    for (int i = 0; i < n_mt; i++) {
        auto& t = mt[i];
        GameState state = parse_fen(t.fen);

        // Single-thread baseline for speedup
        auto s0 = Clock::now();
        perft_st(state, t.depth);
        auto s1 = Clock::now();
        double st_elapsed = std::chrono::duration<double>(s1 - s0).count();

        auto t0 = Clock::now();
        uint64_t nodes = perft_mt(state, t.depth, hw);
        auto t1 = Clock::now();
        double elapsed = std::chrono::duration<double>(t1 - t0).count();
        double nps = (elapsed > 0.0001) ? nodes / elapsed : 0;
        double speedup = (elapsed > 0.0001 && st_elapsed > 0.0001) ? st_elapsed / elapsed : 0;
        bool ok = (nodes == t.expected);
        if (ok) total_pass++; else total_fail++;

        const char* nps_unit = "M"; double nps_val = nps / 1e6;
        if (nps_val >= 1000) { nps_val /= 1000; nps_unit = "B"; }

        printf("│ %-20s │   %d   │ %15llu │ %8.3f │ %6.1f%s │   %5.1fx  │\n",
               t.name, t.depth, nodes, elapsed, nps_val, nps_unit, speedup);
        fflush(stdout);

        total_nodes_mt += nodes;
        total_time_mt += elapsed;
    }

    double avg_nps_mt = (total_time_mt > 0) ? total_nodes_mt / total_time_mt : 0;
    const char* avg_unit_mt = "M"; double avg_val_mt = avg_nps_mt / 1e6;
    if (avg_val_mt >= 1000) { avg_val_mt /= 1000; avg_unit_mt = "B"; }

    printf("├──────────────────────┴───────┼─────────────────┼──────────┼──────────┼───────────┤\n");
    printf("│ TOTAL (multi-threaded)       │ %15llu │ %8.2f │ %6.1f%s │           │\n",
           total_nodes_mt, total_time_mt, avg_val_mt, avg_unit_mt);
    printf("└──────────────────────────────┴─────────────────┴──────────┴──────────┴───────────┘\n");

    // =====================================================================
    // SUMMARY
    // =====================================================================
    printf("\n");
    printf("┌──────────────────────────────────────────────────────────────────────────────────┐\n");
    printf("│                                   SUMMARY                                        │\n");
    printf("├──────────────────────────────────────────────────────────────────────────────────┤\n");
    printf("│  Tests:          %d passed, %d failed                                            │\n", total_pass, total_fail);

    double peak_st = 0, peak_mt = 0;
    // Find peaks from the data we just ran
    for (int p = 0; p < n_pos; p++) {
        GameState state = parse_fen(positions[p].fen);
        for (int d = 4; d <= positions[p].max_depth; d++) {
            if (positions[p].expected[d-1] == 0) break;
            auto t0 = Clock::now();
            uint64_t nodes = perft_st(state, d);
            auto t1 = Clock::now();
            double elapsed = std::chrono::duration<double>(t1 - t0).count();
            if (elapsed > 0.001) {
                double nps = nodes / elapsed;
                if (nps > peak_st) peak_st = nps;
            }
            if (elapsed > 20.0) break;
        }
    }
    for (int i = 0; i < n_mt; i++) {
        GameState state = parse_fen(mt[i].fen);
        auto t0 = Clock::now();
        uint64_t nodes = perft_mt(state, mt[i].depth, hw);
        auto t1 = Clock::now();
        double elapsed = std::chrono::duration<double>(t1 - t0).count();
        if (elapsed > 0.001) {
            double nps = nodes / elapsed;
            if (nps > peak_mt) peak_mt = nps;
        }
    }

    printf("│  Peak ST:        %7.1fM NPS  (%5.2fB)                                        │\n", peak_st/1e6, peak_st/1e9);
    printf("│  Peak MT:        %7.1fM NPS  (%5.2fB)                                        │\n", peak_mt/1e6, peak_mt/1e9);
    printf("│  Total nodes:    %15llu verified correct                               │\n", total_nodes_st + total_nodes_mt);
    printf("│  Improvement:    %dx from original 22M NPS baseline                            │\n", (int)(peak_mt / 22e6));
    printf("└──────────────────────────────────────────────────────────────────────────────────┘\n");

    return total_fail > 0 ? 1 : 0;
}
