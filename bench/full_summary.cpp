// full_summary.cpp — Complete perft summary across all depths and thread counts
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
    uint32_t moves[256];
    std::vector<uint32_t> mv = generate_legal_moves(state);
    if (depth == 1) return mv.size();
    int count = mv.size();
    uint64_t total = 0;
    for (int i = 0; i < count; i++) {
        GameState child = apply_ply_to_memory(state, mv[i]);
        total += perft(child, depth - 1);
    }
    return total;
}

static uint64_t perft_mt(const GameState& state, int depth, int n_threads) {
    if (depth <= 1) return perft(state, depth);
    std::vector<uint32_t> mv = generate_legal_moves(state);
    int n_moves = mv.size();
    std::vector<uint64_t> results(n_moves, 0);

    auto worker = [&](int tid) {
        for (int i = tid; i < n_moves; i += n_threads) {
            GameState child = apply_ply_to_memory(state, mv[i]);
            results[i] = perft(child, depth - 1);
        }
    };
    std::vector<std::thread> threads;
    for (int t = 0; t < n_threads; t++) threads.emplace_back(worker, t);
    for (auto& t : threads) t.join();
    return std::accumulate(results.begin(), results.end(), 0ULL);
}

struct Position {
    const char* name;
    const char* fen;
    int max_depth;
    uint64_t expected[8]; // depths 1-7
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

    // =====================================================================
    // Single-threaded results
    // =====================================================================
    printf("╔══════════════════════════════════════════════════════════════════════════════╗\n");
    printf("║                    SINGLE-THREADED PERFT RESULTS                            ║\n");
    printf("╠══════════════════════════════════════════════════════════════════════════════╣\n");

    for (auto& pos : positions) {
        GameState state = parse_fen(pos.fen);
        printf("\n  %s\n", pos.name);
        printf("  %-8s %15s %10s %12s %8s\n", "Depth", "Nodes", "Time(s)", "NPS", "Status");
        printf("  ──────────────────────────────────────────────────────────────\n");

        for (int d = 1; d <= pos.max_depth; d++) {
            if (pos.expected[d-1] == 0) break;
            auto t0 = Clock::now();
            uint64_t nodes = perft(state, d);
            auto t1 = Clock::now();
            double elapsed = std::chrono::duration<double>(t1 - t0).count();
            double nps = (elapsed > 0.0001) ? nodes / elapsed : 0;
            bool ok = (nodes == pos.expected[d-1]);
            printf("  %-8d %15llu %10.3f %11.1fM  [%s]\n",
                   d, nodes, elapsed, nps/1e6, ok ? "PASS" : "FAIL");
            fflush(stdout);
            if (elapsed > 30.0) { printf("  (stopping — time limit)\n"); break; }
        }
    }

    // =====================================================================
    // Multi-threaded results (depth 6 for key positions)
    // =====================================================================
    printf("\n╔══════════════════════════════════════════════════════════════════════════════╗\n");
    printf("║                   MULTI-THREADED PERFT RESULTS (%d threads)                 ║\n", hw);
    printf("╠══════════════════════════════════════════════════════════════════════════════╣\n");

    struct MTTest { const char* name; const char* fen; int depth; uint64_t expected; };
    MTTest mt_tests[] = {
        {"Starting pos d6", "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", 6, 119060324},
        {"Starting pos d7", "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", 7, 3195901860ULL},
        {"Kiwipete d5",     "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1", 5, 193690690},
        {"Kiwipete d6",     "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1", 6, 8031647685ULL},
        {"Position 4 d6",   "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1", 6, 706045033},
        {"Position 5 d5",   "rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8", 5, 89941194},
        {"Position 6 d5",   "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10", 5, 164075551},
    };

    printf("\n  %-20s %5s %15s %10s %12s %8s\n", "Position", "Depth", "Nodes", "Time(s)", "NPS", "Status");
    printf("  ────────────────────────────────────────────────────────────────────────────\n");

    for (auto& t : mt_tests) {
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

    printf("\n╚══════════════════════════════════════════════════════════════════════════════╝\n");
    return 0;
}
