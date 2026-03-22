// search_bench.cpp — Repeatable search benchmark for tracking optimization progress
// Compile: g++ -O3 -std=c++17 -march=native -o search_bench bench/search_bench.cpp src/chess_engine.cpp
#include "../src/chess_engine.h"
#include <cstdio>
#include <chrono>

using namespace chess;
using Clock = std::chrono::high_resolution_clock;

struct BenchPos {
    const char* name;
    const char* fen;
    int depth;
};

int main() {
    init_attack_tables();
    init_zobrist();

    BenchPos positions[] = {
        {"Starting pos",     "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1",                    7},
        {"Kiwipete",         "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1",       6},
        {"Italian",          "r1bq1rk1/ppp2ppp/2np1n2/2b1p3/2B1P3/2NP1N2/PPP2PPP/R1BQ1RK1 w - - 0 7",      6},
        {"Mate in 2",        "2bqkbn1/2pppp2/np2N3/r3P1p1/p2N2B1/5Q2/PPPPPP1P/RNB1K2R w KQ - 0 1",          6},
        {"Endgame",          "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1",                                    8},
    };

    printf("Search Benchmark\n");
    printf("================\n\n");
    printf("%-16s %5s %8s %12s %10s %8s\n", "Position", "Depth", "Move", "Nodes", "Time(s)", "NPS");
    printf("──────────────────────────────────────────────────────────────────────\n");

    uint64_t total_nodes = 0;
    double total_time = 0;

    for (auto& p : positions) {
        GameState state = parse_fen(p.fen);
        auto t0 = Clock::now();
        SearchResult r = find_best_move(state, p.depth);
        auto t1 = Clock::now();
        double elapsed = std::chrono::duration<double>(t1 - t0).count();
        double nps = r.nodes / elapsed;

        total_nodes += r.nodes;
        total_time += elapsed;

        const char* unit = "M"; double nps_val = nps / 1e6;
        if (nps_val >= 1000) { nps_val = nps / 1e9; unit = "B"; }

        printf("%-16s %5d %8s %12llu %10.3f %6.1f%s\n",
               p.name, p.depth, ply_to_uci(r.best_move).c_str(),
               r.nodes, elapsed, nps_val, unit);
        fflush(stdout);
    }

    printf("──────────────────────────────────────────────────────────────────────\n");
    printf("%-16s %5s %8s %12llu %10.3f %6.1fM\n",
           "TOTAL", "", "", total_nodes, total_time, total_nodes / total_time / 1e6);

    return 0;
}
