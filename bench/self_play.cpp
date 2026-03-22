// self_play.cpp — Engine plays against itself from various positions
// Build: g++ -O3 -std=c++17 -march=native -pthread -o self_play bench/self_play.cpp src/chess_engine.cpp
#include "../src/chess_engine.h"
#include <cstdio>
#include <chrono>
#include <string>

using namespace chess;
using Clock = std::chrono::high_resolution_clock;

struct GameRecord {
    int moves;
    const char* result;
    double total_time;
};

static GameRecord play_game(const GameState& start, int depth, int max_moves) {
    GameState state = start;
    printf("  ");

    auto t0 = Clock::now();
    int move_num = 0;

    for (int ply = 0; ply < max_moves * 2; ply++) {
        // Check for game over
        auto legal = has_any_legal_ply_with_check(state);
        if (!legal.hasLegal) {
            auto t1 = Clock::now();
            double elapsed = std::chrono::duration<double>(t1 - t0).count();
            if (legal.isInCheck) {
                const char* winner = (state.sideToMove == COLOR_WHITE) ? "0-1" : "1-0";
                printf(" %s (checkmate, move %d)\n", winner, move_num);
                return {move_num, winner, elapsed};
            } else {
                printf(" 1/2-1/2 (stalemate, move %d)\n", move_num);
                return {move_num, "1/2-1/2", elapsed};
            }
        }

        // Check draws
        if (state.halfMoveClock >= 100) {
            auto t1 = Clock::now();
            printf(" 1/2-1/2 (50-move rule, move %d)\n", move_num);
            return {move_num, "1/2-1/2", std::chrono::duration<double>(t1 - t0).count()};
        }
        if (is_insufficient_material(state)) {
            auto t1 = Clock::now();
            printf(" 1/2-1/2 (insufficient material, move %d)\n", move_num);
            return {move_num, "1/2-1/2", std::chrono::duration<double>(t1 - t0).count()};
        }

        // Search
        SearchResult r = find_best_move(state, depth);
        if (r.best_move == 0) break;

        // Print move
        if (state.sideToMove == COLOR_WHITE) {
            move_num++;
            if (move_num > 1) printf(" ");
            printf("%d.", move_num);
        }
        printf("%s", ply_to_uci(r.best_move).c_str());

        // Apply
        state = apply_ply_to_memory(state, r.best_move);
    }

    auto t1 = Clock::now();
    printf(" * (max moves reached)\n");
    return {move_num, "*", std::chrono::duration<double>(t1 - t0).count()};
}

int main() {
    init_attack_tables();
    init_zobrist();

    struct TestGame {
        const char* name;
        const char* fen;
        int depth;
        int max_moves;
    };

    TestGame games[] = {
        {"Game 1: Starting pos (d8)",
         "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", 8, 80},

        {"Game 2: Starting pos (d6, fast)",
         "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", 6, 100},

        {"Game 3: Italian Game (d8)",
         "r1bqkbnr/pppp1ppp/2n5/4p3/2B1P3/5N2/PPPP1PPP/RNBQK2R w KQkq - 2 3", 8, 60},

        {"Game 4: Queen's Gambit (d6)",
         "rnbqkbnr/ppp1pppp/8/3p4/2PP4/8/PP2PPPP/RNBQKBNR b KQkq c3 0 2", 6, 100},

        {"Game 5: Endgame KRK (d10)",
         "4k3/8/8/8/8/8/8/4K2R w - - 0 1", 10, 80},
    };

    printf("=== Ply Engine Self-Play ===\n\n");

    int wins_w = 0, wins_b = 0, draws = 0;

    for (auto& g : games) {
        printf("%s\n", g.name);
        GameState state = parse_fen(g.fen);
        GameRecord rec = play_game(state, g.depth, g.max_moves);
        printf("  Time: %.1fs  |  Moves: %d  |  Result: %s\n\n", rec.total_time, rec.moves, rec.result);

        if (std::string(rec.result) == "1-0") wins_w++;
        else if (std::string(rec.result) == "0-1") wins_b++;
        else draws++;
    }

    printf("Summary: White +%d  Black +%d  Draws %d\n", wins_w, wins_b, draws);
    return 0;
}
