// engine_match.cpp — Play Ply engine vs Stockfish via UCI pipes
// Build: g++ -O3 -std=c++17 -march=native -pthread -o engine_match bench/engine_match.cpp src/chess_engine.cpp
// Run:   ./engine_match [--stockfish /path/to/stockfish] [--games N] [--depth D] [--sf-depth D]
//
// This is a testing tool only — not part of the ply R package.

#include "../src/chess_engine.h"
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <string>
#include <vector>
#include <chrono>
#include <sstream>
#include <unistd.h>
#include <sys/wait.h>

using namespace chess;
using Clock = std::chrono::high_resolution_clock;

// ---- UCI pipe to external engine ----

struct UCIEngine {
    FILE* to_engine;
    FILE* from_engine;
    std::string name;

    bool open(const char* path) {
        // bidirectional pipe
        std::string cmd = std::string(path) + " 2>/dev/null";
        // Use popen2-style: we need both read and write
        int to_child[2], from_child[2];
        if (pipe(to_child) != 0 || pipe(from_child) != 0) return false;

        pid_t pid = fork();
        if (pid < 0) return false;

        if (pid == 0) {
            // Child: run stockfish
            close(to_child[1]);
            close(from_child[0]);
            dup2(to_child[0], STDIN_FILENO);
            dup2(from_child[1], STDOUT_FILENO);
            close(to_child[0]);
            close(from_child[1]);
            execl(path, path, nullptr);
            _exit(1);
        }

        // Parent
        close(to_child[0]);
        close(from_child[1]);
        to_engine = fdopen(to_child[1], "w");
        from_engine = fdopen(from_child[0], "r");
        if (!to_engine || !from_engine) return false;

        // Init UCI
        send("uci");
        wait_for("uciok");
        send("isready");
        wait_for("readyok");
        return true;
    }

    void send(const std::string& cmd) {
        fprintf(to_engine, "%s\n", cmd.c_str());
        fflush(to_engine);
    }

    std::string read_line() {
        char buf[4096];
        if (fgets(buf, sizeof(buf), from_engine)) {
            std::string s(buf);
            while (!s.empty() && (s.back() == '\n' || s.back() == '\r')) s.pop_back();
            return s;
        }
        return "";
    }

    void wait_for(const std::string& token) {
        while (true) {
            std::string line = read_line();
            if (line.find(token) != std::string::npos) break;
        }
    }

    std::string get_bestmove(const std::string& position_cmd, int depth) {
        send(position_cmd);
        send("go depth " + std::to_string(depth));
        while (true) {
            std::string line = read_line();
            if (line.substr(0, 8) == "bestmove") {
                // Extract move
                std::istringstream iss(line);
                std::string token, move;
                iss >> token >> move; // "bestmove e2e4 ..."
                return move;
            }
        }
    }

    void new_game() {
        send("ucinewgame");
        send("isready");
        wait_for("readyok");
    }

    void close_engine() {
        send("quit");
        if (to_engine) fclose(to_engine);
        if (from_engine) fclose(from_engine);
    }
};

// ---- Game result ----

struct GameResult {
    std::string result;       // "1-0", "0-1", "1/2-1/2"
    std::string termination;  // "checkmate", "stalemate", "50-move", etc.
    int moves;
    double time_secs;
    std::string move_list;    // space-separated UCI moves
};

// ---- Play one game ----

static GameResult play_game(UCIEngine& sf, int ply_depth, int sf_depth,
                             bool ply_is_white, int max_moves) {
    GameResult gr;
    gr.moves = 0;

    init_attack_tables();
    init_zobrist();

    GameState state;
    init_game(state);

    std::vector<std::string> moves;
    sf.new_game();

    auto t0 = Clock::now();

    for (int ply = 0; ply < max_moves * 2; ply++) {
        // Check game over
        auto legal = has_any_legal_ply_with_check(state);
        if (!legal.hasLegal) {
            if (legal.isInCheck) {
                gr.result = (state.sideToMove == COLOR_WHITE) ? "0-1" : "1-0";
                gr.termination = "checkmate";
            } else {
                gr.result = "1/2-1/2";
                gr.termination = "stalemate";
            }
            break;
        }

        if (state.halfMoveClock >= 100) {
            gr.result = "1/2-1/2";
            gr.termination = "50-move";
            break;
        }

        if (is_insufficient_material(state)) {
            gr.result = "1/2-1/2";
            gr.termination = "insufficient";
            break;
        }

        // Build position command
        std::string pos_cmd = "position startpos";
        if (!moves.empty()) {
            pos_cmd += " moves";
            for (auto& m : moves) pos_cmd += " " + m;
        }

        std::string uci_move;
        bool is_ply_turn = (state.sideToMove == COLOR_WHITE) == ply_is_white;

        if (is_ply_turn) {
            // Our engine's turn
            SearchResult r = find_best_move(state, ply_depth);
            if (r.best_move == 0) {
                gr.result = "1/2-1/2";
                gr.termination = "no-move";
                break;
            }
            uci_move = ply_to_uci(r.best_move);
        } else {
            // Stockfish's turn
            uci_move = sf.get_bestmove(pos_cmd, sf_depth);
        }

        // Apply move
        uint32_t move = uci_to_ply(state, uci_move);
        if (move == 0) {
            gr.result = is_ply_turn ? "0-1" : "1-0";
            gr.termination = "illegal-move";
            break;
        }

        state = apply_ply_to_memory(state, move);
        moves.push_back(uci_move);

        if (state.sideToMove == COLOR_WHITE) gr.moves++;
    }

    if (gr.result.empty()) {
        gr.result = "1/2-1/2";
        gr.termination = "max-moves";
    }

    auto t1 = Clock::now();
    gr.time_secs = std::chrono::duration<double>(t1 - t0).count();

    // Build move list string
    for (size_t i = 0; i < moves.size(); i++) {
        if (i > 0) gr.move_list += " ";
        if (i % 2 == 0) gr.move_list += std::to_string(i / 2 + 1) + ".";
        gr.move_list += moves[i];
    }

    return gr;
}

// ---- Main ----

int main(int argc, char** argv) {
    const char* sf_path = "/opt/homebrew/bin/stockfish";
    int n_games = 10;
    int ply_depth = 8;
    int sf_depth = 8;
    int max_moves = 150;

    // Parse args
    for (int i = 1; i < argc; i++) {
        if (strcmp(argv[i], "--stockfish") == 0 && i + 1 < argc) sf_path = argv[++i];
        else if (strcmp(argv[i], "--games") == 0 && i + 1 < argc) n_games = atoi(argv[++i]);
        else if (strcmp(argv[i], "--depth") == 0 && i + 1 < argc) ply_depth = atoi(argv[++i]);
        else if (strcmp(argv[i], "--sf-depth") == 0 && i + 1 < argc) sf_depth = atoi(argv[++i]);
        else if (strcmp(argv[i], "--max-moves") == 0 && i + 1 < argc) max_moves = atoi(argv[++i]);
    }

    init_attack_tables();
    init_zobrist();

    printf("=== Ply vs Stockfish Match ===\n\n");
    printf("  Ply depth:       %d\n", ply_depth);
    printf("  Stockfish depth: %d\n", sf_depth);
    printf("  Games:           %d\n", n_games);
    printf("  Stockfish path:  %s\n\n", sf_path);

    UCIEngine sf;
    if (!sf.open(sf_path)) {
        fprintf(stderr, "Error: could not open Stockfish at %s\n", sf_path);
        return 1;
    }

    int ply_wins = 0, sf_wins = 0, draws = 0;

    printf("%-6s %-8s %-10s %-12s %6s  %s\n",
           "Game", "Ply", "Result", "Termination", "Moves", "Time");
    printf("──────────────────────────────────────────────────────────\n");

    for (int g = 0; g < n_games; g++) {
        bool ply_is_white = (g % 2 == 0);
        GameResult gr = play_game(sf, ply_depth, sf_depth, ply_is_white, max_moves);

        const char* ply_color = ply_is_white ? "White" : "Black";

        // Determine who won from Ply's perspective
        bool ply_won = (ply_is_white && gr.result == "1-0") ||
                       (!ply_is_white && gr.result == "0-1");
        bool sf_won  = (ply_is_white && gr.result == "0-1") ||
                       (!ply_is_white && gr.result == "1-0");

        if (ply_won) ply_wins++;
        else if (sf_won) sf_wins++;
        else draws++;

        printf("%-6d %-8s %-10s %-12s %6d  %.1fs\n",
               g + 1, ply_color, gr.result.c_str(), gr.termination.c_str(),
               gr.moves, gr.time_secs);
        fflush(stdout);
    }

    printf("──────────────────────────────────────────────────────────\n\n");
    printf("Final Score:\n");
    printf("  Ply:       %d wins\n", ply_wins);
    printf("  Stockfish: %d wins\n", sf_wins);
    printf("  Draws:     %d\n", draws);
    printf("  Ply score: %.1f / %d (%.0f%%)\n",
           ply_wins + 0.5 * draws, n_games,
           100.0 * (ply_wins + 0.5 * draws) / n_games);

    sf.close_engine();
    return 0;
}
