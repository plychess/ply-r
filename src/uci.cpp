// uci.cpp — UCI protocol interface for the ply chess engine
// Build: g++ -O3 -std=c++17 -march=native -pthread -o ply-engine src/uci.cpp src/chess_engine.cpp
//
// Usage: ./ply-engine
// Then connect via a UCI-compatible GUI (Arena, CuteChess, etc.)

#include "chess_engine.h"
#include <iostream>
#include <sstream>
#include <string>
#include <chrono>
#include <thread>
#include <atomic>

using namespace chess;
using Clock = std::chrono::high_resolution_clock;

static GameState position;
static std::atomic<bool> searching{false};

// Parse "position" command: position startpos [moves e2e4 e7e5 ...]
//                           position fen <fen> [moves e2e4 ...]
static void cmd_position(std::istringstream& iss) {
    std::string token;
    iss >> token;

    if (token == "startpos") {
        init_game(position);
        iss >> token; // consume "moves" if present
    } else if (token == "fen") {
        std::string fen;
        // FEN has 6 fields separated by spaces
        for (int i = 0; i < 6 && iss >> token; i++) {
            if (i > 0) fen += " ";
            fen += token;
            if (token == "moves") { fen.resize(fen.size() - 6); break; } // "moves" is not part of FEN
        }
        position = parse_fen(fen.c_str());
        // Check if next token is "moves"
        if (token != "moves") iss >> token;
    }

    // Apply moves
    if (token == "moves") {
        while (iss >> token) {
            uint32_t move = uci_to_ply(position, token.c_str());
            if (move != 0) {
                position = apply_ply_to_memory(position, move);
            }
        }
    }
}

// Parse "go" command and search
static void cmd_go(std::istringstream& iss) {
    int depth = 64; // default: search very deep
    int movetime = 0; // ms
    int wtime = 0, btime = 0, winc = 0, binc = 0;
    int movestogo = 30;
    bool infinite = false;

    std::string token;
    while (iss >> token) {
        if (token == "depth") iss >> depth;
        else if (token == "movetime") iss >> movetime;
        else if (token == "wtime") iss >> wtime;
        else if (token == "btime") iss >> btime;
        else if (token == "winc") iss >> winc;
        else if (token == "binc") iss >> binc;
        else if (token == "movestogo") iss >> movestogo;
        else if (token == "infinite") infinite = true;
    }

    // Simple time management: use 1/30 of remaining time + increment
    if (!infinite && movetime == 0 && (wtime > 0 || btime > 0)) {
        int mytime = (position.sideToMove == COLOR_WHITE) ? wtime : btime;
        int myinc  = (position.sideToMove == COLOR_WHITE) ? winc : binc;
        movetime = mytime / movestogo + myinc / 2;
        if (movetime > mytime - 500) movetime = mytime - 500; // keep 500ms buffer
        if (movetime < 100) movetime = 100; // minimum 100ms
    }

    // If movetime is set, use iterative deepening with time limit
    if (movetime > 0 && depth == 64) {
        auto start = Clock::now();
        SearchResult best = {0, 0, 0, 0};

        for (int d = 1; d <= 64; d++) {
            SearchResult r = find_best_move(position, d);
            best = r;

            auto elapsed = std::chrono::duration_cast<std::chrono::milliseconds>(
                Clock::now() - start).count();

            // Print info
            std::cout << "info depth " << r.depth
                      << " score cp " << r.score
                      << " nodes " << r.nodes
                      << " time " << elapsed
                      << " pv " << ply_to_uci(r.best_move)
                      << std::endl;

            // Stop if we've used enough time or next depth likely won't finish
            if (elapsed >= movetime) break;
            if (elapsed * 4 >= movetime) break; // next depth probably takes 4x longer
        }

        std::cout << "bestmove " << ply_to_uci(best.best_move) << std::endl;
    } else {
        // Fixed depth search
        auto start = Clock::now();
        SearchResult r = find_best_move(position, depth);
        auto elapsed = std::chrono::duration_cast<std::chrono::milliseconds>(
            Clock::now() - start).count();

        std::cout << "info depth " << r.depth
                  << " score cp " << r.score
                  << " nodes " << r.nodes
                  << " time " << elapsed
                  << " pv " << ply_to_uci(r.best_move)
                  << std::endl;
        std::cout << "bestmove " << ply_to_uci(r.best_move) << std::endl;
    }
}

int main() {
    init_attack_tables();
    init_zobrist();
    init_game(position);

    std::string line;
    while (std::getline(std::cin, line)) {
        std::istringstream iss(line);
        std::string cmd;
        iss >> cmd;

        if (cmd == "uci") {
            std::cout << "id name Ply 1.0" << std::endl;
            std::cout << "id author Qusai Jouda" << std::endl;
            std::cout << "uciok" << std::endl;
        }
        else if (cmd == "isready") {
            std::cout << "readyok" << std::endl;
        }
        else if (cmd == "ucinewgame") {
            init_game(position);
        }
        else if (cmd == "position") {
            cmd_position(iss);
        }
        else if (cmd == "go") {
            cmd_go(iss);
        }
        else if (cmd == "eval") {
            int e = evaluate(position);
            std::cout << "info string eval " << e << " cp (white perspective)" << std::endl;
        }
        else if (cmd == "d") {
            // Display position
            std::cout << "info string fen " << state_to_fen(position) << std::endl;
        }
        else if (cmd == "quit") {
            break;
        }
    }

    return 0;
}
