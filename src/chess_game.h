// chess_game.h — Game registry and clock state for managed games.
#ifndef CHESS_GAME_H
#define CHESS_GAME_H

#include "chess_engine.h"
#include <string>
#include <vector>
#include <unordered_map>
#include <cstdint>

namespace chess {

// ============================================================
// Registry enums
// ============================================================

enum class GameStatus : uint8_t {
    Waiting  = 0,
    Active   = 1,
    Finished = 2
};

enum class GameResult : uint8_t {
    InProgress = 0,
    WhiteWins  = 1,
    BlackWins  = 2,
    Draw       = 3
};

enum class Termination : uint8_t {
    Checkmate            = 0,
    Stalemate            = 1,
    Resignation          = 2,
    Timeout              = 3,
    DrawByAgreement      = 4,
    ThreefoldRepetition  = 5,
    FiftyMoveRule        = 6,
    SeventyFiveMoveRule  = 7,
    InsufficientMaterial = 8,
    FivefoldRepetition   = 9,
    Cancelled            = 10
};

enum class SettlementMode : uint8_t {
    CASUAL          = 0,
    FULL_FIDE_RULES = 1
};

enum class TimeMode : uint8_t {
    NONE      = 0,
    FISCHER   = 1,
    BRONSTEIN = 2
};

// ============================================================
// GameInfo — metadata exposed through the registry API
// ============================================================

struct GameInfo {
    std::string white;           // player name (replaces address)
    std::string black;
    GameStatus status     = GameStatus::Waiting;
    GameResult result     = GameResult::InProgress;
    std::string winner;
    double lastPlyTime    = 0;   // seconds since epoch (replaces uint40)
    double plyTimeLimit   = 0;
    SettlementMode settlementMode = SettlementMode::CASUAL;
    Termination termination = Termination::Checkmate;
};

// ============================================================
// PackedGameState — same bitboard state + clock info
// ============================================================

struct PackedGameState {
    GameState state;             // the chess position (bitboards etc.)
    double whiteTime    = 0;     // remaining seconds
    double blackTime    = 0;
    double increment    = 0;
    TimeMode timeMode   = TimeMode::NONE;
    uint8_t drawOfferSide = 0;  // 0=none, 1=white offered, 2=black offered
};

// ============================================================
// Game — combines info + state + history
// ============================================================

struct Game {
    GameInfo info;
    PackedGameState packed;
    std::vector<uint16_t> plyHistory;
    std::unordered_map<uint64_t, uint8_t> repetitionCount;
    std::string creator;
};

// ============================================================
// GameRegistry — manages all games
// ============================================================

class GameRegistry {
public:
    GameRegistry() = default;

    // Create a new game, returns game_id
    uint32_t create_game(
        const std::string& creator,
        SettlementMode settlement_mode,
        double ply_time_limit,
        double initial_time = 0,
        double increment = 0,
        TimeMode time_mode = TimeMode::NONE,
        bool as_black = false
    );

    // Create from custom FEN
    uint32_t create_game_from_fen(
        const std::string& creator,
        const std::string& fen,
        SettlementMode settlement_mode,
        double ply_time_limit,
        double initial_time = 0,
        double increment = 0,
        TimeMode time_mode = TimeMode::NONE,
        bool as_black = false
    );

    // Join an existing game
    bool join_game(uint32_t game_id, const std::string& player);

    // Make a ply (validates legality, updates state, checks settlement)
    // Returns true if ply was applied successfully
    bool make_ply(uint32_t game_id, const std::string& player, uint32_t ply, bool settle = false);

    // Make a ply via UCI string
    bool make_ply_uci(uint32_t game_id, const std::string& player, const std::string& uci, bool settle = false);

    // Cancel a waiting game
    bool cancel_game(uint32_t game_id, const std::string& player);

    // Resign
    bool resign(uint32_t game_id, const std::string& player);

    // Draw operations
    bool offer_draw(uint32_t game_id, const std::string& player);
    bool accept_draw(uint32_t game_id, const std::string& player);

    // Claim-based draws
    bool claim_checkmate(uint32_t game_id, const std::string& player);
    bool claim_insufficient_material(uint32_t game_id, const std::string& player);
    bool claim_fifty_move(uint32_t game_id, const std::string& player);
    bool claim_threefold(uint32_t game_id, const std::string& player);
    bool claim_time(uint32_t game_id, const std::string& player, double current_time);

    // Query
    const Game* get_game(uint32_t game_id) const;
    std::string get_fen(uint32_t game_id) const;
    std::vector<std::string> get_ply_history_uci(uint32_t game_id) const;
    uint32_t game_count() const { return next_game_id_ - 1; }

private:
    std::unordered_map<uint32_t, Game> games_;
    uint32_t next_game_id_ = 1;

    void settle_winner(uint32_t game_id, const std::string& winner, Termination t);
    void settle_draw(uint32_t game_id, Termination t);
    void finalize_after_ply(uint32_t game_id, bool settle);
    uint8_t record_repetition(uint32_t game_id);
};

} // namespace chess

#endif // CHESS_GAME_H
