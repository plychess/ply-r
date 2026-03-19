// chess_game.cpp — Game registry implementation.
#include "chess_game.h"
#include <cmath>
#include <stdexcept>

namespace chess {

// ============================================================
// Create game
// ============================================================

uint32_t GameRegistry::create_game(
    const std::string& creator,
    SettlementMode settlement_mode,
    double ply_time_limit,
    double initial_time,
    double increment,
    TimeMode time_mode,
    bool as_black
) {
    uint32_t game_id = next_game_id_++;
    Game& g = games_[game_id];
    g.creator = creator;

    // Init standard position
    init_game(g.packed.state);

    // Set info
    if (as_black) {
        g.info.black = creator;
    } else {
        g.info.white = creator;
    }
    g.info.status = GameStatus::Waiting;
    g.info.result = GameResult::InProgress;
    g.info.plyTimeLimit = ply_time_limit;
    g.info.settlementMode = settlement_mode;

    // Clock
    g.packed.timeMode = time_mode;
    if (time_mode != TimeMode::NONE) {
        g.packed.whiteTime = initial_time;
        g.packed.blackTime = initial_time;
        g.packed.increment = increment;
    }

    // Record initial position for FIDE rules
    if (settlement_mode == SettlementMode::FULL_FIDE_RULES) {
        record_repetition(game_id);
    }

    return game_id;
}

uint32_t GameRegistry::create_game_from_fen(
    const std::string& creator,
    const std::string& fen,
    SettlementMode settlement_mode,
    double ply_time_limit,
    double initial_time,
    double increment,
    TimeMode time_mode,
    bool as_black
) {
    uint32_t game_id = next_game_id_++;
    Game& g = games_[game_id];
    g.creator = creator;

    g.packed.state = parse_fen(fen);
    if (!validate_position(g.packed.state))
        throw std::runtime_error("Invalid position");

    if (as_black) {
        g.info.black = creator;
    } else {
        g.info.white = creator;
    }
    g.info.status = GameStatus::Waiting;
    g.info.result = GameResult::InProgress;
    g.info.plyTimeLimit = ply_time_limit;
    g.info.settlementMode = settlement_mode;

    g.packed.timeMode = time_mode;
    if (time_mode != TimeMode::NONE) {
        g.packed.whiteTime = initial_time;
        g.packed.blackTime = initial_time;
        g.packed.increment = increment;
    }

    if (settlement_mode == SettlementMode::FULL_FIDE_RULES) {
        record_repetition(game_id);
    }

    return game_id;
}

// ============================================================
// Join game
// ============================================================

bool GameRegistry::join_game(uint32_t game_id, const std::string& player) {
    auto it = games_.find(game_id);
    if (it == games_.end()) return false;
    Game& g = it->second;
    if (g.info.status != GameStatus::Waiting) return false;
    if (g.info.white == player || g.info.black == player) return false;

    if (g.info.white.empty()) {
        g.info.white = player;
    } else if (g.info.black.empty()) {
        g.info.black = player;
    } else {
        return false;
    }

    g.info.status = GameStatus::Active;
    g.info.lastPlyTime = 0; // caller should set via current_time if needed
    return true;
}

// ============================================================
// Make ply — mirrors GameRegistry._makePlyInternal
// ============================================================

bool GameRegistry::make_ply(uint32_t game_id, const std::string& player, uint32_t ply, bool settle) {
    auto it = games_.find(game_id);
    if (it == games_.end()) return false;
    Game& g = it->second;
    if (g.info.status != GameStatus::Active) return false;

    uint8_t side = g.packed.state.sideToMove;
    const std::string& current =
        (side == COLOR_WHITE) ? g.info.white : g.info.black;
    if (player != current) return false;

    // Validate and apply
    if (!is_pseudo_legal_ply(g.packed.state, ply)) return false;
    GameState next = apply_ply_to_memory(g.packed.state, ply);
    if (in_check(next, side)) return false; // leaves own king in check

    g.packed.state = next;

    // Store ply in history
    g.plyHistory.push_back(static_cast<uint16_t>(ply & 0xFFFF));

    // Clear draw offer when a move is made
    g.packed.drawOfferSide = 0;

    finalize_after_ply(game_id, settle);
    return true;
}

bool GameRegistry::make_ply_uci(uint32_t game_id, const std::string& player,
                                 const std::string& uci, bool settle) {
    auto it = games_.find(game_id);
    if (it == games_.end()) return false;
    uint32_t ply = uci_to_ply(it->second.packed.state, uci);
    return make_ply(game_id, player, ply, settle);
}

// ============================================================
// Finalization — mirrors _finalizeAfterPly
// ============================================================

void GameRegistry::finalize_after_ply(uint32_t game_id, bool settle) {
    Game& g = games_[game_id];
    GameState& state = g.packed.state;

    // 75-move rule (FIDE 9.6.2) — mandatory
    if (state.halfMoveClock >= 150) {
        settle_draw(game_id, Termination::SeventyFiveMoveRule);
        return;
    }

    if (g.info.settlementMode == SettlementMode::CASUAL) {
        if (!settle) return;
        // Caller requested settlement check
        auto result = has_any_legal_ply_with_check(state);
        if (!result.hasLegal) {
            if (result.isInCheck) {
                // Checkmate — side to move lost
                const std::string& winner =
                    (state.sideToMove == COLOR_WHITE) ? g.info.black : g.info.white;
                settle_winner(game_id, winner, Termination::Checkmate);
            } else {
                settle_draw(game_id, Termination::Stalemate);
            }
        }
        return;
    }

    // FULL_FIDE_RULES: full detection
    uint8_t rep = record_repetition(game_id);
    if (rep >= 5) {
        settle_draw(game_id, Termination::FivefoldRepetition);
        return;
    }

    if (is_insufficient_material(state)) {
        settle_draw(game_id, Termination::InsufficientMaterial);
        return;
    }

    auto result = has_any_legal_ply_with_check(state);
    if (!result.hasLegal) {
        if (result.isInCheck) {
            const std::string& winner =
                (state.sideToMove == COLOR_WHITE) ? g.info.black : g.info.white;
            settle_winner(game_id, winner, Termination::Checkmate);
        } else {
            settle_draw(game_id, Termination::Stalemate);
        }
    }
}

// ============================================================
// Settlement helpers
// ============================================================

void GameRegistry::settle_winner(uint32_t game_id, const std::string& winner, Termination t) {
    Game& g = games_[game_id];
    g.info.status = GameStatus::Finished;
    g.info.result = (winner == g.info.white) ? GameResult::WhiteWins : GameResult::BlackWins;
    g.info.winner = winner;
    g.info.termination = t;
}

void GameRegistry::settle_draw(uint32_t game_id, Termination t) {
    Game& g = games_[game_id];
    g.info.status = GameStatus::Finished;
    g.info.result = GameResult::Draw;
    g.info.termination = t;
}

uint8_t GameRegistry::record_repetition(uint32_t game_id) {
    Game& g = games_[game_id];
    uint64_t h = position_hash(g.packed.state);
    return ++g.repetitionCount[h];
}

// ============================================================
// Cancel
// ============================================================

bool GameRegistry::cancel_game(uint32_t game_id, const std::string& player) {
    auto it = games_.find(game_id);
    if (it == games_.end()) return false;
    Game& g = it->second;
    if (g.info.status != GameStatus::Waiting) return false;
    if (player != g.creator) return false;

    g.info.status = GameStatus::Finished;
    g.info.result = GameResult::Draw;
    g.info.termination = Termination::Cancelled;
    return true;
}

// ============================================================
// Resign — mirrors GameRegistry.resign
// ============================================================

bool GameRegistry::resign(uint32_t game_id, const std::string& player) {
    auto it = games_.find(game_id);
    if (it == games_.end()) return false;
    Game& g = it->second;
    if (g.info.status != GameStatus::Active) return false;
    if (player != g.info.white && player != g.info.black) return false;

    // If insufficient material, it's a draw regardless
    if (is_insufficient_material(g.packed.state)) {
        settle_draw(game_id, Termination::InsufficientMaterial);
        return true;
    }

    const std::string& winner = (player == g.info.white) ? g.info.black : g.info.white;
    settle_winner(game_id, winner, Termination::Resignation);
    return true;
}

// ============================================================
// Draw operations — mirrors GameRegistry.offerDraw / acceptDraw
// ============================================================

bool GameRegistry::offer_draw(uint32_t game_id, const std::string& player) {
    auto it = games_.find(game_id);
    if (it == games_.end()) return false;
    Game& g = it->second;
    if (g.info.status != GameStatus::Active) return false;
    if (player != g.info.white && player != g.info.black) return false;

    uint8_t side = (player == g.info.white) ? 1 : 2;
    uint8_t opp_side = (player == g.info.white) ? 2 : 1;

    if (g.packed.drawOfferSide == side) {
        // Cancel own offer
        g.packed.drawOfferSide = 0;
    } else if (g.packed.drawOfferSide == opp_side) {
        // Accept opponent's offer
        settle_draw(game_id, Termination::DrawByAgreement);
    } else {
        // New offer
        g.packed.drawOfferSide = side;
    }
    return true;
}

bool GameRegistry::accept_draw(uint32_t game_id, const std::string& player) {
    auto it = games_.find(game_id);
    if (it == games_.end()) return false;
    Game& g = it->second;
    if (g.info.status != GameStatus::Active) return false;

    uint8_t offer_side = g.packed.drawOfferSide;
    if (offer_side == 0) return false;

    const std::string& acceptor = (offer_side == 1) ? g.info.black : g.info.white;
    if (player != acceptor) return false;

    settle_draw(game_id, Termination::DrawByAgreement);
    return true;
}

// ============================================================
// Claim-based draws
// ============================================================

bool GameRegistry::claim_checkmate(uint32_t game_id, const std::string& player) {
    auto it = games_.find(game_id);
    if (it == games_.end()) return false;
    Game& g = it->second;
    if (g.info.status != GameStatus::Active) return false;
    if (player != g.info.white && player != g.info.black) return false;

    auto result = has_any_legal_ply_with_check(g.packed.state);
    if (result.hasLegal) return false;

    if (result.isInCheck) {
        const std::string& winner =
            (g.packed.state.sideToMove == COLOR_WHITE) ? g.info.black : g.info.white;
        settle_winner(game_id, winner, Termination::Checkmate);
    } else {
        settle_draw(game_id, Termination::Stalemate);
    }
    return true;
}

bool GameRegistry::claim_insufficient_material(uint32_t game_id, const std::string& player) {
    auto it = games_.find(game_id);
    if (it == games_.end()) return false;
    Game& g = it->second;
    if (g.info.status != GameStatus::Active) return false;
    if (player != g.info.white && player != g.info.black) return false;
    if (!is_insufficient_material(g.packed.state)) return false;

    settle_draw(game_id, Termination::InsufficientMaterial);
    return true;
}

bool GameRegistry::claim_fifty_move(uint32_t game_id, const std::string& player) {
    auto it = games_.find(game_id);
    if (it == games_.end()) return false;
    Game& g = it->second;
    if (g.info.status != GameStatus::Active) return false;
    if (player != g.info.white && player != g.info.black) return false;
    if (g.packed.state.halfMoveClock < 100) return false;

    settle_draw(game_id, Termination::FiftyMoveRule);
    return true;
}

bool GameRegistry::claim_threefold(uint32_t game_id, const std::string& player) {
    auto it = games_.find(game_id);
    if (it == games_.end()) return false;
    Game& g = it->second;
    if (g.info.status != GameStatus::Active) return false;
    if (player != g.info.white && player != g.info.black) return false;
    if (g.info.settlementMode == SettlementMode::CASUAL) return false;

    uint64_t h = position_hash(g.packed.state);
    if (g.repetitionCount.count(h) == 0 || g.repetitionCount[h] < 3) return false;

    settle_draw(game_id, Termination::ThreefoldRepetition);
    return true;
}

bool GameRegistry::claim_time(uint32_t game_id, const std::string& player, double current_time) {
    auto it = games_.find(game_id);
    if (it == games_.end()) return false;
    Game& g = it->second;
    if (g.info.status != GameStatus::Active) return false;
    if (player != g.info.white && player != g.info.black) return false;

    // Check ply timer
    if (g.info.plyTimeLimit > 0 && g.info.lastPlyTime > 0) {
        double elapsed = current_time - g.info.lastPlyTime;
        if (elapsed <= g.info.plyTimeLimit) return false;
    }

    // The player whose clock expired loses
    uint8_t side = g.packed.state.sideToMove;
    const std::string& winner = (side == COLOR_WHITE) ? g.info.black : g.info.white;

    if (is_insufficient_material(g.packed.state)) {
        settle_draw(game_id, Termination::InsufficientMaterial);
    } else {
        settle_winner(game_id, winner, Termination::Timeout);
    }
    return true;
}

// ============================================================
// Query
// ============================================================

const Game* GameRegistry::get_game(uint32_t game_id) const {
    auto it = games_.find(game_id);
    if (it == games_.end()) return nullptr;
    return &it->second;
}

std::string GameRegistry::get_fen(uint32_t game_id) const {
    auto it = games_.find(game_id);
    if (it == games_.end()) return "";
    return state_to_fen(it->second.packed.state);
}

std::vector<std::string> GameRegistry::get_ply_history_uci(uint32_t game_id) const {
    auto it = games_.find(game_id);
    if (it == games_.end()) return {};
    std::vector<std::string> result;
    result.reserve(it->second.plyHistory.size());
    for (uint16_t p : it->second.plyHistory) {
        result.push_back(ply_to_uci(static_cast<uint32_t>(p)));
    }
    return result;
}

} // namespace chess
