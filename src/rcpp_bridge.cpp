// rcpp_bridge.cpp — Rcpp bindings for chess engine + game registry
// These bindings are compiled as part of the package and called through
// RcppExports-generated entry points.

// [[Rcpp::plugins(cpp17)]]
#include <Rcpp.h>
#include "chess_engine.h"
#include "chess_game.h"
#include <thread>
#include <algorithm>

using namespace Rcpp;

// ============================================================
// Helpers: GameState <-> R list conversion
// ============================================================

static chess::GameState list_to_state(const List& x) {
    chess::GameState s;
    // bitboards stored as character strings (hex) since R can't hold uint64
    CharacterVector bb = x["bitboards"];
    for (int i = 0; i < 12; i++) {
        s.bitboards[i] = std::stoull(as<std::string>(bb[i]), nullptr, 16);
    }
    s.sideToMove     = as<int>(x["sideToMove"]);
    s.castlingRights = as<int>(x["castlingRights"]);
    s.enPassantSquare = as<int>(x["enPassantSquare"]);
    s.halfMoveClock  = as<int>(x["halfMoveClock"]);
    s.fullMoveNumber = as<int>(x["fullMoveNumber"]);
    return s;
}

static List state_to_list(const chess::GameState& s) {
    CharacterVector bb(12);
    char buf[20];
    for (int i = 0; i < 12; i++) {
        std::snprintf(buf, sizeof(buf), "%016llx", (unsigned long long)s.bitboards[i]);
        bb[i] = std::string(buf);
    }
    return List::create(
        Named("bitboards")      = bb,
        Named("sideToMove")     = (int)s.sideToMove,
        Named("castlingRights") = (int)s.castlingRights,
        Named("enPassantSquare")= (int)s.enPassantSquare,
        Named("halfMoveClock")  = (int)s.halfMoveClock,
        Named("fullMoveNumber") = (int)s.fullMoveNumber,
        Named("class")          = "ChessState"
    );
}

// ============================================================
// Engine functions exposed to R
// ============================================================

// [[Rcpp::export]]
List cpp_init_game() {
    chess::GameState s;
    chess::init_game(s);
    return state_to_list(s);
}

// [[Rcpp::export]]
List cpp_parse_fen(std::string fen) {
    chess::GameState s = chess::parse_fen(fen);
    return state_to_list(s);
}

// [[Rcpp::export]]
std::string cpp_state_to_fen(List state) {
    chess::GameState s = list_to_state(state);
    return chess::state_to_fen(s);
}

// [[Rcpp::export]]
bool cpp_in_check(List state, int color) {
    chess::GameState s = list_to_state(state);
    return chess::in_check(s, static_cast<uint8_t>(color));
}

// [[Rcpp::export]]
bool cpp_is_legal_ply(List state, int ply) {
    chess::GameState s = list_to_state(state);
    return chess::is_legal_ply(s, static_cast<uint32_t>(ply));
}

// [[Rcpp::export]]
List cpp_apply_ply(List state, int ply) {
    chess::GameState s = list_to_state(state);
    chess::GameState next = chess::apply_ply_memory(s, static_cast<uint32_t>(ply));
    return state_to_list(next);
}

// [[Rcpp::export]]
IntegerVector cpp_generate_legal_moves(List state) {
    chess::GameState s = list_to_state(state);
    std::vector<uint32_t> moves = chess::generate_legal_moves(s);
    IntegerVector result(moves.size());
    for (size_t i = 0; i < moves.size(); i++) {
        result[i] = static_cast<int>(moves[i]);
    }
    return result;
}

// [[Rcpp::export]]
List cpp_has_any_legal_ply_with_check(List state) {
    chess::GameState s = list_to_state(state);
    auto r = chess::has_any_legal_ply_with_check(s);
    return List::create(
        Named("hasLegal")  = r.hasLegal,
        Named("isInCheck") = r.isInCheck
    );
}

// [[Rcpp::export]]
bool cpp_is_insufficient_material(List state) {
    chess::GameState s = list_to_state(state);
    return chess::is_insufficient_material(s);
}

// [[Rcpp::export]]
bool cpp_validate_position(List state) {
    chess::GameState s = list_to_state(state);
    return chess::validate_position(s);
}

// [[Rcpp::export]]
int cpp_uci_to_ply(List state, std::string uci) {
    chess::GameState s = list_to_state(state);
    return static_cast<int>(chess::uci_to_ply(s, uci));
}

// [[Rcpp::export]]
std::string cpp_ply_to_uci(int ply) {
    return chess::ply_to_uci(static_cast<uint32_t>(ply));
}

// [[Rcpp::export]]
std::string cpp_position_hash(List state) {
    chess::GameState s = list_to_state(state);
    uint64_t h = chess::position_hash(s);
    char buf[20];
    std::snprintf(buf, sizeof(buf), "%016llx", (unsigned long long)h);
    return std::string(buf);
}

// ============================================================
// Batch enrichment — the high-speed version
// ============================================================

// [[Rcpp::export]]
DataFrame cpp_enrich_batch(CharacterVector fens, CharacterVector uci_moves) {
    int n = fens.size();
    LogicalVector is_capture(n), is_castling(n), is_promotion(n);
    LogicalVector is_en_passant(n), gives_check(n), gives_in_check(n);
    LogicalVector gives_discovered_check(n);
    IntegerVector material_bal(n), num_pieces(n), legal_move_count(n);
    IntegerVector moved_piece_value(n), captured_piece_value(n);
    LogicalVector is_checkmate(n), is_stalemate(n);
    IntegerVector num_captures_avail(n), num_checks_avail(n), max_capture_val(n);
    LogicalVector can_castle_avail(n);
    IntegerVector num_sacrifices_avail(n), num_winning_caps_avail(n), max_sacrifice_gap(n);
    // Priority 1 & 2: new enrichment vectors
    IntegerVector king_safety_own(n), king_safety_opp(n);
    IntegerVector mob_pawn(n), mob_knight(n), mob_bishop(n), mob_rook(n), mob_queen(n), mob_king(n);
    IntegerVector doubled_pawns(n), isolated_pawns(n), passed_pawns(n);
    IntegerVector pin_count(n);
    LogicalVector en_passant_avail(n), promotion_available(n);

    for (int i = 0; i < n; i++) {
        chess::GameState state = chess::parse_fen(as<std::string>(fens[i]));
        std::string uci = as<std::string>(uci_moves[i]);
        uint32_t ply = chess::uci_to_ply(state, uci);

        uint8_t from, to, promo;
        chess::decode_ply(ply, from, to, promo);

        // Pre-move analysis
        uint64_t own_occ = chess::side_occupancy(state, state.sideToMove);
        uint64_t opp_occ = chess::side_occupancy(state, state.sideToMove ^ 1);
        uint8_t offset = (state.sideToMove == chess::COLOR_WHITE) ? chess::WHITE_OFFSET : chess::BLACK_OFFSET;
        uint8_t opp_offset = (state.sideToMove == chess::COLOR_WHITE) ? chess::BLACK_OFFSET : chess::WHITE_OFFSET;

        // Is the mover's side in check?
        gives_in_check[i] = chess::in_check(state, state.sideToMove);

        // Find mover piece type
        uint64_t from_mask = 1ULL << from;
        uint8_t piece_type = 0;
        for (int j = 0; j < 6; j++) {
            if (state.bitboards[offset + j] & from_mask) {
                piece_type = static_cast<uint8_t>(j);
                break;
            }
        }

        // Capture?
        uint64_t to_mask = 1ULL << to;
        bool capture = (opp_occ & to_mask) != 0;
        // En passant capture
        if (piece_type == chess::PIECE_PAWN && to == state.enPassantSquare && state.enPassantSquare != chess::NO_EP) {
            capture = true;
            is_en_passant[i] = true;
        } else {
            is_en_passant[i] = false;
        }
        is_capture[i] = capture;

        // Piece values for moved and captured pieces
        static const int piece_vals[] = {1, 3, 3, 5, 9, 0}; // P,N,B,R,Q,K
        moved_piece_value[i] = piece_vals[piece_type];
        if (capture) {
            // Find captured piece type on opponent bitboards
            int cap_val = 0;
            if (is_en_passant[i]) {
                cap_val = 1; // en passant always captures a pawn
            } else {
                for (int j = 0; j < 6; j++) {
                    if (state.bitboards[opp_offset + j] & to_mask) {
                        cap_val = piece_vals[j];
                        break;
                    }
                }
            }
            captured_piece_value[i] = cap_val;
        } else {
            captured_piece_value[i] = 0;
        }

        // Castling?
        is_castling[i] = (piece_type == chess::PIECE_KING) &&
            ((state.sideToMove == chess::COLOR_WHITE && from == 4 && (to == 6 || to == 2)) ||
             (state.sideToMove == chess::COLOR_BLACK && from == 60 && (to == 62 || to == 58)));

        // Promotion?
        is_promotion[i] = (promo > 0);

        // Material balance
        int w_mat = 0, b_mat = 0, n_pcs = 0;
        static const int vals[] = {1, 3, 3, 5, 9, 0}; // P,N,B,R,Q,K
        for (int j = 0; j < 6; j++) {
            int wc = chess::popcount(state.bitboards[chess::WHITE_OFFSET + j]);
            int bc = chess::popcount(state.bitboards[chess::BLACK_OFFSET + j]);
            w_mat += wc * vals[j];
            b_mat += bc * vals[j];
            n_pcs += wc + bc;
        }
        material_bal[i] = w_mat - b_mat;
        num_pieces[i] = n_pcs;

        // ---------------------------------------------------------------
        // Priority 1 & 2 features: computed before legal move generation
        // ---------------------------------------------------------------

        // King safety: squares in each king's zone attacked by the opponent
        {
            uint8_t own_ksq = chess::find_king_square(state, state.sideToMove);
            uint64_t zone = chess::king_attacks(own_ksq) | (1ULL << own_ksq);
            int cnt = 0; uint64_t z = zone;
            while (z) { uint8_t sq = chess::pop_lsb(z); if (chess::is_square_attacked(state, sq, state.sideToMove ^ 1)) cnt++; }
            king_safety_own[i] = cnt;

            uint8_t opp_ksq = chess::find_king_square(state, state.sideToMove ^ 1);
            zone = chess::king_attacks(opp_ksq) | (1ULL << opp_ksq);
            cnt = 0; z = zone;
            while (z) { uint8_t sq = chess::pop_lsb(z); if (chess::is_square_attacked(state, sq, state.sideToMove)) cnt++; }
            king_safety_opp[i] = cnt;
        }

        // Pawn structure: doubled, isolated, passed
        {
            static const uint64_t FILE_BB[8] = {
                0x0101010101010101ULL, 0x0202020202020202ULL,
                0x0404040404040404ULL, 0x0808080808080808ULL,
                0x1010101010101010ULL, 0x2020202020202020ULL,
                0x4040404040404040ULL, 0x8080808080808080ULL
            };
            uint64_t own_pawns = state.bitboards[offset + chess::PIECE_PAWN];
            uint64_t opp_pawns = state.bitboards[opp_offset + chess::PIECE_PAWN];
            int dbl = 0, iso = 0, passed = 0;
            for (int f = 0; f < 8; f++) {
                int cnt = chess::popcount(own_pawns & FILE_BB[f]);
                if (cnt >= 2) dbl += cnt - 1;
                if (cnt > 0) {
                    uint64_t adj = 0;
                    if (f > 0) adj |= FILE_BB[f-1];
                    if (f < 7) adj |= FILE_BB[f+1];
                    if (!(own_pawns & adj)) iso += cnt;
                }
            }
            uint64_t pp = own_pawns;
            while (pp) {
                uint8_t sq = chess::pop_lsb(pp);
                uint8_t f2 = chess::sq_file(sq), r2 = chess::sq_rank(sq);
                uint64_t adj_files = FILE_BB[f2];
                if (f2 > 0) adj_files |= FILE_BB[f2-1];
                if (f2 < 7) adj_files |= FILE_BB[f2+1];
                uint64_t front = 0;
                if (state.sideToMove == chess::COLOR_WHITE) {
                    for (int r3 = r2+1; r3 < 8; r3++) front |= (0xFFULL << (r3*8)) & adj_files;
                } else {
                    for (int r3 = 0; r3 < (int)r2; r3++) front |= (0xFFULL << (r3*8)) & adj_files;
                }
                if (!(opp_pawns & front)) passed++;
            }
            doubled_pawns[i]  = dbl;
            isolated_pawns[i] = iso;
            passed_pawns[i]   = passed;
        }

        // En passant square available before this move
        en_passant_avail[i] = (state.enPassantSquare != chess::NO_EP);

        // Pin count: own pieces whose removal exposes own king to slider
        {
            uint8_t own_ksq   = chess::find_king_square(state, state.sideToMove);
            uint64_t all_occ  = own_occ | opp_occ;
            uint64_t non_king = own_occ & ~(1ULL << own_ksq);
            int pins = 0;
            uint64_t pp = non_king;
            while (pp) {
                uint8_t sq = chess::pop_lsb(pp);
                uint64_t test_occ = all_occ & ~(1ULL << sq);
                if (chess::is_square_attacked_with_occ(state, own_ksq, state.sideToMove ^ 1, test_occ))
                    pins++;
            }
            pin_count[i] = pins;
        }

        // Apply move and check if opponent is now in check
        if (chess::is_pseudo_legal_ply(state, ply)) {
            chess::GameState next = chess::apply_ply_to_memory(state, ply);
            gives_check[i] = chess::in_check(next, next.sideToMove);

            // Discovered check: king is in check, but the moved piece at 'to'
            // does not directly attack the king (a behind piece was revealed).
            if (gives_check[i] && !is_castling[i]) {
                uint64_t next_all_occ =
                    chess::side_occupancy(next, chess::COLOR_WHITE) |
                    chess::side_occupancy(next, chess::COLOR_BLACK);
                uint8_t opp_ksq_next =
                    chess::find_king_square(next, next.sideToMove);
                uint64_t to_bit  = 1ULL << to;
                uint64_t king_bit = 1ULL << opp_ksq_next;
                uint8_t tal_off = (state.sideToMove == chess::COLOR_WHITE)
                    ? chess::WHITE_OFFSET : chess::BLACK_OFFSET;

                bool to_attacks_king = false;
                // Pawn
                if (next.bitboards[tal_off + chess::PIECE_PAWN] & to_bit) {
                    if (state.sideToMove == chess::COLOR_WHITE) {
                        if ((to & 7) >= 1 && to + 7 < 64)
                            to_attacks_king |=
                                ((1ULL << (to + 7)) & king_bit) != 0;
                        if ((to & 7) <= 6 && to + 9 < 64)
                            to_attacks_king |=
                                ((1ULL << (to + 9)) & king_bit) != 0;
                    } else {
                        if ((to & 7) >= 1 && to >= 9)
                            to_attacks_king |=
                                ((1ULL << (to - 9)) & king_bit) != 0;
                        if ((to & 7) <= 6 && to >= 7)
                            to_attacks_king |=
                                ((1ULL << (to - 7)) & king_bit) != 0;
                    }
                }
                // Knight
                if (next.bitboards[tal_off + chess::PIECE_KNIGHT] & to_bit)
                    to_attacks_king |=
                        (chess::knight_attacks(to) & king_bit) != 0;
                // Diagonal sliders (bishop / queen)
                if ((next.bitboards[tal_off + chess::PIECE_BISHOP] |
                     next.bitboards[tal_off + chess::PIECE_QUEEN]) & to_bit)
                    to_attacks_king |=
                        (chess::bishop_attacks(to, next_all_occ, 0) &
                         king_bit) != 0;
                // Orthogonal sliders (rook / queen)
                if ((next.bitboards[tal_off + chess::PIECE_ROOK] |
                     next.bitboards[tal_off + chess::PIECE_QUEEN]) & to_bit)
                    to_attacks_king |=
                        (chess::rook_attacks(to, next_all_occ, 0) &
                         king_bit) != 0;

                // Discovered check: king in check but NOT from piece at 'to'
                gives_discovered_check[i] = !to_attacks_king;
            } else {
                gives_discovered_check[i] = false;
            }

            // Count legal moves from the position BEFORE the move
            auto moves = chess::generate_legal_moves(state);
            legal_move_count[i] = static_cast<int>(moves.size());

            // Count available move types across all legal moves
            int nc = 0, nchk = 0, mcv = 0;
            int n_sac = 0, n_win_cap = 0, max_sac_gap = 0;
            bool castle_ok = false;
            int mob[6] = {0, 0, 0, 0, 0, 0};
            bool promo_av = false;
            for (auto m : moves) {
                uint8_t mf, mt, mp;
                chess::decode_ply(m, mf, mt, mp);
                uint64_t mt_mask = 1ULL << mt;
                uint64_t mf_mask = 1ULL << mf;

                // Mobility per piece type + promotion detection
                for (int j = 0; j < 6; j++) {
                    if (state.bitboards[offset + j] & mf_mask) { mob[j]++; break; }
                }
                if (mp > 0) promo_av = true;

                // Capture?
                bool is_pawn = (state.bitboards[offset + 0] & mf_mask) != 0;
                bool cap = (opp_occ & mt_mask) != 0;
                if (is_pawn && mt == state.enPassantSquare && state.enPassantSquare != chess::NO_EP)
                    cap = true;
                if (cap) {
                    nc++;
                    int cv = 0;
                    if (is_pawn && mt == state.enPassantSquare && state.enPassantSquare != chess::NO_EP) {
                        cv = 1;
                    } else {
                        for (int j = 0; j < 6; j++) {
                            if (state.bitboards[opp_offset + j] & mt_mask) { cv = vals[j]; break; }
                        }
                    }
                    if (cv > mcv) mcv = cv;
                    // Find moved piece value for this legal move
                    int mv = 0;
                    for (int j = 0; j < 6; j++) {
                        if (state.bitboards[offset + j] & mf_mask) { mv = vals[j]; break; }
                    }
                    if (mv > cv) {
                        // Sacrifice: giving up more than gaining
                        n_sac++;
                        int gap = mv - cv;
                        if (gap > max_sac_gap) max_sac_gap = gap;
                    } else {
                        // Equal or winning capture
                        n_win_cap++;
                    }
                }

                // Castling?
                bool is_king = (state.bitboards[offset + chess::PIECE_KING] & mf_mask) != 0;
                if (is_king && abs((int)mt - (int)mf) == 2) castle_ok = true;

                // Check?
                chess::GameState ns = chess::apply_ply_to_memory(state, m);
                if (chess::in_check(ns, ns.sideToMove)) nchk++;
            }
            num_captures_avail[i] = nc;
            num_checks_avail[i] = nchk;
            can_castle_avail[i] = castle_ok;
            max_capture_val[i] = mcv;
            num_sacrifices_avail[i] = n_sac;
            num_winning_caps_avail[i] = n_win_cap;
            max_sacrifice_gap[i] = max_sac_gap;
            mob_pawn[i]   = mob[0]; mob_knight[i] = mob[1]; mob_bishop[i] = mob[2];
            mob_rook[i]   = mob[3]; mob_queen[i]  = mob[4]; mob_king[i]   = mob[5];
            promotion_available[i] = promo_av;

            // Check for checkmate/stalemate AFTER the move
            auto r = chess::has_any_legal_ply_with_check(next);
            is_checkmate[i] = !r.hasLegal && r.isInCheck;
            is_stalemate[i] = !r.hasLegal && !r.isInCheck;
        } else {
            gives_check[i] = false;
            legal_move_count[i] = 0;
            is_checkmate[i] = false;
            is_stalemate[i] = false;
            num_captures_avail[i] = 0;
            num_checks_avail[i] = 0;
            can_castle_avail[i] = false;
            max_capture_val[i] = 0;
            num_sacrifices_avail[i] = 0;
            num_winning_caps_avail[i] = 0;
            max_sacrifice_gap[i] = 0;
            mob_pawn[i] = 0; mob_knight[i] = 0; mob_bishop[i] = 0;
            mob_rook[i] = 0; mob_queen[i]  = 0; mob_king[i]  = 0;
            promotion_available[i] = false;
        }
    }

    return DataFrame::create(
        Named("is_capture")    = is_capture,
        Named("is_castling")   = is_castling,
        Named("is_promotion")  = is_promotion,
        Named("is_en_passant") = is_en_passant,
        Named("gives_check")   = gives_check,
        Named("gives_discovered_check") = gives_discovered_check,
        Named("in_check")      = gives_in_check,
        Named("material_bal")  = material_bal,
        Named("num_pieces")    = num_pieces,
        Named("legal_move_count") = legal_move_count,
        Named("moved_piece_value") = moved_piece_value,
        Named("captured_piece_value") = captured_piece_value,
        Named("is_checkmate")  = is_checkmate,
        Named("is_stalemate")  = is_stalemate,
        Named("num_captures_avail") = num_captures_avail,
        Named("num_checks_avail")   = num_checks_avail,
        Named("can_castle")              = can_castle_avail,
        Named("max_capture_value")        = max_capture_val,
        Named("num_sacrifices_avail")     = num_sacrifices_avail,
        Named("num_winning_caps_avail")   = num_winning_caps_avail,
        Named("max_sacrifice_gap")        = max_sacrifice_gap,
        Named("king_safety_own")          = king_safety_own,
        Named("king_safety_opp")          = king_safety_opp,
        Named("mob_pawn")                 = mob_pawn,
        Named("mob_knight")               = mob_knight,
        Named("mob_bishop")               = mob_bishop,
        Named("mob_rook")                 = mob_rook,
        Named("mob_queen")                = mob_queen,
        Named("mob_king")                 = mob_king,
        Named("doubled_pawns")            = doubled_pawns,
        Named("isolated_pawns")           = isolated_pawns,
        Named("passed_pawns")             = passed_pawns,
        Named("pin_count")                = pin_count,
        Named("en_passant_avail")         = en_passant_avail,
        Named("promotion_available")      = promotion_available,
        Named("stringsAsFactors") = false
    );
}

// ============================================================
// SAN → UCI resolution  (for PGN replay)
// ============================================================

// Identify which piece type (0-5) is on 'sq' for 'color'. Returns -1 if none.
static int piece_on_sq(const chess::GameState& s, uint8_t sq, uint8_t color) {
    uint64_t mask = 1ULL << sq;
    uint8_t off = (color == chess::COLOR_WHITE) ? chess::WHITE_OFFSET : chess::BLACK_OFFSET;
    for (int p = 0; p < 6; p++) {
        if (s.bitboards[off + p] & mask) return p;
    }
    return -1;
}

// Map SAN piece letter to piece index.
static int san_piece_idx(char c) {
    switch (c) {
        case 'N': return chess::PIECE_KNIGHT;
        case 'B': return chess::PIECE_BISHOP;
        case 'R': return chess::PIECE_ROOK;
        case 'Q': return chess::PIECE_QUEEN;
        case 'K': return chess::PIECE_KING;
        default:  return chess::PIECE_PAWN;
    }
}

// Resolve a SAN move string to a uint32_t ply given the current state.
// Returns 0 on failure.
static uint32_t resolve_san(const chess::GameState& state, const std::string& raw_san) {
    if (raw_san.empty()) return 0;

    // Strip annotations: +, #, !, ?
    std::string san;
    san.reserve(raw_san.size());
    for (char c : raw_san) {
        if (c != '+' && c != '#' && c != '!' && c != '?') san += c;
    }
    if (san.empty()) return 0;

    auto legal = chess::generate_legal_moves(state);

    // --- Castling ---
    if (san == "O-O" || san == "0-0") {
        uint8_t king_from = (state.sideToMove == chess::COLOR_WHITE) ? 4 : 60;
        uint8_t king_to   = (state.sideToMove == chess::COLOR_WHITE) ? 6 : 62;
        uint32_t target = chess::encode_ply(king_from, king_to, 0);
        for (uint32_t m : legal) if (m == target) return m;
        return 0;
    }
    if (san == "O-O-O" || san == "0-0-0") {
        uint8_t king_from = (state.sideToMove == chess::COLOR_WHITE) ? 4 : 60;
        uint8_t king_to   = (state.sideToMove == chess::COLOR_WHITE) ? 2 : 58;
        uint32_t target = chess::encode_ply(king_from, king_to, 0);
        for (uint32_t m : legal) if (m == target) return m;
        return 0;
    }

    // --- Parse promotion ---
    int promo_type = 0;  // 0=none, 1=N, 2=B, 3=R, 4=Q
    std::string body = san;
    {
        auto eq = body.find('=');
        if (eq != std::string::npos && eq + 1 < body.size()) {
            switch (body[eq + 1]) {
                case 'N': promo_type = 1; break;
                case 'B': promo_type = 2; break;
                case 'R': promo_type = 3; break;
                case 'Q': promo_type = 4; break;
            }
            body = body.substr(0, eq);
        }
    }

    // --- Destination square (last 2 chars) ---
    if (body.size() < 2) return 0;
    char to_file_c = body[body.size() - 2];
    char to_rank_c = body[body.size() - 1];
    if (to_file_c < 'a' || to_file_c > 'h' || to_rank_c < '1' || to_rank_c > '8') return 0;
    uint8_t to_sq = (to_rank_c - '1') * 8 + (to_file_c - 'a');

    // --- Piece type ---
    int piece = chess::PIECE_PAWN;
    size_t idx = 0;
    if (!body.empty() && body[0] >= 'A' && body[0] <= 'Z') {
        piece = san_piece_idx(body[0]);
        idx = 1;
    }

    // --- Disambiguation (between piece letter and destination, skip 'x') ---
    std::string mid = body.substr(idx, body.size() - idx - 2);
    int disambig_file = -1, disambig_rank = -1;
    for (char c : mid) {
        if (c >= 'a' && c <= 'h') disambig_file = c - 'a';
        else if (c >= '1' && c <= '8') disambig_rank = c - '1';
        // 'x' is ignored
    }

    // --- Match against legal moves ---
    for (uint32_t m : legal) {
        uint8_t mfrom, mto, mpromo;
        chess::decode_ply(m, mfrom, mto, mpromo);

        if (mto != to_sq) continue;
        if (static_cast<int>(mpromo) != promo_type) continue;

        int p = piece_on_sq(state, mfrom, state.sideToMove);
        if (p != piece) continue;

        if (disambig_file >= 0 && chess::sq_file(mfrom) != static_cast<uint8_t>(disambig_file)) continue;
        if (disambig_rank >= 0 && chess::sq_rank(mfrom) != static_cast<uint8_t>(disambig_rank)) continue;

        return m;
    }
    return 0;
}

// [[Rcpp::export]]
std::string cpp_san_to_uci(List state, std::string san) {
    chess::GameState s = list_to_state(state);
    uint32_t ply = resolve_san(s, san);
    if (ply == 0) return "";
    return chess::ply_to_uci(ply);
}

// Replay a full game from SAN move tokens. Returns a DataFrame with
// columns: fen, uci_move, ply_num.
// [[Rcpp::export]]
DataFrame cpp_replay_game(StringVector san_moves) {
    chess::init_attack_tables();
    int n = san_moves.size();
    StringVector fens(n);
    StringVector ucis(n);
    IntegerVector ply_nums(n);
    LogicalVector ok(n);

    chess::GameState state;
    chess::init_game(state);

    for (int i = 0; i < n; i++) {
        fens[i] = chess::state_to_fen(state);
        ply_nums[i] = i + 1;

        std::string san = as<std::string>(san_moves[i]);
        uint32_t ply = resolve_san(state, san);
        if (ply == 0) {
            ucis[i] = NA_STRING;
            ok[i] = false;
            break;  // can't continue after a failed move
        }
        ucis[i] = chess::ply_to_uci(ply);
        ok[i] = true;
        state = chess::apply_ply_memory(state, ply);
    }

    return DataFrame::create(
        Named("fen")      = fens,
        Named("uci_move") = ucis,
        Named("ply_num")  = ply_nums,
        Named("ok")       = ok,
        Named("stringsAsFactors") = false
    );
}

// Replay multiple games in batch (multi-threaded).
// san_moves is a flat vector of all moves.
// game_ids[g] = ID for game g, game_starts[g] = 1-based index into san_moves
// where game g begins.
// [[Rcpp::export]]
DataFrame cpp_replay_games_batch(IntegerVector game_ids, StringVector san_moves,
                                 IntegerVector game_starts) {
    chess::init_attack_tables();
    int n = san_moves.size();
    int n_games = game_starts.size();

    // Copy R vectors into std::vector (R objects are not thread-safe)
    std::vector<int> v_game_ids(game_ids.begin(), game_ids.end());
    std::vector<int> v_game_starts(game_starts.begin(), game_starts.end());
    std::vector<std::string> v_san(n);
    for (int i = 0; i < n; i++) {
        v_san[i] = as<std::string>(san_moves[i]);
    }

    // Output buffers (plain C++ types, safe for threads)
    std::vector<std::string> out_fens(n);
    std::vector<std::string> out_ucis(n);
    std::vector<int>         out_ply_nums(n);
    std::vector<int>         out_game_ids(n);
    std::vector<char>        out_ok(n, 0);  // not std::vector<bool> — bit-packing races

    // Determine thread count
    int hw = static_cast<int>(std::thread::hardware_concurrency());
    int n_threads = std::max(1, std::min(hw, n_games));

    auto worker = [&](int tid) {
        for (int g = tid; g < n_games; g += n_threads) {
            int start = v_game_starts[g] - 1;  // R is 1-indexed
            int end = (g + 1 < n_games) ? (v_game_starts[g + 1] - 1) : n;
            int gid = v_game_ids[g];

            chess::GameState state;
            chess::init_game(state);

            bool game_ok = true;
            for (int i = start; i < end; i++) {
                out_game_ids[i] = gid;
                out_fens[i] = chess::state_to_fen(state);
                out_ply_nums[i] = (i - start) + 1;

                if (!game_ok) {
                    out_ucis[i] = "";
                    out_ok[i] = false;
                    continue;
                }

                uint32_t ply = resolve_san(state, v_san[i]);
                if (ply == 0) {
                    out_ucis[i] = "";
                    out_ok[i] = false;
                    game_ok = false;
                    continue;
                }
                out_ucis[i] = chess::ply_to_uci(ply);
                out_ok[i] = true;
                state = chess::apply_ply_memory(state, ply);
            }
        }
    };

    // Launch threads
    std::vector<std::thread> threads;
    for (int t = 0; t < n_threads; t++) {
        threads.emplace_back(worker, t);
    }
    for (auto& t : threads) {
        t.join();
    }

    // Copy results back into R vectors
    StringVector r_fens(n);
    StringVector r_ucis(n);
    IntegerVector r_ply_nums(n);
    IntegerVector r_game_ids(n);
    LogicalVector r_ok(n);
    for (int i = 0; i < n; i++) {
        r_game_ids[i] = out_game_ids[i];
        r_fens[i]     = out_fens[i];
        r_ucis[i]     = out_ok[i] ? String(out_ucis[i]) : NA_STRING;
        r_ply_nums[i] = out_ply_nums[i];
        r_ok[i]       = out_ok[i];
    }

    return DataFrame::create(
        Named("game_id")  = r_game_ids,
        Named("fen")      = r_fens,
        Named("uci_move") = r_ucis,
        Named("ply_num")  = r_ply_nums,
        Named("ok")       = r_ok,
        Named("stringsAsFactors") = false
    );
}

// ============================================================
// Global GameRegistry instance for R
// ============================================================

static chess::GameRegistry g_registry;

// [[Rcpp::export]]
int cpp_create_game(std::string creator, int settlement_mode,
                    double ply_time_limit, double initial_time = 0,
                    double increment = 0, int time_mode = 0,
                    bool as_black = false) {
    return static_cast<int>(g_registry.create_game(
        creator,
        static_cast<chess::SettlementMode>(settlement_mode),
        ply_time_limit, initial_time, increment,
        static_cast<chess::TimeMode>(time_mode),
        as_black
    ));
}

// [[Rcpp::export]]
int cpp_create_game_from_fen(std::string creator, std::string fen,
                              int settlement_mode, double ply_time_limit,
                              double initial_time = 0, double increment = 0,
                              int time_mode = 0, bool as_black = false) {
    return static_cast<int>(g_registry.create_game_from_fen(
        creator, fen,
        static_cast<chess::SettlementMode>(settlement_mode),
        ply_time_limit, initial_time, increment,
        static_cast<chess::TimeMode>(time_mode),
        as_black
    ));
}

// [[Rcpp::export]]
bool cpp_join_game(int game_id, std::string player) {
    return g_registry.join_game(static_cast<uint32_t>(game_id), player);
}

// [[Rcpp::export]]
bool cpp_make_ply(int game_id, std::string player, int ply, bool settle = false) {
    return g_registry.make_ply(static_cast<uint32_t>(game_id), player,
                                static_cast<uint32_t>(ply), settle);
}

// [[Rcpp::export]]
bool cpp_make_ply_uci(int game_id, std::string player, std::string uci, bool settle = false) {
    return g_registry.make_ply_uci(static_cast<uint32_t>(game_id), player, uci, settle);
}

// [[Rcpp::export]]
bool cpp_resign(int game_id, std::string player) {
    return g_registry.resign(static_cast<uint32_t>(game_id), player);
}

// [[Rcpp::export]]
bool cpp_offer_draw(int game_id, std::string player) {
    return g_registry.offer_draw(static_cast<uint32_t>(game_id), player);
}

// [[Rcpp::export]]
bool cpp_accept_draw(int game_id, std::string player) {
    return g_registry.accept_draw(static_cast<uint32_t>(game_id), player);
}

// [[Rcpp::export]]
bool cpp_cancel_game(int game_id, std::string player) {
    return g_registry.cancel_game(static_cast<uint32_t>(game_id), player);
}

// [[Rcpp::export]]
List cpp_get_game_info(int game_id) {
    const chess::Game* g = g_registry.get_game(static_cast<uint32_t>(game_id));
    if (!g) return List();
    return List::create(
        Named("white")     = g->info.white,
        Named("black")     = g->info.black,
        Named("status")    = (int)g->info.status,
        Named("result")    = (int)g->info.result,
        Named("winner")    = g->info.winner,
        Named("termination") = (int)g->info.termination,
        Named("fen")       = chess::state_to_fen(g->packed.state),
        Named("plyCount")  = (int)g->plyHistory.size()
    );
}

// [[Rcpp::export]]
std::string cpp_get_fen(int game_id) {
    return g_registry.get_fen(static_cast<uint32_t>(game_id));
}

// [[Rcpp::export]]
CharacterVector cpp_get_ply_history(int game_id) {
    auto moves = g_registry.get_ply_history_uci(static_cast<uint32_t>(game_id));
    CharacterVector result(moves.size());
    for (size_t i = 0; i < moves.size(); i++) {
        result[i] = moves[i];
    }
    return result;
}

// [[Rcpp::export]]
void cpp_reset_registry() {
    g_registry = chess::GameRegistry();
}

// [[Rcpp::export]]
int cpp_game_count() {
    return static_cast<int>(g_registry.game_count());
}
