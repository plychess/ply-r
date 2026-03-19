# chess_engine_cpp.R — R wrappers for the C++ bitboard chess engine
# In-package: the C++ code is compiled at install time via src/; no sourceCpp() needed.

# ---- Engine functions ----

#' Parse a FEN string into a ChessState list
#' @param fen A FEN string.
#' @return A named list of class \code{ChessState} representing the board state.
#' @examples
#' state <- ply_fen_parse("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
#' state$sideToMove  # 0 = white
#' @export
ply_fen_parse <- function(fen) cpp_parse_fen(fen)

#' Convert a ChessState list back to a FEN string
#' @param state A \code{ChessState} list from \code{ply_fen_parse}.
#' @return A FEN string.
#' @examples
#' state <- ply_game_init()
#' ply_fen_serialize(state)
#' @export
ply_fen_serialize <- function(state) cpp_state_to_fen(state)

#' Return a ChessState list at the standard starting position
#'
#' Stateless helper that does not interact with the game registry. To create a
#' managed game use \code{\link{ply_game_new}}.
#'
#' @return A named list of class \code{ChessState} representing the starting
#'   chess position.
#' @examples
#' state <- ply_game_init()
#' state$sideToMove  # 0 = white to move
#' state$fullMoveNumber  # 1
#' @export
ply_game_init <- function() cpp_init_game()

#' Apply a UCI move string to a position
#' @param state A \code{ChessState} list.
#' @param uci   A UCI move string such as \code{"e2e4"}.
#' @return The new \code{ChessState} list after the move.
#' @examples
#' s1 <- ply_game_init()
#' s2 <- ply_move_apply(s1, "e2e4")
#' s2$sideToMove  # 1 = black to move
#' @export
ply_move_apply <- function(state, uci) cpp_apply_ply(state, cpp_uci_to_ply(state, uci))

#' List all legal moves from a position as UCI strings
#' @param state A \code{ChessState} list.
#' @return A character vector of UCI move strings.
#' @examples
#' moves <- ply_legal_moves(ply_game_init())
#' length(moves)  # 20 from the starting position
#' @export
ply_legal_moves <- function(state) {
  plies <- cpp_generate_legal_moves(state)
  vapply(plies, cpp_ply_to_uci, character(1))
}

#' Test whether a side is in check
#' @param state A ChessState list.
#' @param color Integer color: \code{0} = white, \code{1} = black (default:
#'   side to move).
#' @return \code{TRUE} if the specified side is in check.
#' @examples
#' ply_in_check(ply_game_init())          # FALSE at the start
#' ply_in_check(ply_game_init(), color = 0L)  # white not in check
#' @export
ply_in_check <- function(state, color = state$sideToMove) {
  color <- as.integer(color)
  if (!color %in% c(0L, 1L))
    stop("'color' must be 0 (white) or 1 (black)")
  cpp_in_check(state, color)
}

#' Test whether the position is checkmate
#' @param state A \code{ChessState} list.
#' @return \code{TRUE} if the side to move is checkmated.
#' @examples
#' # Scholar's Mate — black is checkmated
#' s <- ply_fen_parse(
#'   "r1bqkb1r/pppp1Qpp/2n2n2/4p3/2B1P3/8/PPPP1PPP/RNB1K1NR b KQkq - 0 4"
#' )
#' ply_is_checkmate(s)  # TRUE
#' @export
ply_is_checkmate <- function(state) {
  r <- cpp_has_any_legal_ply_with_check(state)
  !r$hasLegal && r$isInCheck
}

#' Test whether the position is stalemate
#' @param state A \code{ChessState} list.
#' @return \code{TRUE} if the side to move is stalemated.
#' @examples
#' # Black king a8, white queen c7, white king b6 — stalemate
#' s <- ply_fen_parse("k7/2Q5/1K6/8/8/8/8/8 b - - 0 1")
#' ply_is_stalemate(s)  # TRUE
#' @export
ply_is_stalemate <- function(state) {
  r <- cpp_has_any_legal_ply_with_check(state)
  !r$hasLegal && !r$isInCheck
}

#' Test whether the position is a draw by insufficient material
#' @param state A \code{ChessState} list.
#' @return \code{TRUE} if neither side can force checkmate.
#' @examples
#' # Only kings remain
#' s <- ply_fen_parse("8/8/8/8/8/8/8/K6k w - - 0 1")
#' ply_is_insufficient_material(s)  # TRUE
#' @export
ply_is_insufficient_material <- function(state) cpp_is_insufficient_material(state)

#' Validate a position (piece counts, king count, etc.)
#' @param state A \code{ChessState} list.
#' @return \code{TRUE} if the position is legal.
#' @examples
#' ply_validate(ply_game_init())  # TRUE
#' @export
ply_validate <- function(state) cpp_validate_position(state)

#' Compute a Zobrist-style position hash
#' @param state A \code{ChessState} list.
#' @return A 16-character lowercase hex string uniquely identifying the position.
#' @examples
#' h <- ply_hash(ply_game_init())
#' nchar(h)  # 16
#' @export
ply_hash <- function(state) cpp_position_hash(state)

# ---- Batch enrichment (C++ fast path) ----

#' Enrich a batch of positions with chess features
#'
#' Returns a \code{data.frame} with one row per (FEN, UCI-move) pair and
#' columns covering captures, checks, castling, promotion, material balance,
#' pawn structure, mobility, king safety, and pin count.
#'
#' @param fens      Character vector of FEN strings.
#' @param uci_moves Character vector of UCI move strings parallel to
#'   \code{fens}. Must be the same length.
#' @return A \code{data.frame} with one row per position and columns:
#'   \code{is_capture}, \code{is_castling}, \code{is_promotion},
#'   \code{is_en_passant}, \code{gives_check}, \code{gives_discovered_check},
#'   \code{in_check}, \code{material_bal}, \code{num_pieces},
#'   \code{legal_move_count}, \code{moved_piece_value},
#'   \code{captured_piece_value}, \code{is_checkmate}, \code{is_stalemate},
#'   \code{num_captures_avail}, \code{num_checks_avail}, \code{can_castle},
#'   \code{max_capture_value}, \code{king_safety_own}, \code{king_safety_opp},
#'   \code{mob_pawn}, \code{mob_knight}, \code{mob_bishop}, \code{mob_rook},
#'   \code{mob_queen}, \code{mob_king}, \code{doubled_pawns},
#'   \code{isolated_pawns}, \code{passed_pawns}, \code{pin_count},
#'   \code{en_passant_avail}, \code{promotion_available}.
#' @examples
#' start <- "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
#' df <- ply_enrich_batch(start, "e2e4")
#' df$is_capture     # FALSE
#' df$legal_move_count  # 20
#' @export
ply_enrich_batch <- function(fens, uci_moves) {
  if (length(fens) != length(uci_moves))
    stop("'fens' and 'uci_moves' must have the same length")
  cpp_enrich_batch(fens, uci_moves)
}

# ---- Game registry ----

#' Create a new managed game
#'
#' Adds a game to the global registry without resetting existing games. Use
#' \code{\link{ply_game_reset_registry}} first if you want a clean slate.
#'
#' @param creator    Name of the creating player.
#' @param mode       Settlement mode: \code{0} = casual (default), \code{1} =
#'   full FIDE rules (auto-detects repetition and insufficient material).
#' @param time_limit Ply time limit in seconds (\code{0} = no limit, default
#'   600).
#' @return Integer game id.
#' @seealso \code{\link{ply_game_join}}, \code{\link{ply_game_move}},
#'   \code{\link{ply_game_reset_registry}}
#' @examples
#' ply_game_reset_registry()
#' id <- ply_game_new("alice")
#' ply_game_count()  # 1
#' @export
ply_game_new <- function(creator, mode = 0L, time_limit = 600) {
  cpp_create_game(creator, as.integer(mode), as.numeric(time_limit))
}

#' Join an existing game as the opposing player
#' @param game_id Integer game id.
#' @param player  Player name string.
#' @return \code{TRUE} on success.
#' @examples
#' ply_game_reset_registry()
#' id <- ply_game_new("alice")
#' ply_game_join(id, "bob")           # TRUE — game is now Active
#' ply_game_info(id)$status      # 1
#' @export
ply_game_join <- function(game_id, player) cpp_join_game(as.integer(game_id), player)

#' Make a move in a managed game by UCI string
#' @param game_id Integer game id.
#' @param player  Player name string.
#' @param uci     UCI move string (e.g. \code{"e2e4"}).
#' @param settle  If \code{TRUE} (default) the engine checks for checkmate and
#'   stalemate after the move and marks the game finished if detected. In
#'   \code{FULL_FIDE_RULES} mode settlement always runs regardless.
#' @return \code{TRUE} on success.
#' @examples
#' ply_game_reset_registry()
#' id <- ply_game_new("alice")
#' ply_game_join(id, "bob")
#' ply_game_move(id, "alice", "e2e4")  # TRUE
#' ply_game_move(id, "bob",   "e7e5")  # TRUE
#' ply_game_history(id)           # c("e2e4", "e7e5")
#' @export
ply_game_move <- function(game_id, player, uci, settle = TRUE) {
  cpp_make_ply_uci(as.integer(game_id), player, uci, settle)
}

#' Resign a managed game
#' @param game_id Integer game id.
#' @param player  Resigning player's name.
#' @return \code{TRUE} on success.
#' @examples
#' ply_game_reset_registry()
#' id <- ply_game_new("alice")
#' ply_game_join(id, "bob")
#' ply_game_move(id, "alice", "e2e4")
#' ply_game_resign(id, "bob")          # bob resigns; alice wins
#' ply_game_info(id)$result       # 1 = WhiteWins
#' @export
ply_game_resign <- function(game_id, player) cpp_resign(as.integer(game_id), player)

#' Get metadata for a managed game
#' @param game_id Integer game id.
#' @return A named list with fields \code{white}, \code{black},
#'   \code{status} (\code{0}=Waiting, \code{1}=Active, \code{2}=Finished),
#'   \code{result} (\code{0}=InProgress, \code{1}=WhiteWins,
#'   \code{2}=BlackWins, \code{3}=Draw), \code{winner}, \code{termination},
#'   \code{fen}, and \code{plyCount}.
#' @examples
#' ply_game_reset_registry()
#' id <- ply_game_new("alice")
#' ply_game_join(id, "bob")
#' info <- ply_game_info(id)
#' info$white   # "alice"
#' info$status  # 1 (Active)
#' @export
ply_game_info <- function(game_id) cpp_get_game_info(as.integer(game_id))

#' Get the current FEN for a managed game
#' @param game_id Integer game id.
#' @return A FEN string.
#' @examples
#' ply_game_reset_registry()
#' id <- ply_game_new("alice")
#' ply_game_join(id, "bob")
#' ply_game_move(id, "alice", "d2d4")
#' ply_game_fen(id)  # FEN with white pawn on d4
#' @export
ply_game_fen <- function(game_id) cpp_get_fen(as.integer(game_id))

#' Get the full ply history of a managed game as UCI strings
#' @param game_id Integer game id.
#' @return A character vector of UCI move strings, one per ply played.
#' @examples
#' ply_game_reset_registry()
#' id <- ply_game_new("alice")
#' ply_game_join(id, "bob")
#' ply_game_move(id, "alice", "e2e4")
#' ply_game_move(id, "bob",   "c7c5")
#' ply_game_history(id)  # c("e2e4", "c7c5")
#' @export
ply_game_history <- function(game_id) cpp_get_ply_history(as.integer(game_id))

#' Offer or toggle a draw in a managed game
#'
#' Records a draw offer from \code{player}. If the opponent has already offered
#' a draw, this call accepts it and concludes the game as a draw by agreement.
#' A player can cancel their own outstanding offer by calling this again with
#' the same \code{player} name.
#'
#' @param game_id Integer game id.
#' @param player  Player name string.
#' @return \code{TRUE} on success.
#' @seealso \code{\link{ply_game_accept_draw}}
#' @examples
#' ply_game_reset_registry()
#' id <- ply_game_new("alice")
#' ply_game_join(id, "bob")
#' ply_game_move(id, "alice", "e2e4")
#' ply_game_move(id, "bob",   "e7e5")
#' ply_game_offer_draw(id, "alice")   # alice offers a draw
#' ply_game_accept_draw(id, "bob")    # bob accepts
#' ply_game_info(id)$result      # 3 = Draw
#' @export
ply_game_offer_draw <- function(game_id, player) cpp_offer_draw(as.integer(game_id), player)

#' Accept an outstanding draw offer in a managed game
#'
#' @param game_id Integer game id.
#' @param player  Player name — must be the player who did \emph{not} offer.
#' @return \code{TRUE} on success; \code{FALSE} if no offer is pending or wrong
#'   player.
#' @seealso \code{\link{ply_game_offer_draw}}
#' @examples
#' ply_game_reset_registry()
#' id <- ply_game_new("alice")
#' ply_game_join(id, "bob")
#' ply_game_move(id, "alice", "e2e4")
#' ply_game_move(id, "bob",   "e7e5")
#' ply_game_offer_draw(id, "alice")
#' ply_game_accept_draw(id, "bob")    # TRUE — game ends as Draw
#' @export
ply_game_accept_draw <- function(game_id, player) cpp_accept_draw(as.integer(game_id), player)

#' Cancel a waiting managed game
#'
#' Only the game creator can cancel, and only while the game is still in
#' \emph{Waiting} status (i.e.\ the opponent has not yet joined).
#'
#' @param game_id Integer game id.
#' @param player  Creator's player name.
#' @return \code{TRUE} on success; \code{FALSE} if not the creator or game is
#'   already active.
#' @examples
#' ply_game_reset_registry()
#' id <- ply_game_new("alice")  # Waiting — no opponent yet
#' ply_game_cancel(id, "alice") # TRUE
#' ply_game_info(id)$termination  # 10 = Cancelled
#' @export
ply_game_cancel <- function(game_id, player) cpp_cancel_game(as.integer(game_id), player)

#' Create a new managed game from a custom starting position
#'
#' @param creator    Name of the creating player.
#' @param fen        A FEN string for the starting position.
#' @param mode       Settlement mode: \code{0} = casual (default), \code{1} =
#'   full FIDE rules.
#' @param time_limit Ply time limit in seconds (\code{0} = no limit, default
#'   600).
#' @return Integer game id.
#' @seealso \code{\link{ply_game_new}}
#' @examples
#' ply_game_reset_registry()
#' fen <- "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1"
#' id  <- ply_game_new_from_fen("alice", fen)
#' ply_game_join(id, "bob")
#' ply_game_fen(id)  # starts after 1.e4
#' @export
ply_game_new_from_fen <- function(creator, fen, mode = 0L, time_limit = 600) {
  cpp_create_game_from_fen(creator, fen, as.integer(mode), as.numeric(time_limit))
}

#' Return the number of games currently held in the registry
#' @return A non-negative integer.
#' @seealso \code{\link{ply_game_new}}, \code{\link{ply_game_reset_registry}}
#' @examples
#' ply_game_reset_registry()
#' ply_game_count()        # 0
#' ply_game_new("alice")
#' ply_game_count()        # 1
#' @export
ply_game_count <- function() cpp_game_count()

#' Reset (clear) the global game registry
#'
#' Destroys all managed games. Game ids from before the reset become invalid.
#' Subsequent \code{\link{ply_game_count}} calls return 0.
#'
#' @return Invisibly \code{NULL}.
#' @seealso \code{\link{ply_game_new}}, \code{\link{ply_game_count}}
#' @examples
#' ply_game_new("alice")
#' ply_game_reset_registry()
#' ply_game_count()  # 0
#' @export
ply_game_reset_registry <- function() invisible(cpp_reset_registry())

