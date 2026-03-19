# test_public_api.R — Contract tests for the exported ply_* public API.
# Every ply_* function in NAMESPACE must be exercised here.  These tests are
# intentionally independent of cpp_* internals so they protect the public
# contract even if the bridge layer is refactored.

context("Public API — ply_* contract")

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

start_fen <- "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

new_active_game <- function(white = "alice", black = "bob", ...) {
  ply_game_reset_registry()
  id <- ply_game_new(white, ...)
  ply_game_join(id, black)
  id
}

# ---------------------------------------------------------------------------
# Stateless engine functions
# ---------------------------------------------------------------------------

test_that("ply_game_init returns a ChessState list at the starting position", {
  s <- ply_game_init()
  expect_type(s, "list")
  expect_equal(s$class, "ChessState")
  expect_equal(s$sideToMove, 0L)        # white to move
  expect_length(s$bitboards, 12L)       # 6 white + 6 black bitboards
})

test_that("ply_fen_parse parses the starting FEN correctly", {
  s <- ply_fen_parse(start_fen)
  expect_type(s, "list")
  expect_equal(s$sideToMove, 0L)
  expect_equal(s$fullMoveNumber, 1L)
  expect_equal(s$castlingRights, 15L)   # KQkq = 0b1111
})

test_that("ply_fen_serialize round-trips the starting FEN", {
  expect_equal(ply_fen_serialize(ply_fen_parse(start_fen)), start_fen)
})

test_that("ply_move_apply advances position and returns a ChessState", {
  s2 <- ply_move_apply(ply_game_init(), "e2e4")
  expect_type(s2, "list")
  expect_equal(s2$sideToMove, 1L)       # black to move
  expect_true(grepl("4P3", ply_fen_serialize(s2)))
})

test_that("ply_legal_moves returns 20 moves from the starting position", {
  moves <- ply_legal_moves(ply_game_init())
  expect_type(moves, "character")
  expect_equal(length(moves), 20L)
})

test_that("ply_in_check is FALSE at the starting position", {
  expect_false(ply_in_check(ply_game_init()))
})

test_that("ply_in_check explicit color parameter works", {
  expect_false(ply_in_check(ply_game_init(), color = 0L))  # white
  expect_false(ply_in_check(ply_game_init(), color = 1L))  # black
})

test_that("ply_in_check rejects invalid color values", {
  expect_error(ply_in_check(ply_game_init(), color = 2L),  "'color' must be")
  expect_error(ply_in_check(ply_game_init(), color = -1L), "'color' must be")
})

test_that("ply_is_checkmate detects Scholar's Mate", {
  # Position after 1.e4 e5 2.Bc4 Nc6 3.Qh5 Nf6?? 4.Qxf7#
  s <- ply_fen_parse("r1bqkb1r/pppp1Qpp/2n2n2/4p3/2B1P3/8/PPPP1PPP/RNB1K1NR b KQkq - 0 4")
  expect_true(ply_is_checkmate(s))
  expect_false(ply_is_stalemate(s))
})

test_that("ply_is_stalemate detects a stalemate position", {
  # Black king a8, white queen c7, white king b6 — black has no legal moves
  # and is not in check.
  s <- ply_fen_parse("k7/2Q5/1K6/8/8/8/8/8 b - - 0 1")
  expect_true(ply_is_stalemate(s))
  expect_false(ply_is_checkmate(s))
})

test_that("ply_is_insufficient_material detects K vs K", {
  s <- ply_fen_parse("8/8/8/8/8/8/8/K6k w - - 0 1")
  expect_true(ply_is_insufficient_material(s))
})

test_that("ply_validate accepts the starting position", {
  expect_true(ply_validate(ply_game_init()))
})

test_that("ply_hash returns a 16-character lowercase hex string", {
  h <- ply_hash(ply_game_init())
  expect_type(h, "character")
  expect_equal(nchar(h), 16L)
  expect_true(grepl("^[0-9a-f]+$", h))
})

test_that("ply_hash differs between positions", {
  s1 <- ply_game_init()
  s2 <- ply_move_apply(s1, "e2e4")
  expect_false(ply_hash(s1) == ply_hash(s2))
})

# ---------------------------------------------------------------------------
# Batch enrichment
# ---------------------------------------------------------------------------

test_that("ply_enrich_batch rejects mismatched vector lengths", {
  expect_error(
    ply_enrich_batch(c(start_fen), character(0)),
    "same length"
  )
  expect_error(
    ply_enrich_batch(character(0), c("e2e4")),
    "same length"
  )
})

test_that("ply_enrich_batch returns a data.frame with expected columns", {
  df <- ply_enrich_batch(start_fen, "e2e4")
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 1L)
  for (col in c("is_capture", "is_castling", "is_promotion",
                "gives_check", "material_bal", "legal_move_count",
                "king_safety_own", "doubled_pawns", "pin_count")) {
    expect_true(col %in% names(df), info = paste("missing column:", col))
  }
})

test_that("ply_enrich_batch e2e4 from start is not a capture", {
  df <- ply_enrich_batch(start_fen, "e2e4")
  expect_false(df$is_capture[1])
})

# ---------------------------------------------------------------------------
# Game registry — lifecycle
# ---------------------------------------------------------------------------

test_that("ply_game_reset_registry clears all games", {
  ply_game_new("p1")
  ply_game_new("p2")
  ply_game_reset_registry()
  expect_equal(ply_game_count(), 0L)
})

test_that("ply_game_count tracks created games correctly", {
  ply_game_reset_registry()
  expect_equal(ply_game_count(), 0L)
  ply_game_new("p1")
  expect_equal(ply_game_count(), 1L)
  ply_game_new("p2")
  expect_equal(ply_game_count(), 2L)
})

test_that("ply_game_new returns a positive integer id", {
  ply_game_reset_registry()
  id <- ply_game_new("alice")
  expect_type(id, "integer")
  expect_equal(id, 1L)
})

test_that("ply_game_new does NOT reset the registry on each call", {
  ply_game_reset_registry()
  ply_game_new("alice")
  ply_game_new("bob")
  expect_equal(ply_game_count(), 2L)
})

test_that("ply_game_new_from_fen creates a game from a custom FEN", {
  ply_game_reset_registry()
  custom <- "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1"
  id <- ply_game_new_from_fen("alice", custom)
  expect_type(id, "integer")
  expect_true(grepl("4P3", ply_game_fen(id)))
})

test_that("ply_game_join transitions a Waiting game to Active", {
  ply_game_reset_registry()
  id <- ply_game_new("alice")
  expect_equal(ply_game_info(id)$status, 0L)  # Waiting
  expect_true(ply_game_join(id, "bob"))
  expect_equal(ply_game_info(id)$status, 1L)  # Active
})

# ---------------------------------------------------------------------------
# Game registry — play
# ---------------------------------------------------------------------------

test_that("ply_game_move advances position and ply_game_fen reflects it", {
  id <- new_active_game()
  expect_true(ply_game_move(id, "alice", "e2e4"))
  expect_true(grepl("4P3", ply_game_fen(id)))
})

test_that("ply_game_history records moves in order", {
  id <- new_active_game()
  ply_game_move(id, "alice", "e2e4")
  ply_game_move(id, "bob",   "e7e5")
  h <- ply_game_history(id)
  expect_equal(length(h), 2L)
  expect_equal(h[1], "e2e4")
  expect_equal(h[2], "e7e5")
})

test_that("ply_game_info returns all required fields", {
  id <- new_active_game()
  info <- ply_game_info(id)
  expect_true(all(c("white", "black", "status", "result",
                    "winner", "termination", "fen", "plyCount") %in% names(info)))
  expect_equal(info$white, "alice")
  expect_equal(info$black, "bob")
  expect_equal(info$status, 1L)   # Active
})

test_that("ply_game_fen matches the position after moves", {
  id <- new_active_game()
  ply_game_move(id, "alice", "d2d4")
  ply_game_move(id, "bob",   "d7d5")
  fen <- ply_game_fen(id)
  expect_true(grepl("3P4", fen))  # white pawn on d4
  expect_true(grepl("3p4", fen))  # black pawn on d5
})

test_that("ply_game_move settle=TRUE detects checkmate and ends game", {
  id <- new_active_game()
  # Fool's Mate: 1.f3 e5 2.g4 Qh4#
  ply_game_move(id, "alice", "f2f3", settle = TRUE)
  ply_game_move(id, "bob",   "e7e5", settle = TRUE)
  ply_game_move(id, "alice", "g2g4", settle = TRUE)
  ply_game_move(id, "bob",   "d8h4", settle = TRUE)  # Qh4#
  info <- ply_game_info(id)
  expect_equal(info$status, 2L)      # Finished
  expect_equal(info$result, 2L)      # BlackWins
  expect_equal(info$termination, 0L) # Checkmate
})

# ---------------------------------------------------------------------------
# Game registry — termination actions
# ---------------------------------------------------------------------------

test_that("ply_game_resign ends game with correct winner", {
  id <- new_active_game()
  ply_game_move(id, "alice", "e2e4")
  expect_true(ply_game_resign(id, "bob"))
  info <- ply_game_info(id)
  expect_equal(info$status, 2L)       # Finished
  expect_equal(info$result, 1L)       # WhiteWins
  expect_equal(info$termination, 2L)  # Resignation
  expect_equal(info$winner, "alice")
})

test_that("ply_game_offer_draw + ply_game_accept_draw conclude game as draw", {
  id <- new_active_game()
  ply_game_move(id, "alice", "e2e4")
  ply_game_move(id, "bob",   "e7e5")
  expect_true(ply_game_offer_draw(id, "alice"))
  expect_true(ply_game_accept_draw(id, "bob"))
  info <- ply_game_info(id)
  expect_equal(info$status, 2L)      # Finished
  expect_equal(info$result, 3L)      # Draw
  expect_equal(info$termination, 4L) # DrawByAgreement
})

test_that("ply_game_offer_draw by same player twice cancels the offer", {
  id <- new_active_game()
  ply_game_move(id, "alice", "e2e4")
  ply_game_move(id, "bob",   "e7e5")
  ply_game_offer_draw(id, "alice")
  ply_game_offer_draw(id, "alice")   # cancels own offer
  # Game must still be active — accepting now should fail (no outstanding offer)
  expect_false(ply_game_accept_draw(id, "bob"))
  expect_equal(ply_game_info(id)$status, 1L)  # still Active
})

test_that("ply_game_cancel cancels a Waiting game", {
  ply_game_reset_registry()
  id <- ply_game_new("alice")     # no join → Waiting
  expect_true(ply_game_cancel(id, "alice"))
  info <- ply_game_info(id)
  expect_equal(info$status, 2L)       # Finished
  expect_equal(info$termination, 10L) # Cancelled
})

test_that("ply_game_cancel is rejected for non-creator", {
  ply_game_reset_registry()
  id <- ply_game_new("alice")
  expect_false(ply_game_cancel(id, "eve"))
  expect_equal(ply_game_info(id)$status, 0L)  # still Waiting
})

test_that("ply_game_cancel is rejected for an Active game", {
  id <- new_active_game()
  expect_false(ply_game_cancel(id, "alice"))  # already Active
})
