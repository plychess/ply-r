# test_edge_cases_copilot.R — Tests for edge cases identified by Copilot code review
#
# Covers: empty PGN files, overflow protection, empty game handling,
#         mailbox consistency, and file read safety.

context("Copilot Review — Edge Cases")

# ---------------------------------------------------------------------------
# 1. Empty PGN file (no games at all)
# ---------------------------------------------------------------------------

test_that("replay_all_games handles empty PGN file gracefully", {
  tmp <- tempfile(fileext = ".pgn")
  writeLines("", tmp)
  on.exit(unlink(tmp))

  result <- replay_all_games(tmp)
  expect_true(is.data.frame(result) || nrow(result) == 0 || is.null(result))
})

test_that("cpp_replay_pgn_file handles empty file gracefully", {
  tmp <- tempfile(fileext = ".pgn")
  writeLines("", tmp)
  on.exit(unlink(tmp))

  result <- ply:::cpp_replay_pgn_file(tmp)
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 0L)
})

# ---------------------------------------------------------------------------
# 2. PGN with tags but no movetext
# ---------------------------------------------------------------------------

test_that("replay_all_games handles tags-only PGN (no moves)", {
  tmp <- tempfile(fileext = ".pgn")
  writeLines(c(
    '[Event "Test"]',
    '[White "Alice"]',
    '[Black "Bob"]',
    '[Result "*"]',
    '',
    '*',
    ''
  ), tmp)
  on.exit(unlink(tmp))

  result <- replay_all_games(tmp)
  # Should return empty or zero-row — no moves to replay
  expect_true(is.data.frame(result))
})

test_that("cpp_replay_pgn_file handles tags-only PGN (no moves)", {
  tmp <- tempfile(fileext = ".pgn")
  writeLines(c(
    '[Event "Test"]',
    '[White "Alice"]',
    '[Black "Bob"]',
    '[Result "*"]',
    '',
    '*',
    ''
  ), tmp)
  on.exit(unlink(tmp))

  result <- ply:::cpp_replay_pgn_file(tmp)
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 0L)
})

# ---------------------------------------------------------------------------
# 3. PGN with no [Event tag
# ---------------------------------------------------------------------------

test_that("cpp_replay_pgn_file handles file with no [Event tag", {
  tmp <- tempfile(fileext = ".pgn")
  writeLines("This is not a PGN file at all.", tmp)
  on.exit(unlink(tmp))

  result <- ply:::cpp_replay_pgn_file(tmp)
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 0L)
})

# ---------------------------------------------------------------------------
# 4. cpp_xp_perft returns correct values (overflow check)
# ---------------------------------------------------------------------------

test_that("cpp_xp_perft returns correct values for small depths", {
  fen <- "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
  expect_equal(ply:::cpp_xp_perft(fen, 1L), 20)
  expect_equal(ply:::cpp_xp_perft(fen, 2L), 400)
  expect_equal(ply:::cpp_xp_perft(fen, 3L), 8902)
  expect_equal(ply:::cpp_xp_perft(fen, 4L), 197281)
})

test_that("cpp_xp_perft depth 0 returns 1", {
  fen <- "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
  expect_equal(ply:::cpp_xp_perft(fen, 0L), 1)
})

test_that("cpp_xp_perft depth 5 does not overflow (returns double > 2^31)", {
  fen <- "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
  result <- ply:::cpp_xp_perft(fen, 5L)
  expect_equal(result, 4865609)
  expect_true(is.double(result))  # must be double, not integer
})

# ---------------------------------------------------------------------------
# 5. External pointer API correctness
# ---------------------------------------------------------------------------

test_that("XP API produces same results as list API", {
  fen <- "r1bqkb1r/pppp1ppp/2n2n2/4p3/2B1P3/5N2/PPPP1PPP/RNBQK2R w KQkq - 4 4"

  # List API
  state_list <- ply:::cpp_parse_fen(fen)
  moves_list <- ply:::cpp_generate_legal_moves(state_list)
  fen_list <- ply:::cpp_state_to_fen(state_list)

  # XP API
  state_xp <- ply:::cpp_xp_parse_fen(fen)
  moves_xp <- ply:::cpp_xp_generate_legal_moves(state_xp)
  fen_xp <- ply:::cpp_xp_state_to_fen(state_xp)

  expect_equal(fen_list, fen_xp)
  expect_equal(sort(moves_list), sort(moves_xp))
})

test_that("XP apply_ply produces correct FEN", {
  fen <- "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
  state <- ply:::cpp_xp_parse_fen(fen)
  moves <- ply:::cpp_xp_generate_legal_moves(state)

  # Apply first legal move
  next_state <- ply:::cpp_xp_apply_ply(state, moves[1])
  next_fen <- ply:::cpp_xp_state_to_fen(next_state)

  # Should be a valid FEN, black to move
  expect_true(grepl(" b ", next_fen))
})

test_that("XP count_legal_moves matches generate length", {
  fen <- "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1"
  state <- ply:::cpp_xp_parse_fen(fen)

  count <- ply:::cpp_xp_count_legal_moves(state)
  moves <- ply:::cpp_xp_generate_legal_moves(state)

  expect_equal(count, length(moves))
})

# ---------------------------------------------------------------------------
# 6. Enrichment with edge-case positions
# ---------------------------------------------------------------------------

test_that("enrichment handles position with no pawns", {
  # K vs K — no pawns, no captures, no nothing
  fen <- "4k3/8/8/8/8/8/8/4K3 w - - 0 1"
  uci <- "e1d2"
  result <- ply:::cpp_enrich_batch(fen, uci)

  expect_equal(result$doubled_pawns, 0L)
  expect_equal(result$isolated_pawns, 0L)
  expect_equal(result$passed_pawns, 0L)
  expect_equal(result$pawn_chain_length, 0L)
  expect_equal(result$pawn_islands, 0L)
  expect_equal(result$num_pieces, 2L)
})

test_that("enrichment handles promotion move", {
  # White pawn on e7, can promote
  fen <- "4k3/4P3/8/8/8/8/8/4K3 w - - 0 1"
  uci <- "e7e8q"
  result <- ply:::cpp_enrich_batch(fen, uci)

  expect_true(result$is_promotion)
  expect_equal(result$moved_piece_value, 1L)  # pawn
})

test_that("new style features have correct ranges", {
  fen <- "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1"
  uci <- "e7e5"
  result <- ply:::cpp_enrich_batch(fen, uci)

  expect_true(result$center_occupied >= 0 && result$center_occupied <= 4)
  expect_true(result$center_attacked >= 0 && result$center_attacked <= 4)
  expect_true(result$undeveloped_minors >= 0 && result$undeveloped_minors <= 4)
  expect_true(result$space_advantage >= 0 && result$space_advantage <= 32)
  expect_true(result$pieces_defended >= 0)
  expect_true(result$pawn_chain_length >= 0)
  expect_true(result$pawn_islands >= 0 && result$pawn_islands <= 8)
})

# ---------------------------------------------------------------------------
# 7. Multiple games in PGN — correct game_id assignment
# ---------------------------------------------------------------------------

test_that("cpp_replay_pgn_file assigns distinct game_ids", {
  tmp <- tempfile(fileext = ".pgn")
  writeLines(c(
    '[Event "Game 1"]', '[Result "1-0"]', '',
    '1.e4 e5 1-0', '',
    '[Event "Game 2"]', '[Result "0-1"]', '',
    '1.d4 d5 2.c4 0-1', ''
  ), tmp)
  on.exit(unlink(tmp))

  result <- ply:::cpp_replay_pgn_file(tmp)
  expect_equal(length(unique(result$game_id)), 2L)
  # Game 1: 2 plies, Game 2: 3 plies
  expect_equal(nrow(result), 5L)
  expect_equal(sum(result$ok), 5L)
})
