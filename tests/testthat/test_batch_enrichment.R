# test_batch_enrichment.R — cpp_enrich_batch correctness

context("C++ Engine — Batch Enrichment")

test_that("enrich_batch returns expected columns", {
  fens <- c("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
  ucis <- c("e2e4")
  result <- cpp_enrich_batch(fens, ucis)

  expected_cols <- c(
    "is_capture", "is_castling", "is_promotion", "is_en_passant",
    "gives_check", "gives_discovered_check", "in_check",
    "material_bal", "num_pieces", "legal_move_count",
    "moved_piece_value", "captured_piece_value",
    "is_checkmate", "is_stalemate",
    "num_captures_avail", "num_checks_avail", "can_castle",
    "max_capture_value", "num_sacrifices_avail", "num_winning_caps_avail",
    "max_sacrifice_gap"
  )
  for (col in expected_cols) {
    expect_true(col %in% names(result), info = paste("Missing column:", col))
  }
})

test_that("starting position e2e4 is not a capture", {
  result <- cpp_enrich_batch(
    "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1",
    "e2e4"
  )
  expect_false(as.logical(result$is_capture))
  expect_false(as.logical(result$is_castling))
  expect_false(as.logical(result$is_promotion))
  expect_false(as.logical(result$gives_check))
  expect_false(as.logical(result$in_check))
})

test_that("starting position has 32 pieces and 0 material balance", {
  result <- cpp_enrich_batch(
    "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1",
    "e2e4"
  )
  expect_equal(result$num_pieces, 32L)
  expect_equal(result$material_bal, 0L)
})

test_that("starting position has 20 legal moves", {
  result <- cpp_enrich_batch(
    "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1",
    "e2e4"
  )
  expect_equal(result$legal_move_count, 20L)
})

test_that("capture move is detected correctly", {
  # Scandinavian: 1.e4 d5 2.exd5
  result <- cpp_enrich_batch(
    "rnbqkbnr/ppp1pppp/8/3p4/4P3/8/PPPP1PPP/RNBQKBNR w KQkq d6 0 2",
    "e4d5"
  )
  expect_true(as.logical(result$is_capture))
  expect_equal(result$moved_piece_value, 1L)    # pawn
  expect_equal(result$captured_piece_value, 1L)  # captures pawn
})

test_that("castling move is detected correctly", {
  result <- cpp_enrich_batch(
    "r3k2r/pppppppp/8/8/8/8/PPPPPPPP/R3K2R w KQkq - 0 1",
    "e1g1"
  )
  expect_true(as.logical(result$is_castling))
})

test_that("check-giving move is detected", {
  # Scholar's mate Qf3-f7 gives check
  result <- cpp_enrich_batch(
    "rnbqkbnr/pppp1ppp/8/4p3/2B1P3/5Q2/PPPP1PPP/RNB1K1NR w KQkq - 0 3",
    "f3f7"
  )
  expect_true(as.logical(result$gives_check))
})

test_that("batch enrichment handles multiple positions", {
  fens <- c(
    "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1",
    "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1",
    "rnbqkbnr/ppp1pppp/8/3p4/4P3/8/PPPP1PPP/RNBQKBNR w KQkq d6 0 2"
  )
  ucis <- c("e2e4", "d7d5", "e4d5")
  result <- cpp_enrich_batch(fens, ucis)
  expect_equal(nrow(result), 3L)
})

test_that("position with material imbalance reports correct balance", {
  # White has extra queen (white: K+Q, black: K only)
  result <- cpp_enrich_batch(
    "4k3/8/8/8/8/8/8/3QK3 w - - 0 1",
    "d1d2"
  )
  expect_equal(result$material_bal, 9L)  # queen = 9
})
