# test_engine_core.R — C++ engine: FEN parsing, legal moves, check detection

context("C++ Engine — Core")

# --- FEN Parsing & Round-Trip ---

test_that("starting position FEN round-trips correctly", {
  state <- cpp_parse_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
  fen <- cpp_state_to_fen(state)
  expect_equal(fen, "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
})

test_that("parse FEN detects white to move", {
  state <- cpp_parse_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
  expect_equal(state$side, 0L)  # 0 = white
})

test_that("parse FEN detects black to move", {
  state <- cpp_parse_fen("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1")
  expect_equal(state$side, 1L)  # 1 = black
})

test_that("FEN with castling rights parsed correctly", {
  state <- cpp_parse_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
  expect_equal(state$castling, 15L)  # all 4 castling rights = 0b1111 = 15
})

test_that("FEN with no castling rights parsed correctly", {
  state <- cpp_parse_fen("4k3/8/8/8/8/8/8/4K3 w - - 0 1")
  expect_equal(state$castling, 0L)
})

# --- Legal Move Generation ---

test_that("starting position has 20 legal moves", {
  state <- cpp_parse_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
  moves <- cpp_generate_legal_moves(state)
  expect_equal(length(moves), 20L)
})

test_that("position after 1.e4 has 20 legal moves for black", {
  state <- cpp_parse_fen("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1")
  moves <- cpp_generate_legal_moves(state)
  expect_equal(length(moves), 20L)
})

test_that("back-rank mate: king has 0 legal moves when checkmated", {
  # White rook delivers mate on 8th rank, black king boxed in by own pawns
  state <- cpp_parse_fen("6k1/5ppp/8/8/8/8/8/R3K3 w Q - 0 1")
  # Play Ra8# — rook to a8 gives checkmate
  ply <- cpp_uci_to_ply(state, "a1a8")
  new_state <- cpp_apply_ply(state, ply)
  moves <- cpp_generate_legal_moves(new_state)
  expect_equal(length(moves), 0L)
  expect_true(cpp_in_check(new_state, 1L))
})

# --- Check Detection ---

test_that("starting position: neither side in check", {
  state <- cpp_parse_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
  expect_false(cpp_in_check(state, 0L))  # white
  expect_false(cpp_in_check(state, 1L))  # black
})

test_that("scholar's mate: black king is in check", {
  state <- cpp_parse_fen("rnb1kbnr/pppp1Qpp/8/4p3/2B1P3/8/PPPP1PPP/RNB1K1NR b KQkq - 0 4")
  expect_true(cpp_in_check(state, 1L))   # black in check
  expect_false(cpp_in_check(state, 0L))  # white not in check
})

test_that("king attacked by knight is in check", {
  state <- cpp_parse_fen("4k3/8/5N2/8/8/8/8/4K3 b - - 0 1")
  expect_true(cpp_in_check(state, 1L))
})

# --- Move Application ---

test_that("applying e2e4 produces correct FEN", {
  state <- cpp_parse_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
  ply <- cpp_uci_to_ply(state, "e2e4")
  new_state <- cpp_apply_ply(state, ply)
  fen <- cpp_state_to_fen(new_state)
  expect_equal(fen, "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1")
})

test_that("applying move toggles side to move", {
  state <- cpp_parse_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
  ply <- cpp_uci_to_ply(state, "e2e4")
  new_state <- cpp_apply_ply(state, ply)
  expect_equal(new_state$side, 1L)  # now black's turn
})

# --- UCI Conversion ---

test_that("UCI to ply round-trips", {
  state <- cpp_parse_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
  ply <- cpp_uci_to_ply(state, "e2e4")
  uci_back <- cpp_ply_to_uci(ply)
  expect_equal(uci_back, "e2e4")
})

test_that("castling move UCI encodes as king move", {
  # White can castle kingside
  state <- cpp_parse_fen("r3k2r/pppppppp/8/8/8/8/PPPPPPPP/R3K2R w KQkq - 0 1")
  ply <- cpp_uci_to_ply(state, "e1g1")
  uci_back <- cpp_ply_to_uci(ply)
  expect_equal(uci_back, "e1g1")
})

# --- Position Validation ---

test_that("starting position is valid", {
  state <- cpp_parse_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
  expect_true(cpp_validate_position(state))
})

test_that("position with two white kings is invalid", {
  # Two white kings — should fail validation
  state <- cpp_parse_fen("4k3/8/8/8/8/8/8/3KK3 w - - 0 1")
  expect_false(cpp_validate_position(state))
})
