# test_san_replay.R — SAN parsing and game replay via C++ engine

context("C++ Engine — SAN Parsing & Game Replay")

# --- SAN to UCI ---

test_that("SAN 'e4' resolves to e2e4 from starting position", {
  state <- cpp_parse_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
  uci <- cpp_san_to_uci(state, "e4")
  expect_equal(uci, "e2e4")
})

test_that("SAN 'Nf3' resolves to g1f3 from starting position", {
  state <- cpp_parse_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
  uci <- cpp_san_to_uci(state, "Nf3")
  expect_equal(uci, "g1f3")
})

test_that("SAN 'O-O' resolves to kingside castling", {
  state <- cpp_parse_fen("r3k2r/pppppppp/8/8/8/8/PPPPPPPP/R3K2R w KQkq - 0 1")
  uci <- cpp_san_to_uci(state, "O-O")
  expect_equal(uci, "e1g1")
})

test_that("SAN 'O-O-O' resolves to queenside castling", {
  state <- cpp_parse_fen("r3k2r/pppppppp/8/8/8/8/PPPPPPPP/R3K2R w KQkq - 0 1")
  uci <- cpp_san_to_uci(state, "O-O-O")
  expect_equal(uci, "e1c1")
})

# --- Full Game Replay ---

test_that("replay_game handles Scholar's Mate", {
  # 1.e4 e5 2.Bc4 Nc6 3.Qh5 Nf6 4.Qxf7#
  moves <- c("e4", "e5", "Bc4", "Nc6", "Qh5", "Nf6", "Qxf7#")
  result <- cpp_replay_game(moves)
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), length(moves))
  expect_true(all(result$ok))
})

test_that("replay_game returns correct number of rows", {
  # Italian Game opening: 1.e4 e5 2.Nf3 Nc6 3.Bc4
  moves <- c("e4", "e5", "Nf3", "Nc6", "Bc4")
  result <- cpp_replay_game(moves)
  expect_equal(nrow(result), length(moves))
  expect_true("fen" %in% names(result))
  expect_true("uci_move" %in% names(result))
})
