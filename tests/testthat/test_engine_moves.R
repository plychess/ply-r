# test_engine_moves.R — Move generation edge cases, special moves

context("C++ Engine — Special Moves")

# --- Castling ---

test_that("white can castle kingside when path clear", {
  state <- cpp_parse_fen("r3k2r/pppppppp/8/8/8/8/PPPPPPPP/R3K2R w KQkq - 0 1")
  moves <- cpp_generate_legal_moves(state)
  uci_moves <- vapply(moves, cpp_ply_to_uci, character(1))
  expect_true("e1g1" %in% uci_moves)  # kingside
  expect_true("e1c1" %in% uci_moves)  # queenside
})

test_that("castling blocked when path is attacked", {
  # Black bishop on c3 attacks f6..e5..d4..c3 and a1,b2,d4,e5,f6,g7,h8
  # More importantly: bishop on g3 attacks f2,e1 — so f1 is not attacked but e1 is in check? No.
  # Use black rook on f8 to attack f1: white can't castle kingside through f1
  state <- cpp_parse_fen("4kr2/8/8/8/8/8/8/R3K2R w KQ - 0 1")
  moves <- cpp_generate_legal_moves(state)
  uci_moves <- vapply(moves, cpp_ply_to_uci, character(1))
  expect_false("e1g1" %in% uci_moves)  # kingside blocked (rook attacks f1)
})

test_that("castling blocked when king in check", {
  state <- cpp_parse_fen("4k3/8/8/8/4r3/8/8/R3K2R w KQ - 0 1")
  moves <- cpp_generate_legal_moves(state)
  uci_moves <- vapply(moves, cpp_ply_to_uci, character(1))
  expect_false("e1g1" %in% uci_moves)
  expect_false("e1c1" %in% uci_moves)
})

# --- En Passant ---

test_that("en passant capture is available", {
  # After 1.e4 d5 2.e5 f5, white can play exf6 e.p.
  state <- cpp_parse_fen("rnbqkbnr/ppp1p1pp/8/3pPp2/8/8/PPPP1PPP/RNBQKBNR w KQkq f6 0 3")
  moves <- cpp_generate_legal_moves(state)
  uci_moves <- vapply(moves, cpp_ply_to_uci, character(1))
  expect_true("e5f6" %in% uci_moves)
})

# --- Promotion ---

test_that("pawn promotion generates promotion moves", {
  # White pawn on a7, black king on e8 (not blocking), white king on e1
  state <- cpp_parse_fen("4k3/P7/8/8/8/8/8/4K3 w - - 0 1")
  moves <- cpp_generate_legal_moves(state)
  uci_moves <- vapply(moves, cpp_ply_to_uci, character(1))
  # Should have promotion variants (a7a8q, a7a8r, a7a8b, a7a8n)
  promo_moves <- grep("^a7a8", uci_moves, value = TRUE)
  expect_gte(length(promo_moves), 1L)
})

# --- Stalemate ---

test_that("stalemate detected: king has no legal moves but not in check", {
  # Classic stalemate: black king in corner, not in check, no moves
  state <- cpp_parse_fen("k7/2Q5/1K6/8/8/8/8/8 b - - 0 1")
  moves <- cpp_generate_legal_moves(state)
  expect_equal(length(moves), 0L)
  expect_false(cpp_in_check(state, 1L))  # not in check = stalemate
})

# --- Insufficient Material ---

test_that("K vs K is insufficient material", {
  state <- cpp_parse_fen("4k3/8/8/8/8/8/8/4K3 w - - 0 1")
  expect_true(cpp_is_insufficient_material(state))
})

# --- Pin Detection ---

test_that("pinned piece cannot move off pin line", {
  # White bishop on d2 pins: if black rook on e3 attacks white king on c1 through d2
  # Actually let's use: white knight on d4 pinned by black rook on d8, king on d1
  state <- cpp_parse_fen("3r4/8/8/8/3N4/8/8/3K4 w - - 0 1")
  moves <- cpp_generate_legal_moves(state)
  uci_moves <- vapply(moves, cpp_ply_to_uci, character(1))
  # Knight on d4 is pinned along d-file by rook on d8 — cannot move at all
  knight_moves <- grep("^d4", uci_moves, value = TRUE)
  expect_equal(length(knight_moves), 0L)
})
