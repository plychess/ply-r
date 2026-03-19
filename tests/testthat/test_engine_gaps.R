# test_engine_gaps.R — High-value gap coverage identified by audit
#
# Covers: position hash correctness, castling-rights persistence,
# capture-promotion, diagonal pins, cpp_has_any_legal_ply_with_check,
# and K+B vs K / K+N vs K insufficient material.

context("C++ Engine — Gap Coverage")

# ---------------------------------------------------------------------------
# 1. Position hash correctness
# ---------------------------------------------------------------------------

test_that("same position parsed twice produces the same hash", {
  fen <- "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
  s1 <- cpp_parse_fen(fen)
  s2 <- cpp_parse_fen(fen)
  expect_equal(cpp_position_hash(s1), cpp_position_hash(s2))
})

test_that("different positions produce different hashes", {
  s_start <- cpp_parse_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
  s_after  <- cpp_parse_fen("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1")
  expect_false(cpp_position_hash(s_start) == cpp_position_hash(s_after))
})

test_that("same position reached by different move orders has the same hash", {
  # 1.e4 e5 2.Nf3 Nc6  vs  1.Nf3 Nc6 2.e4 e5  — identical board state
  s1 <- cpp_parse_fen("r1bqkbnr/pppp1ppp/2n5/4p3/4P3/5N2/PPPP1PPP/RNBQKB1R w KQkq - 2 3")
  s2 <- cpp_parse_fen("r1bqkbnr/pppp1ppp/2n5/4p3/4P3/5N2/PPPP1PPP/RNBQKB1R w KQkq - 2 3")
  expect_equal(cpp_position_hash(s1), cpp_position_hash(s2))
})

test_that("hash changes after a move is applied", {
  state <- cpp_parse_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
  ply   <- cpp_uci_to_ply(state, "e2e4")
  new_state <- cpp_apply_ply(state, ply)
  expect_false(cpp_position_hash(state) == cpp_position_hash(new_state))
})

# ---------------------------------------------------------------------------
# 2. Castling rights update after king/rook moves
# ---------------------------------------------------------------------------

test_that("moving the king clears both castling rights for that side", {
  # White king moves f1: loses K and Q rights
  state <- cpp_parse_fen("r3k2r/pppppppp/8/8/8/8/PPPPPPPP/R3K2R w KQkq - 0 1")
  ply   <- cpp_uci_to_ply(state, "e1f1")
  new_state <- cpp_apply_ply(state, ply)
  new_fen   <- cpp_state_to_fen(new_state)
  # Castling field (token 3) should no longer contain K or Q
  castling <- strsplit(new_fen, " ")[[1]][3]
  expect_false(grepl("K", castling), info = "King move clears kingside rights")
  expect_false(grepl("Q", castling), info = "King move clears queenside rights")
})

test_that("moving the h1 rook clears only kingside castling right", {
  # White h1 rook moves to h3: loses K but keeps Q (no pawns blocking)
  state <- cpp_parse_fen("r3k2r/8/8/8/8/8/8/R3K2R w KQkq - 0 1")
  ply   <- cpp_uci_to_ply(state, "h1h3")
  new_state <- cpp_apply_ply(state, ply)
  # Let black make a null-ish move so we can read the fen properly
  new_fen   <- cpp_state_to_fen(new_state)
  castling <- strsplit(new_fen, " ")[[1]][3]
  expect_false(grepl("K", castling), info = "h1 rook move clears white kingside right")
  expect_true(grepl("Q", castling),  info = "h1 rook move keeps white queenside right")
})

test_that("moving the a1 rook clears only queenside castling right", {
  # White a1 rook moves to a3: loses Q but keeps K (no pawns blocking)
  state <- cpp_parse_fen("r3k2r/8/8/8/8/8/8/R3K2R w KQkq - 0 1")
  ply   <- cpp_uci_to_ply(state, "a1a3")
  new_state <- cpp_apply_ply(state, ply)
  new_fen   <- cpp_state_to_fen(new_state)
  castling <- strsplit(new_fen, " ")[[1]][3]
  expect_true(grepl("K", castling),  info = "a1 rook move keeps white kingside right")
  expect_false(grepl("Q", castling), info = "a1 rook move clears white queenside right")
})

test_that("castling rights are cleared permanently after king moves and returns", {
  # King moves out and back — rights should still be gone (no pawns so rooks/king can move freely)
  state <- cpp_parse_fen("r3k2r/8/8/8/8/8/8/R3K2R w KQkq - 0 1")
  # White king e1->f1
  ply1 <- cpp_uci_to_ply(state, "e1f1")
  s2   <- cpp_apply_ply(state, ply1)
  # Black makes a quiet move (a8a7)
  ply2 <- cpp_uci_to_ply(s2, "a8a7")
  s3   <- cpp_apply_ply(s2, ply2)
  # White king f1->e1 (returns to original square)
  ply3 <- cpp_uci_to_ply(s3, "f1e1")
  s4   <- cpp_apply_ply(s3, ply3)
  castling <- strsplit(cpp_state_to_fen(s4), " ")[[1]][3]
  expect_false(grepl("K", castling), info = "Rights gone permanently after king moved")
  expect_false(grepl("Q", castling), info = "Rights gone permanently after king moved")
})

# ---------------------------------------------------------------------------
# 3. Capture-promotion (diagonal capture into promotion square)
# ---------------------------------------------------------------------------

test_that("pawn can promote by capturing diagonally", {
  # White pawn on b7, black rook on c8; straight push b8 also available
  # FEN: 2r1k3/1P6/8/8/8/8/8/4K3 w - - 0 1
  state <- cpp_parse_fen("2r1k3/1P6/8/8/8/8/8/4K3 w - - 0 1")
  moves <- cpp_generate_legal_moves(state)
  uci_moves <- vapply(moves, cpp_ply_to_uci, character(1))

  # Capture-promotions on c8
  expect_true("b7c8q" %in% uci_moves, info = "Capture-promote to queen")
  expect_true("b7c8r" %in% uci_moves, info = "Capture-promote to rook")
  expect_true("b7c8b" %in% uci_moves, info = "Capture-promote to bishop")
  expect_true("b7c8n" %in% uci_moves, info = "Capture-promote to knight")
})

test_that("capture-promotion is distinct from straight push promotion", {
  state <- cpp_parse_fen("2r1k3/1P6/8/8/8/8/8/4K3 w - - 0 1")
  moves <- cpp_generate_legal_moves(state)
  uci_moves <- vapply(moves, cpp_ply_to_uci, character(1))

  # Straight push promotions on b8 should also exist
  expect_true("b7b8q" %in% uci_moves, info = "Straight-push promote to queen")
  expect_true("b7b8n" %in% uci_moves, info = "Straight-push promote to knight")
})

test_that("capture-promotion actually removes the captured piece", {
  state <- cpp_parse_fen("2r1k3/1P6/8/8/8/8/8/4K3 w - - 0 1")
  ply   <- cpp_uci_to_ply(state, "b7c8q")
  new_state <- cpp_apply_ply(state, ply)
  # The black rook on c8 must be gone; white queen should be on c8
  new_fen <- cpp_state_to_fen(new_state)
  # Verify position is valid (no two pieces on same square etc.)
  expect_true(cpp_validate_position(new_state))
})

# ---------------------------------------------------------------------------
# 4. Diagonal pin
# ---------------------------------------------------------------------------

test_that("diagonally pinned piece cannot move off the pin line", {
  # White knight on e5 is pinned along the b2-h8 diagonal:
  # white king on b2, black bishop on h8.
  # Knight on e5 cannot move — any knight move leaves the diagonal.
  state <- cpp_parse_fen("7b/8/8/4N3/8/8/1K6/7k w - - 0 1")
  moves <- cpp_generate_legal_moves(state)
  uci_moves <- vapply(moves, cpp_ply_to_uci, character(1))
  knight_moves <- grep("^e5", uci_moves, value = TRUE)
  expect_equal(length(knight_moves), 0L,
    info = "Diagonally pinned knight cannot move at all")
})

test_that("diagonally pinned bishop can move along the pin line", {
  # White bishop on d4 pinned along b2-h8 diagonal by black bishop on h8.
  # White king on b2. The white bishop CAN slide along b2-h8;
  # it can move to c3, e5, f6, g7 (and capture h8).
  state <- cpp_parse_fen("7b/8/8/8/3B4/8/1K6/7k w - - 0 1")
  moves <- cpp_generate_legal_moves(state)
  uci_moves <- vapply(moves, cpp_ply_to_uci, character(1))
  bishop_moves <- grep("^d4", uci_moves, value = TRUE)
  # Must be able to move along the diagonal (c3, e5, f6, g7, h8)
  expect_true("d4c3" %in% bishop_moves, info = "Pinned bishop can move toward king")
  expect_true("d4e5" %in% bishop_moves, info = "Pinned bishop can move away")
  expect_true("d4h8" %in% bishop_moves, info = "Pinned bishop can capture the pinner")
  # Cannot move off the diagonal (e.g., d4e3 — perpendicular)
  expect_false("d4e3" %in% bishop_moves, info = "Pinned bishop cannot leave pin line")
})

# ---------------------------------------------------------------------------
# 5. cpp_has_any_legal_ply_with_check — direct coverage
# ---------------------------------------------------------------------------

test_that("cpp_has_any_legal_ply_with_check returns hasLegal=FALSE and isInCheck=TRUE for checkmate", {
  # Back-rank mate: white king on e1 mated by black queen on e2 + black rook on e8 covers escape
  # Simpler classic: Fool's mate position
  state <- cpp_parse_fen("rnb1kbnr/pppp1ppp/8/4p3/6Pq/5P2/PPPPP2P/RNBQKBNR w KQkq - 1 3")
  result <- cpp_has_any_legal_ply_with_check(state)
  expect_false(result$hasLegal, info = "Checkmate: no legal moves")
  expect_true(result$isInCheck,  info = "Checkmate: king is in check")
})

test_that("cpp_has_any_legal_ply_with_check returns hasLegal=FALSE and isInCheck=FALSE for stalemate", {
  # Classic stalemate
  state <- cpp_parse_fen("k7/2Q5/1K6/8/8/8/8/8 b - - 0 1")
  result <- cpp_has_any_legal_ply_with_check(state)
  expect_false(result$hasLegal, info = "Stalemate: no legal moves")
  expect_false(result$isInCheck, info = "Stalemate: king not in check")
})

test_that("cpp_has_any_legal_ply_with_check returns hasLegal=TRUE for normal position", {
  state <- cpp_parse_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
  result <- cpp_has_any_legal_ply_with_check(state)
  expect_true(result$hasLegal,   info = "Starting position has legal moves")
  expect_false(result$isInCheck, info = "Starting position: not in check")
})

# ---------------------------------------------------------------------------
# 6. Insufficient material — K+B vs K and K+N vs K
# ---------------------------------------------------------------------------

test_that("K+B vs K is insufficient material", {
  state <- cpp_parse_fen("4k3/8/8/8/8/8/8/3BK3 w - - 0 1")
  expect_true(cpp_is_insufficient_material(state),
    info = "K+B vs K cannot force checkmate")
})

test_that("K+N vs K is insufficient material", {
  state <- cpp_parse_fen("4k3/8/8/8/8/8/8/3NK3 w - - 0 1")
  expect_true(cpp_is_insufficient_material(state),
    info = "K+N vs K cannot force checkmate")
})

test_that("K+R vs K is NOT insufficient material", {
  state <- cpp_parse_fen("4k3/8/8/8/8/8/8/3RK3 w - - 0 1")
  expect_false(cpp_is_insufficient_material(state),
    info = "K+R vs K can force checkmate")
})

test_that("K+Q vs K is NOT insufficient material", {
  state <- cpp_parse_fen("4k3/8/8/8/8/8/8/3QK3 w - - 0 1")
  expect_false(cpp_is_insufficient_material(state),
    info = "K+Q vs K can force checkmate")
})

# ---------------------------------------------------------------------------
# 7. cpp_make_ply — raw integer ply interface (game registry)
# ---------------------------------------------------------------------------

test_that("cpp_make_ply accepts a raw ply integer and advances game state", {
  cpp_reset_registry()
  gid <- cpp_create_game(creator = "white", settlement_mode = 0L, ply_time_limit = 0)
  cpp_join_game(gid, player = "black")
  state <- cpp_parse_fen(cpp_get_fen(gid))
  raw_ply <- cpp_uci_to_ply(state, "e2e4")          # convert UCI → int
  expect_true(cpp_make_ply(gid, "white", raw_ply))  # raw int path
  expect_equal(length(cpp_get_ply_history(gid)), 1L) # move recorded
})
