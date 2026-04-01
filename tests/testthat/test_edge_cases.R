# test_edge_cases.R — Tricky chess positions that expose engine bugs

context("Chess Edge Cases")

# ---------------------------------------------------------------------------
# Double check — only king can move
# ---------------------------------------------------------------------------

test_that("double check: only king moves are legal", {
  # White rook on d1 and bishop on b5 deliver double check on black king e8
  # after Rd1-d8+ (rook gives check on d8, bishop gives discovered check from b5)
  # Set up a position where black is in double check:
  # Black king on e8, white rook on d8 and white bishop on b5 both check
  state <- cpp_parse_fen("3Rk3/8/8/1B6/8/8/8/4K3 b - - 0 1")
  in_check <- cpp_in_check(state, 1L)  # black in check
  expect_true(in_check)

  moves <- cpp_generate_legal_moves(state)
  uci_moves <- vapply(moves, cpp_ply_to_uci, character(1))
  # All moves must start with "e8" (king moves only)
  king_moves <- grep("^e8", uci_moves, value = TRUE)
  expect_equal(length(moves), length(king_moves))
  expect_gt(length(moves), 0L)  # king must have at least one escape
})

# ---------------------------------------------------------------------------
# En passant that would leave king in check (illegal)
# ---------------------------------------------------------------------------

test_that("en passant is illegal when it exposes king to check", {
  # White king on a5, white pawn on b5, black pawn on c5 (just advanced 2 squares)
  # Black rook on h5. If white plays bxc6 e.p., the b5 pawn and c5 pawn both
  # disappear from rank 5, exposing the white king to the rook on h5.
  state <- cpp_parse_fen("4k3/8/8/K1pP3r/8/8/8/8 w - c6 0 2")
  moves <- cpp_generate_legal_moves(state)
  uci_moves <- vapply(moves, cpp_ply_to_uci, character(1))
  # d5c6 (en passant) should NOT be in legal moves
  expect_false("d5c6" %in% uci_moves,
    info = "En passant should be illegal: exposes king to rook on h5")
})

# ---------------------------------------------------------------------------
# Discovered check via en passant
# ---------------------------------------------------------------------------

test_that("en passant can deliver discovered check", {
  # White pawn on e5, black pawn on d5 (just moved d7-d5)
  # White bishop on a1 — after exd6 e.p., bishop discovers check on black king g7
  # Wait, let me set up correctly:
  # White pawn on e5, black pawn on f5 (just played f7-f5).
  # White queen on d1. After exf6 e.p., the e-file opens... hmm.
  # Simpler: white rook on a5, white pawn on b5, black pawn on c5 (just played c7-c5)
  # Black king on h5. After bxc6 e.p., rook on a5 discovers check on king h5.
  state <- cpp_parse_fen("8/8/8/R1pP3k/8/8/8/4K3 w - c6 0 2")
  moves <- cpp_generate_legal_moves(state)
  uci_moves <- vapply(moves, cpp_ply_to_uci, character(1))
  # d5c6 en passant should be legal — and gives discovered check
  expect_true("d5c6" %in% uci_moves)

  # Verify it gives check
  ply <- cpp_uci_to_ply(state, "d5c6")
  new_state <- cpp_apply_ply(state, ply)
  expect_true(cpp_in_check(new_state, 1L))  # black king in check
})

# ---------------------------------------------------------------------------
# Underpromotion to knight giving check
# ---------------------------------------------------------------------------

test_that("underpromotion to knight is a legal move", {
  # White pawn on g7, black king on h8 — pawn promotes
  # Knight promotion g7g8n or g7f8n might be interesting
  # Let's use: pawn on e7, king on e1, black king on d8
  # e7-e8=N would check the king on d8? No, knight on e8 doesn't attack d8.
  # Pawn on f7, black king on d8: f7f8n — knight on f8 doesn't check d8.
  # Pawn on c7, black king on e8: c7c8n — knight on c8 doesn't check e8.
  # Pawn on f7, black king on e8: f7f8n — nope. f7g8n? Knight on g8 checks e7 not e8.
  # Pawn on d7, black king on e8: d7d8n — no. d7c8n? No.
  # Simple: pawn on e7, black king on g8: e7f8n — nope.
  # Pawn on f7, black king on h8: f7g8n — knight attacks f6,h6,e7 — no.
  # f7f8n — knight on f8 attacks d7,e6,g6,h7 — no check on h8.
  # Let's just test that underpromotion exists as a legal move
  state <- cpp_parse_fen("4k3/P7/8/8/8/8/8/4K3 w - - 0 1")
  moves <- cpp_generate_legal_moves(state)
  uci_moves <- vapply(moves, cpp_ply_to_uci, character(1))
  # Should have a7a8n (knight promotion)
  expect_true("a7a8n" %in% uci_moves, info = "Knight underpromotion should be legal")
  # Also verify queen promotion exists
  expect_true("a7a8q" %in% uci_moves, info = "Queen promotion should be legal")
})

test_that("knight promotion can give check", {
  # White pawn on b7, black king on a8 is blocked. Let's think...
  # Pawn on f7, black king on d8: f7f8n — knight on f8 attacks d7,e6,g6,h7 — no.
  # Pawn on c7, black king on a8: c7c8n — knight on c8 attacks a7,b6,d6,e7 — no.
  # Pawn on c7, black king on b5: c7c8n — knight c8 attacks a7,b6,d6,e7. No.
  # Pawn on c7, black king on d6: c7c8n — knight c8 attacks d6! Yes!
  # But king on d6 needs to be legal. Let's check: white pawn on c7, black king on d6, white king on a1
  state <- cpp_parse_fen("8/2P5/3k4/8/8/8/8/K7 w - - 0 1")
  ply <- cpp_uci_to_ply(state, "c7c8n")
  new_state <- cpp_apply_ply(state, ply)
  expect_true(cpp_in_check(new_state, 1L))  # knight on c8 checks king on d6
})

# ---------------------------------------------------------------------------
# Castling: into check vs through check vs out of check
# ---------------------------------------------------------------------------

test_that("cannot castle into check (destination square attacked)", {
  # Black rook on g8 attacks g1 — white cannot castle kingside (king ends on g1)
  state <- cpp_parse_fen("6r1/4k3/8/8/8/8/8/R3K2R w KQ - 0 1")
  moves <- cpp_generate_legal_moves(state)
  uci_moves <- vapply(moves, cpp_ply_to_uci, character(1))
  expect_false("e1g1" %in% uci_moves, info = "Cannot castle into check")
  # But queenside should still be OK (c1 not attacked)
  expect_true("e1c1" %in% uci_moves, info = "Queenside castling should be legal")
})

test_that("cannot castle through check (intermediate square attacked)", {
  # Black rook on f8 attacks f1 — white cannot castle kingside (king passes f1)
  state <- cpp_parse_fen("4kr2/8/8/8/8/8/8/R3K2R w KQ - 0 1")
  moves <- cpp_generate_legal_moves(state)
  uci_moves <- vapply(moves, cpp_ply_to_uci, character(1))
  expect_false("e1g1" %in% uci_moves, info = "Cannot castle through check")
})

test_that("cannot castle out of check", {
  # Black rook on e8 gives check — black king on h8 (off e-file, not blocking)
  state <- cpp_parse_fen("4r2k/8/8/8/8/8/8/R3K2R w KQ - 0 1")
  in_check <- cpp_in_check(state, 0L)
  expect_true(in_check)
  moves <- cpp_generate_legal_moves(state)
  uci_moves <- vapply(moves, cpp_ply_to_uci, character(1))
  expect_false("e1g1" %in% uci_moves, info = "Cannot castle out of check")
  expect_false("e1c1" %in% uci_moves, info = "Cannot castle out of check")
})

# ---------------------------------------------------------------------------
# Halfmove clock (50-move rule tracking)
# ---------------------------------------------------------------------------

test_that("halfmove clock is preserved in FEN", {
  # Position with halfmove clock = 99 (one move from 50-move rule)
  fen <- "4k3/8/8/8/8/8/8/4K3 w - - 99 100"
  state <- cpp_parse_fen(fen)
  fen_out <- cpp_state_to_fen(state)
  parts <- strsplit(fen_out, " ")[[1]]
  expect_equal(parts[5], "99")  # halfmove clock preserved
})

test_that("halfmove clock resets on pawn move", {
  fen <- "4k3/8/8/8/8/8/4P3/4K3 w - - 50 60"
  state <- cpp_parse_fen(fen)
  ply <- cpp_uci_to_ply(state, "e2e4")
  new_state <- cpp_apply_ply(state, ply)
  new_fen <- cpp_state_to_fen(new_state)
  parts <- strsplit(new_fen, " ")[[1]]
  expect_equal(parts[5], "0")  # reset after pawn move
})

test_that("halfmove clock resets on capture", {
  # White knight on d4 captures black pawn on e6 (valid L-shape: d4 → e6)
  fen <- "4k3/8/4p3/8/3N4/8/8/4K3 w - - 45 50"
  state <- cpp_parse_fen(fen)
  ply <- cpp_uci_to_ply(state, "d4e6")
  new_state <- cpp_apply_ply(state, ply)
  new_fen <- cpp_state_to_fen(new_state)
  parts <- strsplit(new_fen, " ")[[1]]
  expect_equal(parts[5], "0")  # reset after capture
})

test_that("halfmove clock increments on quiet move", {
  fen <- "4k3/8/8/8/8/8/8/4K3 w - - 10 50"
  state <- cpp_parse_fen(fen)
  ply <- cpp_uci_to_ply(state, "e1d1")
  new_state <- cpp_apply_ply(state, ply)
  new_fen <- cpp_state_to_fen(new_state)
  parts <- strsplit(new_fen, " ")[[1]]
  expect_equal(parts[5], "11")  # incremented
})
