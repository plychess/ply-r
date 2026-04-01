# test_perft_deep.R — Deep perft tests against published node counts
#
# Reference: https://www.chessprogramming.org/Perft_Results

context("Perft — Deep Correctness")

# Helper: compute perft to given depth
perft <- function(state, depth) {
  moves <- cpp_generate_legal_moves(state)
  if (depth == 1L) return(length(moves))
  count <- 0L
  for (m in moves) {
    child <- cpp_apply_ply(state, m)
    count <- count + perft(child, depth - 1L)
  }
  count
}

# ---------------------------------------------------------------------------
# Position 1: Starting position
# ---------------------------------------------------------------------------

test_that("starting position perft(4) = 197,281", {
  state <- cpp_parse_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
  expect_equal(perft(state, 4L), 197281L)
})

# ---------------------------------------------------------------------------
# Position 2: Kiwipete — castling, en passant, promotions
# ---------------------------------------------------------------------------

test_that("Kiwipete perft(1) = 48", {
  state <- cpp_parse_fen("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1")
  expect_equal(perft(state, 1L), 48L)
})

test_that("Kiwipete perft(2) = 2,039", {
  state <- cpp_parse_fen("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1")
  expect_equal(perft(state, 2L), 2039L)
})

test_that("Kiwipete perft(3) = 97,862", {
  state <- cpp_parse_fen("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1")
  expect_equal(perft(state, 3L), 97862L)
})

# ---------------------------------------------------------------------------
# Position 3: catches en passant + promotion bugs
# ---------------------------------------------------------------------------

test_that("position 3 perft(1) = 14", {
  state <- cpp_parse_fen("8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1")
  expect_equal(perft(state, 1L), 14L)
})

test_that("position 3 perft(2) = 191", {
  state <- cpp_parse_fen("8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1")
  expect_equal(perft(state, 2L), 191L)
})

test_that("position 3 perft(3) = 2,812", {
  state <- cpp_parse_fen("8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1")
  expect_equal(perft(state, 3L), 2812L)
})

test_that("position 3 perft(4) = 43,238", {
  state <- cpp_parse_fen("8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1")
  expect_equal(perft(state, 4L), 43238L)
})

# ---------------------------------------------------------------------------
# Position 4: catches discovered check bugs
# ---------------------------------------------------------------------------

test_that("position 4 perft(1) = 6", {
  state <- cpp_parse_fen("r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1")
  expect_equal(perft(state, 1L), 6L)
})

test_that("position 4 perft(2) = 264", {
  state <- cpp_parse_fen("r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1")
  expect_equal(perft(state, 2L), 264L)
})

test_that("position 4 perft(3) = 9,467", {
  state <- cpp_parse_fen("r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1")
  expect_equal(perft(state, 3L), 9467L)
})

test_that("position 4 perft(4) = 422,333", {
  state <- cpp_parse_fen("r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1")
  expect_equal(perft(state, 4L), 422333L)
})

# ---------------------------------------------------------------------------
# Position 5: another tricky position (mirrored)
# ---------------------------------------------------------------------------

test_that("position 5 perft(1) = 44", {
  state <- cpp_parse_fen("rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8")
  expect_equal(perft(state, 1L), 44L)
})

test_that("position 5 perft(2) = 1,486", {
  state <- cpp_parse_fen("rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8")
  expect_equal(perft(state, 2L), 1486L)
})

test_that("position 5 perft(3) = 62,379", {
  state <- cpp_parse_fen("rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8")
  expect_equal(perft(state, 3L), 62379L)
})

# ---------------------------------------------------------------------------
# Kiwipete perft(4) — highest-value extension: dense special moves
# ---------------------------------------------------------------------------

test_that("Kiwipete perft(4) = 4,085,603", {
  state <- cpp_parse_fen("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1")
  t0 <- proc.time()
  count <- perft(state, 4L)
  elapsed <- (proc.time() - t0)["elapsed"]
  cat(sprintf("\n  Kiwipete perft(4): %s nodes in %.1fs (%s nodes/sec)\n",
              format(count, big.mark = ","), elapsed,
              format(round(count / elapsed), big.mark = ",")))
  expect_equal(count, 4085603L)
})
