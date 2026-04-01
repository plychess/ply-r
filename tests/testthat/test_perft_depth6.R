# test_perft_depth6.R — Perft depth 6: extended move generation correctness
#
# Depth 6 from starting position = 119,060,324 nodes.
# Reference: https://www.chessprogramming.org/Perft_Results

context("Perft — Depth 6")

skip_on_ci()
skip_on_cran()

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

test_that("starting position perft(6) = 119,060,324", {
  state <- cpp_parse_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
  t0 <- proc.time()
  count <- perft(state, 6L)
  elapsed <- (proc.time() - t0)["elapsed"]
  cat(sprintf("\n  Perft(6): %s nodes in %.1fs (%s nodes/sec)\n",
              format(count, big.mark = ","), elapsed,
              format(round(count / elapsed), big.mark = ",")))
  expect_equal(count, 119060324L)
})
