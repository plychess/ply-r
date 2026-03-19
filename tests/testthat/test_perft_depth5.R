# test_perft_depth5.R — Perft depth 5: the ultimate move generation correctness test
#
# Depth 5 from starting position = 4,865,609 nodes.
# This exercises every edge case in move generation at scale.

context("Perft — Depth 5")

skip_on_ci()
skip_on_cran()

# Iterative perft to avoid R call stack limits
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

test_that("starting position perft(5) = 4,865,609", {
  state <- cpp_parse_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
  t0 <- proc.time()
  count <- perft(state, 5L)
  elapsed <- (proc.time() - t0)["elapsed"]
  cat(sprintf("\n  Perft(5): %s nodes in %.1fs (%s nodes/sec)\n",
              format(count, big.mark = ","), elapsed,
              format(round(count / elapsed), big.mark = ",")))
  expect_equal(count, 4865609L)
})
