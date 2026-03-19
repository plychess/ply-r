# test_benchmarks.R — Performance regression tests with timing floors
#
# These tests assert minimum throughput so performance regressions are caught.
# Floors are set conservatively — any modern machine should clear them easily.

context("Benchmarks — Performance Regression Guards")

skip_on_ci()
skip_on_cran()

root <- Sys.getenv("TAL_PROJECT_ROOT", unset = ".")

# ---------------------------------------------------------------------------
# 1. FEN parse + serialize round-trip
# ---------------------------------------------------------------------------

test_that("FEN parse+serialize > 50,000 ops/sec", {
  fen <- "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1"
  n <- 5000L
  t0 <- proc.time()
  for (i in seq_len(n)) {
    state <- cpp_parse_fen(fen)
    cpp_state_to_fen(state)
  }
  elapsed <- (proc.time() - t0)["elapsed"]
  rate <- n / elapsed
  cat(sprintf("\n  FEN round-trip: %.0f ops/sec (%.3fs for %d ops)\n", rate, elapsed, n))
  expect_gt(rate, 50000)
})

# ---------------------------------------------------------------------------
# 2. Legal move generation throughput
# ---------------------------------------------------------------------------

test_that("legal move generation > 50,000 positions/sec", {
  fens <- c(
    "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1",           # opening: 20 moves
    "r1bqkb1r/pppp1ppp/2n2n2/4p3/2B1P3/5N2/PPPP1PPP/RNBQK2R w KQkq - 4 4", # middlegame
    "8/5k2/8/8/3Q4/8/8/4K3 w - - 0 1"                                       # endgame: Q vs K
  )
  n_per <- 2000L
  n_total <- n_per * length(fens)
  states <- lapply(fens, cpp_parse_fen)

  t0 <- proc.time()
  for (i in seq_len(n_per)) {
    for (s in states) {
      cpp_generate_legal_moves(s)
    }
  }
  elapsed <- (proc.time() - t0)["elapsed"]
  rate <- n_total / elapsed
  cat(sprintf("\n  Legal move gen: %.0f positions/sec (%.3fs for %d)\n", rate, elapsed, n_total))
  expect_gt(rate, 50000)
})

# ---------------------------------------------------------------------------
# 3. Single game replay speed
# ---------------------------------------------------------------------------

test_that("single game replay > 20,000 plies/sec", {
  # Italian Game: 1.e4 e5 2.Nf3 Nc6 3.Bc4 Bc5 4.d3 Nf6 5.Nc3 d6
  moves <- c("e4","e5","Nf3","Nc6","Bc4","Bc5","d3","Nf6","Nc3","d6",
             "Bg5","h6","Bh4","g5","Bg3","Nh5","Nd5","Nf4","Bxf4","gxf4",
             "c3","Be6","Qb3","Bxd5","Bxd5","Qd7","O-O","O-O-O","a4","Kb8")
  n_reps <- 200L
  n_plies <- length(moves) * n_reps

  t0 <- proc.time()
  for (i in seq_len(n_reps)) {
    cpp_replay_game(moves)
  }
  elapsed <- (proc.time() - t0)["elapsed"]
  rate <- n_plies / elapsed
  cat(sprintf("\n  Single game replay: %.0f plies/sec (%.3fs for %d plies)\n",
              rate, elapsed, n_plies))
  expect_gt(rate, 20000)
})

# ---------------------------------------------------------------------------
# 4. Batch replay throughput (Tal.pgn — 2,431 games, 173K plies)
# ---------------------------------------------------------------------------

test_that("batch replay of Tal.pgn > 30,000 plies/sec", {
  pgn <- file.path(root, "pgnmentor", "Tal.pgn")
  skip_if_not(file.exists(pgn), "Tal.pgn not found")

  t0 <- proc.time()
  positions <- replay_all_games(pgn)
  elapsed <- (proc.time() - t0)["elapsed"]
  n_plies <- nrow(positions)
  rate <- n_plies / elapsed
  cat(sprintf("\n  Tal.pgn batch replay: %.0f plies/sec (%s plies in %.2fs)\n",
              rate, format(n_plies, big.mark = ","), elapsed))
  expect_gt(rate, 30000)
  expect_equal(sum(!positions$ok), 0L)
})

# ---------------------------------------------------------------------------
# 5. Batch enrichment throughput (cpp_enrich_batch)
# ---------------------------------------------------------------------------

test_that("batch enrichment > 5,000 positions/sec", {
  pgn <- file.path(root, "pgnmentor", "Tal.pgn")
  skip_if_not(file.exists(pgn), "Tal.pgn not found")

  # Use first 5,000 Tal positions
  positions <- replay_all_games(pgn)
  sample_pos <- head(positions[positions$ok, ], 5000)

  t0 <- proc.time()
  cpp_enrich_batch(sample_pos$fen, sample_pos$uci_move)
  elapsed <- (proc.time() - t0)["elapsed"]
  rate <- nrow(sample_pos) / elapsed
  cat(sprintf("\n  Batch enrichment: %.0f positions/sec (5000 in %.2fs)\n",
              rate, elapsed))
  expect_gt(rate, 5000)
})

# ---------------------------------------------------------------------------
# 6. Perft — correctness via known node counts (also measures speed)
# ---------------------------------------------------------------------------

test_that("perft depth 1-3 matches published node counts", {
  # Starting position: https://www.chessprogramming.org/Perft_Results
  state <- cpp_parse_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")

  # Depth 1: 20 nodes
  moves_d1 <- cpp_generate_legal_moves(state)
  expect_equal(length(moves_d1), 20L)

  # Depth 2: 400 nodes (each of 20 moves leads to 20 replies)
  count_d2 <- 0L
  for (m in moves_d1) {
    s2 <- cpp_apply_ply(state, m)
    count_d2 <- count_d2 + length(cpp_generate_legal_moves(s2))
  }
  expect_equal(count_d2, 400L)

  # Depth 3: 8,902 nodes
  t0 <- proc.time()
  count_d3 <- 0L
  for (m1 in moves_d1) {
    s2 <- cpp_apply_ply(state, m1)
    moves_d2 <- cpp_generate_legal_moves(s2)
    for (m2 in moves_d2) {
      s3 <- cpp_apply_ply(s2, m2)
      count_d3 <- count_d3 + length(cpp_generate_legal_moves(s3))
    }
  }
  elapsed <- (proc.time() - t0)["elapsed"]
  cat(sprintf("\n  Perft(3): %d nodes in %.3fs (%.0f nodes/sec)\n",
              count_d3, elapsed, count_d3 / elapsed))
  expect_equal(count_d3, 8902L)
})

test_that("perft depth 1 for Kiwipete position = 48", {
  # Kiwipete: complex position with castling, en passant, promotions
  state <- cpp_parse_fen("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1")
  moves <- cpp_generate_legal_moves(state)
  expect_equal(length(moves), 48L)
})

# ---------------------------------------------------------------------------
# 7. Multi-PGN stress test — replay a large tournament file
# ---------------------------------------------------------------------------

test_that("large tournament PGN replays without errors", {
  pgn <- file.path(root, "pgnmentor", "Gibraltar2019.pgn")
  skip_if_not(file.exists(pgn), "Gibraltar2019.pgn not found")

  t0 <- proc.time()
  positions <- replay_all_games(pgn)
  elapsed <- (proc.time() - t0)["elapsed"]
  n_plies <- nrow(positions)
  n_err <- sum(!positions$ok)
  rate <- n_plies / elapsed

  cat(sprintf("\n  Gibraltar2019: %s plies, %d errors, %.0f plies/sec (%.2fs)\n",
              format(n_plies, big.mark = ","), n_err, rate, elapsed))
  # Allow some errors (PGN quality varies) but must be < 1%
  error_pct <- 100 * n_err / n_plies
  expect_lt(error_pct, 1.0)
  expect_gt(rate, 20000)
})
