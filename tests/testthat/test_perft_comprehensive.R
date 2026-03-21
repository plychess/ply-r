# test_perft_comprehensive.R — Comprehensive perft correctness tests
#
# Tests all 6 standard positions up to depth 8 using the full C++ perft engine.
# Reference: https://www.chessprogramming.org/Perft_Results
#
# Uses cpp_xp_perft which runs entirely in C++ (no R loop overhead).

context("Perft — Comprehensive (all positions, depth 4-8)")

skip_on_cran()
skip_on_ci()

# ---------------------------------------------------------------------------
# Position 1: Starting position (depths 1-8)
# ---------------------------------------------------------------------------

test_that("Starting position perft(1) = 20", {
  expect_equal(ply:::cpp_xp_perft("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", 1L), 20)
})

test_that("Starting position perft(2) = 400", {
  expect_equal(ply:::cpp_xp_perft("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", 2L), 400)
})

test_that("Starting position perft(3) = 8,902", {
  expect_equal(ply:::cpp_xp_perft("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", 3L), 8902)
})

test_that("Starting position perft(4) = 197,281", {
  expect_equal(ply:::cpp_xp_perft("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", 4L), 197281)
})

test_that("Starting position perft(5) = 4,865,609", {
  expect_equal(ply:::cpp_xp_perft("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", 5L), 4865609)
})

test_that("Starting position perft(6) = 119,060,324", {
  expect_equal(ply:::cpp_xp_perft("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", 6L), 119060324)
})

test_that("Starting position perft(7) = 3,195,901,860", {
  expect_equal(ply:::cpp_xp_perft("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", 7L), 3195901860)
})

test_that("Starting position perft(8) = 84,998,978,956", {
  skip_on_ci()  # ~55s single-threaded
  expect_equal(ply:::cpp_xp_perft("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", 8L), 84998978956)
})

# ---------------------------------------------------------------------------
# Position 2: Kiwipete — castling, en passant, promotions, dense tactics
# ---------------------------------------------------------------------------

test_that("Kiwipete perft(1) = 48", {
  expect_equal(ply:::cpp_xp_perft("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1", 1L), 48)
})

test_that("Kiwipete perft(2) = 2,039", {
  expect_equal(ply:::cpp_xp_perft("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1", 2L), 2039)
})

test_that("Kiwipete perft(3) = 97,862", {
  expect_equal(ply:::cpp_xp_perft("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1", 3L), 97862)
})

test_that("Kiwipete perft(4) = 4,085,603", {
  expect_equal(ply:::cpp_xp_perft("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1", 4L), 4085603)
})

test_that("Kiwipete perft(5) = 193,690,690", {
  expect_equal(ply:::cpp_xp_perft("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1", 5L), 193690690)
})

test_that("Kiwipete perft(6) = 8,031,647,685", {
  skip_on_ci()  # ~6s single-threaded
  expect_equal(ply:::cpp_xp_perft("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1", 6L), 8031647685)
})

# ---------------------------------------------------------------------------
# Position 3: En passant + promotion edge cases
# ---------------------------------------------------------------------------

test_that("Position 3 perft(1) = 14", {
  expect_equal(ply:::cpp_xp_perft("8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1", 1L), 14)
})

test_that("Position 3 perft(2) = 191", {
  expect_equal(ply:::cpp_xp_perft("8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1", 2L), 191)
})

test_that("Position 3 perft(3) = 2,812", {
  expect_equal(ply:::cpp_xp_perft("8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1", 3L), 2812)
})

test_that("Position 3 perft(4) = 43,238", {
  expect_equal(ply:::cpp_xp_perft("8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1", 4L), 43238)
})

test_that("Position 3 perft(5) = 674,624", {
  expect_equal(ply:::cpp_xp_perft("8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1", 5L), 674624)
})

test_that("Position 3 perft(6) = 11,030,083", {
  expect_equal(ply:::cpp_xp_perft("8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1", 6L), 11030083)
})

test_that("Position 3 perft(7) = 178,633,661", {
  expect_equal(ply:::cpp_xp_perft("8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1", 7L), 178633661)
})

test_that("Position 3 perft(8) = 3,009,794,393", {
  skip_on_ci()  # ~0.2s but skip to be safe
  expect_equal(ply:::cpp_xp_perft("8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1", 8L), 3009794393)
})

# ---------------------------------------------------------------------------
# Position 4: Discovered checks + promotions
# ---------------------------------------------------------------------------

test_that("Position 4 perft(1) = 6", {
  expect_equal(ply:::cpp_xp_perft("r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1", 1L), 6)
})

test_that("Position 4 perft(2) = 264", {
  expect_equal(ply:::cpp_xp_perft("r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1", 2L), 264)
})

test_that("Position 4 perft(3) = 9,467", {
  expect_equal(ply:::cpp_xp_perft("r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1", 3L), 9467)
})

test_that("Position 4 perft(4) = 422,333", {
  expect_equal(ply:::cpp_xp_perft("r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1", 4L), 422333)
})

test_that("Position 4 perft(5) = 15,833,292", {
  expect_equal(ply:::cpp_xp_perft("r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1", 5L), 15833292)
})

test_that("Position 4 perft(6) = 706,045,033", {
  expect_equal(ply:::cpp_xp_perft("r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1", 6L), 706045033)
})

# ---------------------------------------------------------------------------
# Position 5: Promotion-heavy position
# ---------------------------------------------------------------------------

test_that("Position 5 perft(1) = 44", {
  expect_equal(ply:::cpp_xp_perft("rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8", 1L), 44)
})

test_that("Position 5 perft(2) = 1,486", {
  expect_equal(ply:::cpp_xp_perft("rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8", 2L), 1486)
})

test_that("Position 5 perft(3) = 62,379", {
  expect_equal(ply:::cpp_xp_perft("rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8", 3L), 62379)
})

test_that("Position 5 perft(4) = 2,103,487", {
  expect_equal(ply:::cpp_xp_perft("rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8", 4L), 2103487)
})

test_that("Position 5 perft(5) = 89,941,194", {
  expect_equal(ply:::cpp_xp_perft("rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8", 5L), 89941194)
})

# ---------------------------------------------------------------------------
# Position 6: Edwards — symmetric, many pieces
# ---------------------------------------------------------------------------

test_that("Position 6 perft(1) = 46", {
  expect_equal(ply:::cpp_xp_perft("r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10", 1L), 46)
})

test_that("Position 6 perft(2) = 2,079", {
  expect_equal(ply:::cpp_xp_perft("r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10", 2L), 2079)
})

test_that("Position 6 perft(3) = 89,890", {
  expect_equal(ply:::cpp_xp_perft("r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10", 3L), 89890)
})

test_that("Position 6 perft(4) = 3,894,594", {
  expect_equal(ply:::cpp_xp_perft("r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10", 4L), 3894594)
})

test_that("Position 6 perft(5) = 164,075,551", {
  expect_equal(ply:::cpp_xp_perft("r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10", 5L), 164075551)
})
