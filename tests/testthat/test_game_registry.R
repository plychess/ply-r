# test_game_registry.R — Game registry: create, join, play, resign, draw

context("C++ Engine — Game Registry")

setup({
  cpp_reset_registry()
})

test_that("create game returns game ID 1", {
  cpp_reset_registry()
  id <- cpp_create_game(creator = "player1", settlement_mode = 0L, ply_time_limit = 0)
  expect_equal(id, 1L)
})

test_that("join game succeeds", {
  cpp_reset_registry()
  id <- cpp_create_game(creator = "player1", settlement_mode = 0L, ply_time_limit = 0)
  result <- cpp_join_game(id, player = "player2")
  expect_true(result)
})

test_that("play opening moves via UCI", {
  cpp_reset_registry()
  id <- cpp_create_game(creator = "player1", settlement_mode = 0L, ply_time_limit = 0)
  cpp_join_game(id, player = "player2")

  # 1. e4
  cpp_make_ply_uci(id, player = "player1", uci = "e2e4", settle = FALSE)
  info <- cpp_get_game_info(id)
  expect_equal(info$status, 1L)  # active

  # 1... e5
  cpp_make_ply_uci(id, player = "player2", uci = "e7e5", settle = FALSE)
  fen <- cpp_get_fen(id)
  expect_true(grepl("rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR", fen))
})

test_that("ply history tracks moves", {
  cpp_reset_registry()
  id <- cpp_create_game(creator = "player1", settlement_mode = 0L, ply_time_limit = 0)
  cpp_join_game(id, "player2")
  cpp_make_ply_uci(id, "player1", "e2e4", FALSE)
  cpp_make_ply_uci(id, "player2", "e7e5", FALSE)

  history <- cpp_get_ply_history(id)
  expect_equal(length(history), 2L)
  expect_equal(history[1], "e2e4")
  expect_equal(history[2], "e7e5")
})

test_that("resign ends game", {
  cpp_reset_registry()
  id <- cpp_create_game(creator = "player1", settlement_mode = 0L, ply_time_limit = 0)
  cpp_join_game(id, "player2")
  cpp_make_ply_uci(id, "player1", "e2e4", FALSE)
  cpp_resign(id, "player2")

  info <- cpp_get_game_info(id)
  # Status should indicate game over (not active=1)
  expect_true(info$status != 1L)
})

test_that("create game from custom FEN", {
  cpp_reset_registry()
  custom_fen <- "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1"
  id <- cpp_create_game_from_fen(
    creator = "player1", fen = custom_fen,
    settlement_mode = 0L, ply_time_limit = 0
  )
  fen <- cpp_get_fen(id)
  expect_true(grepl("4P3", fen))
})

test_that("cpp_game_count tracks number of created games", {
  cpp_reset_registry()
  expect_equal(cpp_game_count(), 0L)
  cpp_create_game(creator = "player1", settlement_mode = 0L, ply_time_limit = 0)
  expect_equal(cpp_game_count(), 1L)
  cpp_create_game(creator = "player2", settlement_mode = 0L, ply_time_limit = 0)
  expect_equal(cpp_game_count(), 2L)
})

test_that("cpp_cancel_game cancels a waiting game", {
  cpp_reset_registry()
  id <- cpp_create_game(creator = "player1", settlement_mode = 0L, ply_time_limit = 0)
  expect_equal(cpp_get_game_info(id)$status, 0L)  # Waiting
  result <- cpp_cancel_game(id, "player1")
  expect_true(result)
  info <- cpp_get_game_info(id)
  expect_equal(info$status, 2L)       # Finished
  expect_equal(info$termination, 10L) # Cancelled
})

test_that("cpp_offer_draw and cpp_accept_draw end game as draw", {
  cpp_reset_registry()
  id <- cpp_create_game(creator = "player1", settlement_mode = 0L, ply_time_limit = 0)
  cpp_join_game(id, player = "player2")
  # Play one move each so the game is active with both sides having moved
  cpp_make_ply_uci(id, "player1", "e2e4", FALSE)
  cpp_make_ply_uci(id, "player2", "e7e5", FALSE)
  # player1 offers draw
  expect_true(cpp_offer_draw(id, "player1"))
  # player2 accepts
  expect_true(cpp_accept_draw(id, "player2"))
  info <- cpp_get_game_info(id)
  expect_equal(info$status, 2L)      # Finished
  expect_equal(info$result, 3L)      # Draw
  expect_equal(info$termination, 4L) # DrawByAgreement
})

test_that("cpp_offer_draw by same player who just offered is rejected or idempotent", {
  cpp_reset_registry()
  id <- cpp_create_game(creator = "player1", settlement_mode = 0L, ply_time_limit = 0)
  cpp_join_game(id, player = "player2")
  cpp_make_ply_uci(id, "player1", "e2e4", FALSE)
  cpp_make_ply_uci(id, "player2", "e7e5", FALSE)
  cpp_offer_draw(id, "player1")
  # Offering again from the same side — engine accepts it idempotently
  cpp_offer_draw(id, "player1")
  # Game must still be active (not concluded — only accept_draw concludes it)
  info <- cpp_get_game_info(id)
  expect_equal(info$status, 1L)  # still Active
})
