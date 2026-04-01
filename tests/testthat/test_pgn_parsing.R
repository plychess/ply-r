# test_pgn_parsing.R — PGN parsing edge cases

context("PGN Parsing — Edge Cases")

# ---------------------------------------------------------------------------
# Comments
# ---------------------------------------------------------------------------

test_that("comments are stripped from movetext", {
  game_text <- '[Event "Test"]\n\n1.e4 {Best by test} e5 2.Nf3 {developing} Nc6 1-0'
  tokens <- strsplit(ply_pgn_extract_movetext(game_text), " ")[[1]]
  tokens <- tokens[nzchar(tokens)]
  expect_equal(tokens, c("e4", "e5", "Nf3", "Nc6"))
})

test_that("multi-word comments are fully stripped", {
  game_text <- '[Event "Test"]\n\n1.e4 {a long multi-word comment here} e5 1-0'
  tokens <- strsplit(ply_pgn_extract_movetext(game_text), " ")[[1]]
  tokens <- tokens[nzchar(tokens)]
  expect_equal(tokens, c("e4", "e5"))
  expect_false(any(grepl("\\{|\\}", tokens)))
})

test_that("nested braces in comments are handled", {
  game_text <- '[Event "Test"]\n\n1.e4 {comment with {nested}} e5 1-0'
  tokens <- strsplit(ply_pgn_extract_movetext(game_text), " ")[[1]]
  tokens <- tokens[nzchar(tokens)]
  expect_true("e4" %in% tokens)
  expect_true("e5" %in% tokens)
  expect_false(any(grepl("\\{|\\}", tokens)))
})

# ---------------------------------------------------------------------------
# NAGs (Numeric Annotation Glyphs)
# ---------------------------------------------------------------------------

test_that("NAGs ($1, $2, etc.) are stripped", {
  game_text <- '[Event "Test"]\n\n1.e4 $1 e5 $2 2.Nf3 $6 Nc6 1-0'
  tokens <- strsplit(ply_pgn_extract_movetext(game_text), " ")[[1]]
  tokens <- tokens[nzchar(tokens)]
  expect_equal(tokens, c("e4", "e5", "Nf3", "Nc6"))
})

# ---------------------------------------------------------------------------
# Variations (parenthesized alternative lines)
# ---------------------------------------------------------------------------

test_that("variations in parentheses are stripped", {
  game_text <- '[Event "Test"]\n\n1.e4 e5 (1...c5 2.Nf3) 2.Nf3 Nc6 1-0'
  tokens <- strsplit(ply_pgn_extract_movetext(game_text), " ")[[1]]
  tokens <- tokens[nzchar(tokens)]
  expect_equal(tokens, c("e4", "e5", "Nf3", "Nc6"))
})

# ---------------------------------------------------------------------------
# Incomplete game ending with *
# ---------------------------------------------------------------------------

test_that("game ending with * is parsed (result stripped)", {
  game_text <- '[Event "Test"]\n[Result "*"]\n\n1.d4 Nf6 2.c4 *'
  tokens <- strsplit(ply_pgn_extract_movetext(game_text), " ")[[1]]
  tokens <- tokens[nzchar(tokens)]
  expect_equal(tokens, c("d4", "Nf6", "c4"))
})

# ---------------------------------------------------------------------------
# Game with no moves
# ---------------------------------------------------------------------------

test_that("game with no moves produces empty token list", {
  game_text <- '[Event "Test"]\n[Result "1-0"]\n\n1-0'
  tokens <- strsplit(ply_pgn_extract_movetext(game_text), " ")[[1]]
  tokens <- tokens[nzchar(tokens)]
  expect_equal(length(tokens), 0L)
})

# ---------------------------------------------------------------------------
# Result strings in movetext
# ---------------------------------------------------------------------------

test_that("all result strings are removed from movetext", {
  for (result in c("1-0", "0-1", "1/2-1/2", "*")) {
    game_text <- sprintf('[Event "Test"]\n\n1.e4 e5 %s', result)
    tokens <- strsplit(ply_pgn_extract_movetext(game_text), " ")[[1]]
    tokens <- tokens[nzchar(tokens)]
    expect_equal(tokens, c("e4", "e5"),
      info = paste("Result not stripped:", result))
  }
})

# ---------------------------------------------------------------------------
# Move number formats
# ---------------------------------------------------------------------------

test_that("move numbers with dots are stripped (1. and 1...)", {
  game_text <- '[Event "Test"]\n\n1. e4 e5 2. Nf3 Nc6 1-0'
  tokens <- strsplit(ply_pgn_extract_movetext(game_text), " ")[[1]]
  tokens <- tokens[nzchar(tokens)]
  expect_equal(tokens, c("e4", "e5", "Nf3", "Nc6"))
})

test_that("continuation dots are stripped (1...)", {
  game_text <- '[Event "Test"]\n\n1. e4 1... e5 2. Nf3 2... Nc6 1-0'
  tokens <- strsplit(ply_pgn_extract_movetext(game_text), " ")[[1]]
  tokens <- tokens[nzchar(tokens)]
  expect_equal(tokens, c("e4", "e5", "Nf3", "Nc6"))
})

# ---------------------------------------------------------------------------
# PGN tag parsing
# ---------------------------------------------------------------------------

test_that("ply_pgn_parse_tags extracts all standard tags", {
  game_text <- '[Event "Test Event"]\n[Site "Test Site"]\n[Date "2025.01.01"]\n[White "Player1"]\n[Black "Player2"]\n[Result "1-0"]\n[ECO "B00"]\n\n1.e4 e5 1-0'
  tags <- ply_pgn_parse_tags(game_text)
  expect_equal(tags$Event, "Test Event")
  expect_equal(tags$Site, "Test Site")
  expect_equal(tags$White, "Player1")
  expect_equal(tags$Black, "Player2")
  expect_equal(tags$Result, "1-0")
  expect_equal(tags$ECO, "B00")
})

# ---------------------------------------------------------------------------
# Full replay of edge-case game via C++ engine
# ---------------------------------------------------------------------------

test_that("game with promotions replays correctly", {
  # Short game ending in promotion
  moves <- c("e4", "d5", "exd5", "Qxd5", "Nc3", "Qa5", "d4", "e5",
             "dxe5", "Bb4", "Bd2", "Nc6", "Nf3", "Nge7", "a3", "Bxc3",
             "Bxc3", "Qb6", "Bd3", "O-O")
  result <- cpp_replay_game(moves)
  expect_equal(nrow(result), length(moves))
  expect_true(all(result$ok))
})

test_that("game with castling both sides replays correctly", {
  moves <- c("e4", "e5", "Nf3", "Nc6", "Bc4", "Bc5", "d3", "Nf6",
             "O-O", "O-O")
  result <- cpp_replay_game(moves)
  expect_equal(nrow(result), length(moves))
  expect_true(all(result$ok))
})
