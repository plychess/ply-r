# pgn.R — PGN parsing utilities

#' Parse PGN tag pairs from a raw game block
#'
#' @param game_text A character string containing one PGN game block.
#' @return A named list of tag key/value pairs.
#' @examples
#' g <- '[Event "Test"]\n[White "Alice"]\n[Black "Bob"]\n\n1.e4 e5 1-0'
#' tags <- ply_pgn_parse_tags(g)
#' tags$White  # "Alice"
#' @export
ply_pgn_parse_tags <- function(game_text) {
  tags <- regmatches(game_text,
    gregexpr("\\[(\\w+)\\s+\"([^\"]*)\"\\]", game_text, perl = TRUE))[[1]]
  keys <- sub("^\\[(\\w+)\\s+\".*\"\\]$",  "\\1", tags)
  vals <- sub("^\\[\\w+\\s+\"(.*)\"\\]$",  "\\1", tags)
  setNames(as.list(vals), keys)
}

#' Extract clean SAN movetext tokens from a PGN game block
#'
#' Strips tags, comments, variations, NAGs, and result strings.
#'
#' @param game_text A character string containing one PGN game block.
#' @return A single trimmed character string of space-separated SAN tokens.
#' @examples
#' g <- '[Event "Test"]\n\n1.e4 {Best by test} e5 2.Nf3 Nc6 1-0'
#' ply_pgn_extract_movetext(g)  # "e4 e5 Nf3 Nc6"
#' @export
ply_pgn_extract_movetext <- function(game_text) {
  txt <- gsub("\\[.*?\\]\\s*",       "",  game_text, perl = TRUE)
  txt <- gsub("\\{[^{}]*(?:\\{[^{}]*\\}[^{}]*)*\\}", " ", txt, perl = TRUE)
  txt <- gsub("\\([^)]*\\)",         " ", txt)
  txt <- gsub("\\$\\d+",             " ", txt)
  # Truncate at the game result (drop everything after it, including junk footers)
  txt <- sub("(?s)(1-0|0-1|1/2-1/2|\\*).*$", "", txt, perl = TRUE)
  txt <- gsub("\\d+\\.{1,3}",        " ", txt)
  trimws(gsub("\\s+", " ", txt))
}

#' Load a PGN file and return a data.frame of game-level metadata
#'
#' Returns a \code{data.frame} with columns \code{Event}, \code{White},
#' \code{Black}, \code{Result}, \code{ECO}, and \code{Plys} (number of
#' half-moves).
#'
#' @param pgn_path Path to a PGN file.
#' @return A \code{data.frame} with one row per game and columns
#'   \code{Event}, \code{White}, \code{Black}, \code{Result}, \code{ECO},
#'   \code{Plys}.
#' @examples
#' pgn_file <- system.file("extdata", "example.pgn", package = "ply")
#' if (file.exists(pgn_file)) {
#'   games <- ply_pgn_load_games(pgn_file)
#'   head(games)
#' }
#' @export
ply_pgn_load_games <- function(pgn_path) {
  lines     <- readLines(pgn_path, warn = FALSE, encoding = "UTF-8")
  raw       <- paste(lines, collapse = "\n")
  games_raw <- strsplit(raw, "\n(?=\\[Event )", perl = TRUE)[[1]]

  null_or <- function(x, default = NA_character_) {
    if (is.null(x) || length(x) == 0) default else x
  }

  rows <- lapply(games_raw, function(g) {
    tags  <- ply_pgn_parse_tags(g)
    moves <- strsplit(ply_pgn_extract_movetext(g), " ")[[1]]
    moves <- moves[nzchar(moves)]
    data.frame(
      Event  = null_or(tags$Event),
      White  = null_or(tags$White),
      Black  = null_or(tags$Black),
      Result = null_or(tags$Result),
      ECO    = null_or(tags$ECO),
      Plys   = length(moves),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

#' Replay all games in a PGN file via the C++ batch engine
#'
#' Parses every game in the PGN file, extracts SAN moves, and replays them
#' through \code{cpp_replay_games_batch}. Returns a data.frame with one row
#' per ply containing the FEN before the move, the UCI move, and a success flag.
#'
#' @param pgn_path Path to a PGN file.
#' @return A \code{data.frame} with columns \code{game_id}, \code{fen},
#'   \code{uci_move}, \code{ply_num}, \code{ok}.
#' @export
replay_all_games <- function(pgn_path) {
  lines     <- readLines(pgn_path, warn = FALSE, encoding = "UTF-8")
  raw       <- paste(lines, collapse = "\n")
  games_raw <- strsplit(raw, "\n(?=\\[Event )", perl = TRUE)[[1]]

  # Extract movetext for all games at once (vectorised regex)
  movetexts <- vapply(games_raw, ply_pgn_extract_movetext, character(1),
                      USE.NAMES = FALSE)
  move_lists <- strsplit(movetexts, " ")
  move_lists <- lapply(move_lists, function(m) m[nzchar(m)])
  lengths    <- lengths(move_lists)

  # Drop empty games
  keep       <- lengths > 0L
  move_lists <- move_lists[keep]
  lengths    <- lengths[keep]
  ids        <- which(keep)

  all_moves   <- unlist(move_lists, use.names = FALSE)
  game_starts <- cumsum(c(1L, lengths[-length(lengths)]))

  cpp_replay_games_batch(ids, all_moves, game_starts)
}

parse_pgn_tags <- ply_pgn_parse_tags
extract_movetext <- ply_pgn_extract_movetext
load_pgn_games <- ply_pgn_load_games
