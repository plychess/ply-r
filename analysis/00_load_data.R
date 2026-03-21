# 00_load_data.R — Shared data loading for all analysis steps
#
# Source this from any analysis script:
#   source("analysis/00_load_data.R")
#
# Provides: df (86,861 Tal positions, enriched with all features)

if (!exists("df") || !is.data.frame(df) || nrow(df) < 1000) {
  library(ply)
  ns <- getNamespace("ply")
  for (fn in ls(ns, pattern = "^cpp_")) assign(fn, get(fn, envir = ns), envir = .GlobalEnv)

  tal_pgn <- "Tal.pgn"
  stopifnot(file.exists(tal_pgn))

  cat("Loading and enriching Tal dataset...\n")

  lines     <- readLines(tal_pgn, warn = FALSE, encoding = "UTF-8")
  raw       <- paste(lines, collapse = "\n")
  games_raw <- strsplit(raw, "\n(?=\\[Event )", perl = TRUE)[[1]]
  n_games   <- length(games_raw)

  game_meta <- lapply(seq_len(n_games), function(i) {
    tags <- ply_pgn_parse_tags(games_raw[i])
    tal_is_white <- grepl("Tal", tags$White %||% "", ignore.case = TRUE)
    data.frame(game_id = i,
               tal_color = ifelse(tal_is_white, "White", "Black"),
               result = tags$Result %||% NA_character_,
               year = as.integer(sub("\\..*", "", tags$Date %||% "NA")),
               eco = tags$ECO %||% NA_character_,
               stringsAsFactors = FALSE)
  })
  game_meta <- do.call(rbind, game_meta)

  movetexts  <- vapply(games_raw, ply_pgn_extract_movetext, character(1), USE.NAMES = FALSE)
  move_lists <- strsplit(movetexts, " ")
  move_lists <- lapply(move_lists, function(m) m[nzchar(m)])
  lens       <- lengths(move_lists)
  keep       <- lens > 0L
  move_lists <- move_lists[keep]
  lens       <- lens[keep]
  ids        <- which(keep)
  all_moves   <- unlist(move_lists, use.names = FALSE)
  game_starts <- cumsum(c(1L, lens[-length(lens)]))
  replayed    <- cpp_replay_games_batch(ids, all_moves, game_starts)
  good        <- replayed[replayed$ok, ]

  good$tal_color <- game_meta$tal_color[match(good$game_id, game_meta$game_id)]
  good$result    <- game_meta$result[match(good$game_id, game_meta$game_id)]
  good$year      <- game_meta$year[match(good$game_id, game_meta$game_id)]
  good$eco       <- game_meta$eco[match(good$game_id, game_meta$game_id)]

  tal_turns <- good[
    (good$tal_color == "White" & good$ply_num %% 2 == 1) |
    (good$tal_color == "Black" & good$ply_num %% 2 == 0), ]

  features <- cpp_enrich_batch(tal_turns$fen, tal_turns$uci_move)
  df <- cbind(tal_turns, features)

  df$ply <- vapply(df$fen, function(fen) {
    parts <- strsplit(fen, " ", fixed = TRUE)[[1]]
    fm <- as.integer(parts[6]); side <- parts[2]
    (fm - 1L) * 2L + ifelse(side == "w", 0L, 1L)
  }, integer(1), USE.NAMES = FALSE)

  df$phase <- vapply(df$fen, function(fen) {
    parts <- strsplit(fen, " ", fixed = TRUE)[[1]]
    fullmove <- as.integer(parts[6])
    if (!is.na(fullmove) && fullmove < 10) return("opening")
    board <- parts[1]
    pieces <- nchar(gsub("[^pnbrqkPNBRQK]", "", board))
    queens <- nchar(gsub("[^qQ]", "", board))
    if (pieces > 20 || queens >= 2) "middlegame" else "endgame"
  }, character(1), USE.NAMES = FALSE)

  df$material_bal <- ifelse(df$tal_color == "Black", -df$material_bal, df$material_bal)
  df$in_material_deficit <- as.integer(df$material_bal < 0)

  df$tal_won  <- (df$result == "1-0" & df$tal_color == "White") |
                 (df$result == "0-1" & df$tal_color == "Black")
  df$tal_lost <- (df$result == "0-1" & df$tal_color == "White") |
                 (df$result == "1-0" & df$tal_color == "Black")
  df$tal_drew <- df$result == "1/2-1/2"

  # Repetition count (cumulative position occurrences within game)
  df$fen_key <- vapply(strsplit(df$fen, " "),
    function(f) paste(f[seq_len(4L)], collapse = " "), character(1L))
  df$repetition_count <- ave(seq_len(nrow(df)),
    list(df$game_id, df$fen_key), FUN = seq_along)
  df$fen_key <- NULL

  label <- rep("quiet_restrained", nrow(df))
  label[df$num_checks_avail == 0 & df$num_captures_avail == 0 &
        df$num_sacrifices_avail == 0] <- "quiet_forced"
  label[df$is_capture] <- "trade"
  label[df$is_capture & df$captured_piece_value > df$moved_piece_value] <- "winning_capture"
  is_sac <- df$is_capture & df$moved_piece_value > df$captured_piece_value
  label[is_sac] <- "sacrifice"
  label[df$gives_check & !df$is_capture] <- "check"
  label[is_sac & df$gives_check] <- "sacrifice_check"
  df$move_type <- factor(label, levels = c(
    "quiet_forced", "quiet_restrained", "trade",
    "winning_capture", "sacrifice", "sacrifice_check", "check"))

  for (col in c("in_check", "can_castle", "en_passant_avail", "gives_discovered_check"))
    if (col %in% names(df)) df[[col]] <- as.integer(df[[col]])

  cat(sprintf("Done: %s positions across %d games\n\n",
              format(nrow(df), big.mark = ","), length(unique(df$game_id))))
}
