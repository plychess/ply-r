# 01_eda_tal.R — Exploratory Data Analysis on Tal's 2,431 games
#
# Step 1 of incremental analysis. Goal: understand what the data looks like
# BEFORE modeling. What does Tal's play look like in our feature space?
#
# Run from project root:
#   Rscript analysis/01_eda_tal.R

library(ply)

# Make internal cpp_* functions available
ns <- getNamespace("ply")
for (fn in ls(ns, pattern = "^cpp_")) assign(fn, get(fn, envir = ns))

cat("============================================================\n")
cat("  Tal Case Study — Step 1: Exploratory Data Analysis\n")
cat("============================================================\n\n")

# =========================================================================
# 1. Load and build dataset (reusing pipeline from test case study)
# =========================================================================

tal_pgn <- "Tal.pgn"
stopifnot(file.exists(tal_pgn))

# --- build_tal_dataset (self-contained) ---
build_tal_dataset <- function(tal_pgn, n_games = 2500L) {
  lines     <- readLines(tal_pgn, warn = FALSE, encoding = "UTF-8")
  raw       <- paste(lines, collapse = "\n")
  games_raw <- strsplit(raw, "\n(?=\\[Event )", perl = TRUE)[[1]]
  n_games   <- min(n_games, length(games_raw))

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

  games_sub  <- games_raw[seq_len(n_games)]
  movetexts  <- vapply(games_sub, ply_pgn_extract_movetext, character(1), USE.NAMES = FALSE)
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

  # Tal's game result from his perspective
  df$tal_won <- (df$result == "1-0" & df$tal_color == "White") |
                (df$result == "0-1" & df$tal_color == "Black")
  df$tal_lost <- (df$result == "0-1" & df$tal_color == "White") |
                 (df$result == "1-0" & df$tal_color == "Black")
  df$tal_drew <- df$result == "1/2-1/2"

  # Move type labels (7-class)
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

  df
}

cat("Building dataset (all games)...\n")
df <- build_tal_dataset(tal_pgn, n_games = 2500L)
n_games <- length(unique(df$game_id))
cat(sprintf("Done: %s positions across %d games\n\n",
            format(nrow(df), big.mark = ","), n_games))

# =========================================================================
# 2. Dataset overview
# =========================================================================

cat("============================================================\n")
cat("  2. DATASET OVERVIEW\n")
cat("============================================================\n\n")

cat(sprintf("  Total Tal positions:     %s\n", format(nrow(df), big.mark = ",")))
cat(sprintf("  Games:                   %d\n", n_games))
cat(sprintf("  Avg positions per game:  %.1f\n", nrow(df) / n_games))
cat(sprintf("  Median ply number:       %d\n", median(df$ply)))
cat(sprintf("  Ply range:               %d – %d\n", min(df$ply), max(df$ply)))

# =========================================================================
# 3. Game results from Tal's perspective
# =========================================================================

cat("\n============================================================\n")
cat("  3. TAL'S RECORD\n")
cat("============================================================\n\n")

n_won  <- sum(df$tal_won[!duplicated(df$game_id)])
n_lost <- sum(df$tal_lost[!duplicated(df$game_id)])
n_drew <- sum(df$tal_drew[!duplicated(df$game_id)])
n_other <- n_games - n_won - n_lost - n_drew

cat(sprintf("  Wins:   %4d  (%.1f%%)\n", n_won,  100 * n_won / n_games))
cat(sprintf("  Losses: %4d  (%.1f%%)\n", n_lost, 100 * n_lost / n_games))
cat(sprintf("  Draws:  %4d  (%.1f%%)\n", n_drew, 100 * n_drew / n_games))
if (n_other > 0) cat(sprintf("  Other:  %4d\n", n_other))

cat(sprintf("\n  As White: %d games  |  As Black: %d games\n",
            sum(df$tal_color[!duplicated(df$game_id)] == "White"),
            sum(df$tal_color[!duplicated(df$game_id)] == "Black")))

# =========================================================================
# 4. Game phase distribution
# =========================================================================

cat("\n============================================================\n")
cat("  4. GAME PHASE DISTRIBUTION\n")
cat("============================================================\n\n")

phase_tab <- table(factor(df$phase, levels = c("opening", "middlegame", "endgame")))
for (ph in names(phase_tab)) {
  cat(sprintf("  %-12s %6s  (%.1f%%)\n", ph,
              format(phase_tab[ph], big.mark = ","),
              100 * phase_tab[ph] / sum(phase_tab)))
}

# =========================================================================
# 5. Move type distribution
# =========================================================================

cat("\n============================================================\n")
cat("  5. MOVE TYPE DISTRIBUTION (7-class)\n")
cat("============================================================\n\n")

mt_tab <- sort(table(df$move_type), decreasing = TRUE)
for (cls in names(mt_tab)) {
  cat(sprintf("  %-22s %6s  (%.1f%%)\n", cls,
              format(mt_tab[cls], big.mark = ","),
              100 * mt_tab[cls] / sum(mt_tab)))
}
cat(sprintf("\n  Tactical moves (sacrifice + sac_check + check): %.1f%%\n",
            100 * sum(mt_tab[c("sacrifice", "sacrifice_check", "check")]) / sum(mt_tab)))

# =========================================================================
# 6. Move types by game phase
# =========================================================================

cat("\n============================================================\n")
cat("  6. MOVE TYPES BY GAME PHASE\n")
cat("============================================================\n\n")

for (ph in c("opening", "middlegame", "endgame")) {
  sub <- df[df$phase == ph, ]
  cat(sprintf("  --- %s (%s positions) ---\n", toupper(ph),
              format(nrow(sub), big.mark = ",")))
  ph_tab <- table(sub$move_type)
  for (cls in levels(df$move_type)) {
    n <- if (cls %in% names(ph_tab)) ph_tab[cls] else 0
    cat(sprintf("    %-22s %5s  (%.1f%%)\n", cls,
                format(n, big.mark = ","), 100 * n / nrow(sub)))
  }
  cat("\n")
}

# =========================================================================
# 7. Move types in wins vs losses
# =========================================================================

cat("============================================================\n")
cat("  7. MOVE TYPES: WINS vs LOSSES\n")
cat("============================================================\n\n")

for (outcome in c("Wins", "Losses")) {
  sub <- if (outcome == "Wins") df[df$tal_won, ] else df[df$tal_lost, ]
  cat(sprintf("  --- %s (%s positions across %d games) ---\n",
              outcome, format(nrow(sub), big.mark = ","),
              length(unique(sub$game_id))))
  o_tab <- table(sub$move_type)
  for (cls in levels(df$move_type)) {
    n <- if (cls %in% names(o_tab)) o_tab[cls] else 0
    cat(sprintf("    %-22s %5s  (%.1f%%)\n", cls,
                format(n, big.mark = ","), 100 * n / nrow(sub)))
  }
  cat("\n")
}

# =========================================================================
# 8. Key feature distributions
# =========================================================================

cat("============================================================\n")
cat("  8. KEY FEATURE DISTRIBUTIONS\n")
cat("============================================================\n\n")

summary_feat <- function(name, vals) {
  cat(sprintf("  %-28s  mean=%6.2f  median=%5.1f  sd=%5.2f  range=[%d, %d]\n",
              name, mean(vals), median(vals), sd(vals), min(vals), max(vals)))
}

summary_feat("material_bal",         df$material_bal)
summary_feat("num_pieces",           df$num_pieces)
summary_feat("legal_move_count",     df$legal_move_count)
summary_feat("king_safety_own",      df$king_safety_own)
summary_feat("king_safety_opp",      df$king_safety_opp)
summary_feat("num_captures_avail",   df$num_captures_avail)
summary_feat("num_checks_avail",     df$num_checks_avail)
summary_feat("num_sacrifices_avail", df$num_sacrifices_avail)
summary_feat("space_advantage",      df$space_advantage)
summary_feat("center_occupied",      df$center_occupied)
summary_feat("center_attacked",      df$center_attacked)
summary_feat("undeveloped_minors",   df$undeveloped_minors)
summary_feat("passed_pawns",         df$passed_pawns)
summary_feat("doubled_pawns",        df$doubled_pawns)
summary_feat("isolated_pawns",       df$isolated_pawns)
summary_feat("pin_count",            df$pin_count)
summary_feat("best_capture_net",     df$best_capture_net)
summary_feat("num_safe_sacrifices",  df$num_safe_sacrifices)
summary_feat("opp_recapture_after_move", df$opp_recapture_after_move)

# =========================================================================
# 9. Feature means by move type
# =========================================================================

cat("\n============================================================\n")
cat("  9. FEATURE MEANS BY MOVE TYPE\n")
cat("============================================================\n\n")

key_feats <- c("material_bal", "king_safety_opp", "num_captures_avail",
               "num_sacrifices_avail", "legal_move_count", "space_advantage",
               "best_capture_net", "num_safe_sacrifices")

cat(sprintf("  %-22s", ""))
for (f in key_feats) cat(sprintf(" %8s", substr(f, 1, 8)))
cat("\n")
cat(paste0("  ", paste(rep("-", 22 + length(key_feats) * 9), collapse = "")), "\n")

for (cls in levels(df$move_type)) {
  sub <- df[df$move_type == cls, ]
  cat(sprintf("  %-22s", cls))
  for (f in key_feats) {
    cat(sprintf(" %8.2f", mean(sub[[f]])))
  }
  cat(sprintf("  (n=%s)\n", format(nrow(sub), big.mark = ",")))
}

# =========================================================================
# 10. Material balance in wins vs losses
# =========================================================================

cat("\n============================================================\n")
cat("  10. MATERIAL BALANCE BY OUTCOME\n")
cat("============================================================\n\n")

for (outcome in c("Wins", "Losses", "Draws")) {
  sub <- switch(outcome,
    Wins   = df[df$tal_won, ],
    Losses = df[df$tal_lost, ],
    Draws  = df[df$tal_drew, ])
  cat(sprintf("  %-8s  mean_bal=%+.2f  pct_behind=%.1f%%  pct_ahead=%.1f%%  avg_pieces=%.1f\n",
              outcome,
              mean(sub$material_bal),
              100 * mean(sub$material_bal < 0),
              100 * mean(sub$material_bal > 0),
              mean(sub$num_pieces)))
}

# =========================================================================
# 11. Sacrifice rate by phase and outcome
# =========================================================================

cat("\n============================================================\n")
cat("  11. SACRIFICE RATE BY PHASE AND OUTCOME\n")
cat("============================================================\n\n")

is_sac <- df$move_type %in% c("sacrifice", "sacrifice_check")
cat(sprintf("  %-20s %8s %8s %8s\n", "", "Opening", "Middle", "Endgame"))
for (outcome in c("Wins", "Losses", "Draws", "All")) {
  sub <- switch(outcome,
    Wins   = df[df$tal_won, ],
    Losses = df[df$tal_lost, ],
    Draws  = df[df$tal_drew, ],
    All    = df)
  rates <- vapply(c("opening", "middlegame", "endgame"), function(ph) {
    ph_sub <- sub[sub$phase == ph, ]
    if (nrow(ph_sub) == 0) return(0)
    100 * mean(ph_sub$move_type %in% c("sacrifice", "sacrifice_check"))
  }, numeric(1))
  cat(sprintf("  %-20s %7.1f%% %7.1f%% %7.1f%%\n", outcome, rates[1], rates[2], rates[3]))
}

# =========================================================================
# 12. Decade analysis
# =========================================================================

cat("\n============================================================\n")
cat("  12. TAL BY DECADE\n")
cat("============================================================\n\n")

df$decade <- paste0(floor(df$year / 10) * 10, "s")
decades <- sort(unique(df$decade))
cat(sprintf("  %-8s %6s %8s %8s %8s %8s\n",
            "Decade", "Pos", "Sac%", "Check%", "AvgBal", "AvgPcs"))
for (dec in decades) {
  sub <- df[df$decade == dec, ]
  sac_rate <- 100 * mean(sub$move_type %in% c("sacrifice", "sacrifice_check"))
  chk_rate <- 100 * mean(sub$move_type == "check")
  cat(sprintf("  %-8s %6s %7.1f%% %7.1f%%   %+5.2f   %5.1f\n",
              dec, format(nrow(sub), big.mark = ","),
              sac_rate, chk_rate,
              mean(sub$material_bal), mean(sub$num_pieces)))
}

cat("\n============================================================\n")
cat("  EDA complete. Next step: 02_feature_importance.R\n")
cat("============================================================\n")
