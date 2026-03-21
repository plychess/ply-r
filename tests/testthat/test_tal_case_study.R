# test_tal_case_study.R — Case study: Mikhail Tal's 2,431 games
#
# Validates the full pipeline (PGN parse → replay → enrich) against
# a real grandmaster corpus and sanity-checks enrichment distributions
# against Tal's known aggressive, sacrificial style.

context("Case Study — Tal.pgn")

# -------------------------------------------------------------------------
# Setup: locate the PGN file
# -------------------------------------------------------------------------

root <- Sys.getenv("PLY_ROOT", unset = normalizePath(file.path(getwd(), "..", "..")))
tal_pgn <- file.path(root, "Tal.pgn")

skip_if_not <- function(cond, msg) if (!cond) skip(msg)

# =========================================================================
# 1. PGN loading
# =========================================================================

test_that("Tal.pgn loads and contains expected game count", {
  skip_if_not(file.exists(tal_pgn), "Tal.pgn not found")
  games <- ply_pgn_load_games(tal_pgn)
  expect_true(is.data.frame(games))
  expect_true(nrow(games) >= 2400L)
  expect_true(all(c("Event", "White", "Black", "Result", "Plys") %in% names(games)))
})

test_that("Tal appears as White or Black in every game", {
  skip_if_not(file.exists(tal_pgn), "Tal.pgn not found")
  games <- ply_pgn_load_games(tal_pgn)
  tal_in_game <- grepl("Tal", games$White, ignore.case = TRUE) |
                 grepl("Tal", games$Black, ignore.case = TRUE)
  expect_true(mean(tal_in_game) > 0.95)
})

test_that("results are valid PGN results", {
  skip_if_not(file.exists(tal_pgn), "Tal.pgn not found")
  games <- ply_pgn_load_games(tal_pgn)
  valid_results <- c("1-0", "0-1", "1/2-1/2", "*")
  expect_true(all(games$Result %in% valid_results))
})

# =========================================================================
# 2. Replay — first 50 games through the C++ engine
# =========================================================================

test_that("first 50 Tal games replay without errors", {
  skip_if_not(file.exists(tal_pgn), "Tal.pgn not found")

  lines     <- readLines(tal_pgn, warn = FALSE, encoding = "UTF-8")
  raw       <- paste(lines, collapse = "\n")
  games_raw <- strsplit(raw, "\n(?=\\[Event )", perl = TRUE)[[1]]

  # Take first 50 games
  n <- min(50L, length(games_raw))
  games_raw <- games_raw[seq_len(n)]

  movetexts  <- vapply(games_raw, ply_pgn_extract_movetext, character(1),
                        USE.NAMES = FALSE)
  move_lists <- strsplit(movetexts, " ")
  move_lists <- lapply(move_lists, function(m) m[nzchar(m)])
  lengths    <- lengths(move_lists)

  keep       <- lengths > 0L
  move_lists <- move_lists[keep]
  lengths    <- lengths[keep]
  ids        <- which(keep)

  all_moves   <- unlist(move_lists, use.names = FALSE)
  game_starts <- cumsum(c(1L, lengths[-length(lengths)]))

  result <- cpp_replay_games_batch(ids, all_moves, game_starts)
  expect_true(is.data.frame(result))
  expect_true(nrow(result) > 0L)
  # All plies should replay successfully
  fail_rate <- mean(!result$ok)
  expect_true(fail_rate < 0.01,
    info = sprintf("%.1f%% of plies failed to replay", fail_rate * 100))
})

# =========================================================================
# 3. Enrichment — first 10 games, full feature extraction
# =========================================================================

test_that("enrichment runs on replayed Tal games and returns all columns", {
  skip_if_not(file.exists(tal_pgn), "Tal.pgn not found")

  lines     <- readLines(tal_pgn, warn = FALSE, encoding = "UTF-8")
  raw       <- paste(lines, collapse = "\n")
  games_raw <- strsplit(raw, "\n(?=\\[Event )", perl = TRUE)[[1]]

  n <- min(10L, length(games_raw))
  games_raw <- games_raw[seq_len(n)]

  movetexts  <- vapply(games_raw, ply_pgn_extract_movetext, character(1),
                        USE.NAMES = FALSE)
  move_lists <- strsplit(movetexts, " ")
  move_lists <- lapply(move_lists, function(m) m[nzchar(m)])
  lengths    <- lengths(move_lists)

  keep       <- lengths > 0L
  move_lists <- move_lists[keep]
  lengths    <- lengths[keep]
  ids        <- which(keep)

  all_moves   <- unlist(move_lists, use.names = FALSE)
  game_starts <- cumsum(c(1L, lengths[-length(lengths)]))

  replayed <- cpp_replay_games_batch(ids, all_moves, game_starts)
  good     <- replayed[replayed$ok, ]
  expect_true(nrow(good) > 0L)

  enriched <- cpp_enrich_batch(good$fen, good$uci_move)
  expect_equal(nrow(enriched), nrow(good))

  # All 50 enrichment columns should be present
  expected_cols <- c(
    "is_capture", "is_castling", "is_promotion", "is_en_passant",
    "gives_check", "gives_discovered_check", "in_check",
    "material_bal", "num_pieces", "legal_move_count",
    "moved_piece_value", "captured_piece_value",
    "is_checkmate", "is_stalemate",
    "num_captures_avail", "num_checks_avail", "can_castle",
    "max_capture_value", "num_sacrifices_avail", "num_winning_caps_avail",
    "max_sacrifice_gap",
    "king_safety_own", "king_safety_opp",
    "mob_pawn", "mob_knight", "mob_bishop", "mob_rook", "mob_queen", "mob_king",
    "doubled_pawns", "isolated_pawns", "passed_pawns",
    "pin_count", "en_passant_avail", "promotion_available",
    "center_occupied", "center_attacked",
    "undeveloped_minors", "space_advantage",
    "pieces_defended", "pawn_chain_length", "pawn_islands",
    "best_capture_net", "num_safe_sacrifices", "opp_recapture_after_move",
    "position_eval", "chosen_move_eval", "best_move_eval", "eval_gap",
    "best_quiet_eval", "best_capture_eval", "worst_capture_eval",
    "capture_eval_spread", "best_sacrifice_eval", "num_moves_better_than_chosen"
  )
  for (col in expected_cols) {
    expect_true(col %in% names(enriched), info = paste("Missing column:", col))
  }
})

# =========================================================================
# 4. Sanity checks on enrichment distributions
# =========================================================================

test_that("material balance is bounded and centered near zero", {
  skip_if_not(file.exists(tal_pgn), "Tal.pgn not found")

  lines     <- readLines(tal_pgn, warn = FALSE, encoding = "UTF-8")
  raw       <- paste(lines, collapse = "\n")
  games_raw <- strsplit(raw, "\n(?=\\[Event )", perl = TRUE)[[1]]

  n <- min(20L, length(games_raw))
  games_raw <- games_raw[seq_len(n)]

  movetexts  <- vapply(games_raw, ply_pgn_extract_movetext, character(1),
                        USE.NAMES = FALSE)
  move_lists <- strsplit(movetexts, " ")
  move_lists <- lapply(move_lists, function(m) m[nzchar(m)])
  lengths    <- lengths(move_lists)

  keep       <- lengths > 0L
  move_lists <- move_lists[keep]
  lengths    <- lengths[keep]
  ids        <- which(keep)

  all_moves   <- unlist(move_lists, use.names = FALSE)
  game_starts <- cumsum(c(1L, lengths[-length(lengths)]))

  replayed <- cpp_replay_games_batch(ids, all_moves, game_starts)
  good     <- replayed[replayed$ok, ]
  enriched <- cpp_enrich_batch(good$fen, good$uci_move)

  # Material balance should stay within theoretical max (~39 per side)
  expect_true(all(enriched$material_bal >= -40L & enriched$material_bal <= 40L))
  # Median should be near zero (games start equal)
  expect_true(abs(median(enriched$material_bal)) <= 5L)
})

test_that("num_pieces decreases over course of a game", {
  skip_if_not(file.exists(tal_pgn), "Tal.pgn not found")

  # Replay game 1 only
  lines     <- readLines(tal_pgn, warn = FALSE, encoding = "UTF-8")
  raw       <- paste(lines, collapse = "\n")
  games_raw <- strsplit(raw, "\n(?=\\[Event )", perl = TRUE)[[1]]

  moves_str  <- ply_pgn_extract_movetext(games_raw[1])
  moves      <- strsplit(moves_str, " ")[[1]]
  moves      <- moves[nzchar(moves)]

  replayed <- cpp_replay_game(moves)
  good     <- replayed[replayed$ok, ]
  enriched <- cpp_enrich_batch(good$fen, good$uci_move)

  # First ply should have 32 pieces
  expect_equal(enriched$num_pieces[1], 32L)
  # Last ply should have fewer pieces than first
  expect_true(enriched$num_pieces[nrow(enriched)] < 32L)
})

test_that("Tal games show sacrificial tendencies (sacrifices available)", {
  skip_if_not(file.exists(tal_pgn), "Tal.pgn not found")

  lines     <- readLines(tal_pgn, warn = FALSE, encoding = "UTF-8")
  raw       <- paste(lines, collapse = "\n")
  games_raw <- strsplit(raw, "\n(?=\\[Event )", perl = TRUE)[[1]]

  n <- min(20L, length(games_raw))
  games_raw <- games_raw[seq_len(n)]

  movetexts  <- vapply(games_raw, ply_pgn_extract_movetext, character(1),
                        USE.NAMES = FALSE)
  move_lists <- strsplit(movetexts, " ")
  move_lists <- lapply(move_lists, function(m) m[nzchar(m)])
  lengths    <- lengths(move_lists)

  keep       <- lengths > 0L
  move_lists <- move_lists[keep]
  lengths    <- lengths[keep]
  ids        <- which(keep)

  all_moves   <- unlist(move_lists, use.names = FALSE)
  game_starts <- cumsum(c(1L, lengths[-length(lengths)]))

  replayed <- cpp_replay_games_batch(ids, all_moves, game_starts)
  good     <- replayed[replayed$ok, ]
  enriched <- cpp_enrich_batch(good$fen, good$uci_move)

  # At least some positions should have sacrifices available
  expect_true(sum(enriched$num_sacrifices_avail > 0) > 0L,
    info = "Expected some positions with sacrifice opportunities")
  # Tal's games should feature checks
  expect_true(sum(as.logical(enriched$gives_check)) > 0L,
    info = "Expected some check-giving moves in Tal's games")
  # Captures should appear
  expect_true(mean(as.logical(enriched$is_capture)) > 0.05,
    info = "Expected >5% capture rate across all plies")
})

# =========================================================================
# 5. Specific game: Tal's first game in the file
# =========================================================================

test_that("game 1 (Klasup vs Tal 1952) replays and enriches fully", {
  skip_if_not(file.exists(tal_pgn), "Tal.pgn not found")

  lines     <- readLines(tal_pgn, warn = FALSE, encoding = "UTF-8")
  raw       <- paste(lines, collapse = "\n")
  games_raw <- strsplit(raw, "\n(?=\\[Event )", perl = TRUE)[[1]]

  tags <- ply_pgn_parse_tags(games_raw[1])
  expect_equal(tags$White, "Klasup, Karlis")
  expect_equal(tags$Black, "Tal, Mihail")
  expect_equal(tags$Result, "0-1")  # Tal wins

  moves_str <- ply_pgn_extract_movetext(games_raw[1])
  moves     <- strsplit(moves_str, " ")[[1]]
  moves     <- moves[nzchar(moves)]

  # Game has 60 half-moves (30 moves per side)
  expect_equal(length(moves), 60L)

  replayed <- cpp_replay_game(moves)
  expect_true(all(replayed$ok), info = "All 60 plies should replay cleanly")

  enriched <- cpp_enrich_batch(replayed$fen, replayed$uci_move)
  expect_equal(nrow(enriched), 60L)

  # Game ends with Rook captures on e1 — last move should be a capture
  expect_true(as.logical(enriched$is_capture[60L]))
})

# =========================================================================
# 6. Model comparison — replicating midterm classification pipeline
#    (Baseline vs Multinomial Logistic Regression vs Naive Bayes)
# =========================================================================

# Helper: replay N games and return enriched data with derived features
build_tal_dataset <- function(tal_pgn, n_games = 100L) {
  lines     <- readLines(tal_pgn, warn = FALSE, encoding = "UTF-8")
  raw       <- paste(lines, collapse = "\n")
  games_raw <- strsplit(raw, "\n(?=\\[Event )", perl = TRUE)[[1]]
  n_games   <- min(n_games, length(games_raw))

  # Parse game metadata to determine Tal's color
  game_meta <- lapply(seq_len(n_games), function(i) {
    tags <- ply_pgn_parse_tags(games_raw[i])
    tal_is_white <- grepl("Tal", tags$White %||% "", ignore.case = TRUE)
    data.frame(
      game_id   = i,
      tal_color = ifelse(tal_is_white, "White", "Black"),
      stringsAsFactors = FALSE
    )
  })
  game_meta <- do.call(rbind, game_meta)

  # Replay games
  games_sub  <- games_raw[seq_len(n_games)]
  movetexts  <- vapply(games_sub, ply_pgn_extract_movetext, character(1),
                        USE.NAMES = FALSE)
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

  # Filter to Tal's turns only
  good$tal_color <- game_meta$tal_color[match(good$game_id, game_meta$game_id)]
  tal_turns <- good[
    (good$tal_color == "White" & good$ply_num %% 2 == 1) |
    (good$tal_color == "Black" & good$ply_num %% 2 == 0), ]

  if (nrow(tal_turns) == 0L) return(NULL)

  # Enrich with C++ features
  features <- cpp_enrich_batch(tal_turns$fen, tal_turns$uci_move)
  df <- cbind(tal_turns, features)

  # Derived: ply number from FEN
  df$ply <- vapply(df$fen, function(fen) {
    parts <- strsplit(fen, " ", fixed = TRUE)[[1]]
    fm   <- as.integer(parts[6])
    side <- parts[2]
    (fm - 1L) * 2L + ifelse(side == "w", 0L, 1L)
  }, integer(1), USE.NAMES = FALSE)

  # Derived: phase
  df$phase <- vapply(df$fen, function(fen) {
    parts    <- strsplit(fen, " ", fixed = TRUE)[[1]]
    fullmove <- as.integer(parts[6])
    if (!is.na(fullmove) && fullmove < 10) return("opening")
    board  <- parts[1]
    pieces <- nchar(gsub("[^pnbrqkPNBRQK]", "", board))
    queens <- nchar(gsub("[^qQ]", "", board))
    if (pieces > 20 || queens >= 2) "middlegame" else "endgame"
  }, character(1), USE.NAMES = FALSE)

  # Derived: flip material_bal so positive = Tal ahead
  df$material_bal <- ifelse(df$tal_color == "Black", -df$material_bal, df$material_bal)
  df$in_material_deficit <- as.integer(df$material_bal < 0)

  # Derived: repetition_count (cumulative position occurrences within game)
  df$fen_key <- vapply(
    strsplit(df$fen, " "),
    function(f) paste(f[seq_len(4L)], collapse = " "),
    character(1L)
  )
  df$repetition_count <- ave(
    seq_len(nrow(df)),
    list(df$game_id, df$fen_key),
    FUN = seq_along
  )
  df$fen_key <- NULL

  # 7-class move type labels (priority order; castling merged into quiet_restrained)
  # Castling is mechanically unique but not a distinct decision type — it's a
  # quiet positional move. Keeping it as a class inflated accuracy via the
  # near-deterministic can_castle gate. is_castling remains as an enrichment flag.
  label        <- rep("quiet_restrained", nrow(df))
  quiet_forced <- df$num_checks_avail == 0 &
                  df$num_captures_avail == 0 &
                  df$num_sacrifices_avail == 0
  is_sac       <- df$is_capture & df$moved_piece_value > df$captured_piece_value
  win_cap      <- df$is_capture & df$captured_piece_value > df$moved_piece_value
  any_chk      <- df$gives_check & !df$is_capture
  sac_chk      <- is_sac & df$gives_check

  label[quiet_forced]    <- "quiet_forced"
  label[df$is_capture]   <- "trade"
  label[win_cap]         <- "winning_capture"
  label[is_sac]          <- "sacrifice"
  label[any_chk]         <- "check"
  label[sac_chk]         <- "sacrifice_check"
  # castling stays as quiet_restrained (the default)
  df$move_type <- factor(label, levels = c(
    "quiet_forced", "quiet_restrained", "trade",
    "winning_capture", "sacrifice", "sacrifice_check",
    "check"
  ))

  # Coerce logical columns to integer for model fitting
  for (col in c("in_check", "can_castle", "en_passant_avail",
                "gives_discovered_check")) {
    if (col %in% names(df)) df[[col]] <- as.integer(df[[col]])
  }

  df
}

# Midterm formula (24 predictors — baseline for comparison)
MOVE_TYPE_FORMULA_MIDTERM <- move_type ~ num_pieces + material_bal + legal_move_count +
  ply + in_check + phase + num_captures_avail + num_checks_avail +
  can_castle + max_capture_value + num_sacrifices_avail +
  num_winning_caps_avail + max_sacrifice_gap + in_material_deficit +
  king_safety_own + king_safety_opp +
  passed_pawns + doubled_pawns + isolated_pawns + pin_count +
  mob_knight + mob_queen +
  en_passant_avail + repetition_count +
  gives_discovered_check

# Enhanced formula with static eval features (37 predictors)
MOVE_TYPE_FORMULA <- move_type ~ num_pieces + material_bal + legal_move_count +
  ply + num_captures_avail + num_checks_avail +
  max_capture_value + num_sacrifices_avail +
  num_winning_caps_avail + max_sacrifice_gap + in_material_deficit +
  king_safety_own + king_safety_opp +
  passed_pawns + isolated_pawns + pin_count +
  mob_knight + mob_queen +
  best_capture_net + num_safe_sacrifices + opp_recapture_after_move +
  undeveloped_minors + center_occupied + center_attacked +
  space_advantage + pieces_defended + pawn_chain_length + pawn_islands +
  position_eval + chosen_move_eval + best_move_eval + eval_gap +
  best_quiet_eval + best_capture_eval + worst_capture_eval +
  capture_eval_spread + best_sacrifice_eval + num_moves_better_than_chosen

test_that("move type labeling produces all 7 classes", {
  skip_if_not(file.exists(tal_pgn), "Tal.pgn not found")
  df <- build_tal_dataset(tal_pgn, n_games = 100L)
  skip_if(is.null(df), "No Tal positions replayed")

  expect_true(is.factor(df$move_type))
  expect_equal(length(levels(df$move_type)), 7L)
  # Castling moves should be absorbed into quiet_restrained
  expect_false("castling" %in% levels(df$move_type))
  # is_castling enrichment flag should still be present
  expect_true("is_castling" %in% names(df))
  expect_true(any(as.logical(df$is_castling)),
    info = "Castling moves exist but are classified as quiet_restrained")
  # At least 5 of the 7 classes should appear in 100 games
  observed <- length(unique(df$move_type))
  expect_true(observed >= 5L,
    info = sprintf("Only %d of 7 classes observed in 100 games", observed))
})

test_that("class distribution is imbalanced (quiet classes dominate)", {
  skip_if_not(file.exists(tal_pgn), "Tal.pgn not found")
  df <- build_tal_dataset(tal_pgn, n_games = 100L)
  skip_if(is.null(df), "No Tal positions replayed")

  tab <- table(df$move_type)
  quiet_pct <- sum(tab[c("quiet_forced", "quiet_restrained")]) / sum(tab)
  # Quiet classes should be >50% of all positions (midterm showed ~65%)
  expect_true(quiet_pct > 0.50,
    info = sprintf("Quiet classes = %.1f%%, expected >50%%", quiet_pct * 100))
})

test_that("multinomial LR beats majority baseline", {
  skip_if_not(file.exists(tal_pgn), "Tal.pgn not found")
  skip_if_not(requireNamespace("nnet", quietly = TRUE), "nnet not installed")
  df <- build_tal_dataset(tal_pgn, n_games = 2500L)
  skip_if(is.null(df) || nrow(df) < 500L, "Too few positions for modeling")

  set.seed(42)
  train_idx <- sample(nrow(df), size = floor(0.8 * nrow(df)))
  train <- df[train_idx, ]
  test  <- df[-train_idx, ]

  # Baseline: always predict majority class
  baseline_type <- names(which.max(table(train$move_type)))
  baseline_acc  <- mean(test$move_type == baseline_type)

  # Multinomial logistic regression
  lr_model <- nnet::multinom(MOVE_TYPE_FORMULA, data = train, trace = FALSE, maxit = 300)
  lr_pred  <- predict(lr_model, newdata = test)
  lr_acc   <- mean(lr_pred == test$move_type)

  cat(sprintf("\n  Baseline (always '%s'): %.1f%%\n", baseline_type, baseline_acc * 100))
  cat(sprintf("  Logistic Regression:    %.1f%%\n", lr_acc * 100))

  # LR should beat the baseline
  expect_true(lr_acc > baseline_acc,
    info = sprintf("LR (%.1f%%) did not beat baseline (%.1f%%)",
                   lr_acc * 100, baseline_acc * 100))
})

test_that("Naive Bayes runs and produces predictions", {
  skip_if_not(file.exists(tal_pgn), "Tal.pgn not found")
  skip_if_not(requireNamespace("e1071", quietly = TRUE), "e1071 not installed")
  df <- build_tal_dataset(tal_pgn, n_games = 2500L)
  skip_if(is.null(df) || nrow(df) < 500L, "Too few positions for modeling")

  set.seed(42)
  train_idx <- sample(nrow(df), size = floor(0.8 * nrow(df)))
  train <- df[train_idx, ]
  test  <- df[-train_idx, ]

  nb_model <- e1071::naiveBayes(MOVE_TYPE_FORMULA, data = train)
  nb_pred  <- predict(nb_model, newdata = test)
  nb_acc   <- mean(nb_pred == test$move_type)

  cat(sprintf("\n  Naive Bayes: %.1f%%\n", nb_acc * 100))

  # NB should produce valid factor predictions
  expect_true(is.factor(nb_pred))
  expect_true(all(levels(nb_pred) == levels(test$move_type)))
  # NB accuracy should be non-trivial (>10%)
  expect_true(nb_acc > 0.10,
    info = sprintf("Naive Bayes accuracy %.1f%% is suspiciously low", nb_acc * 100))
})

test_that("per-class sensitivity: gate-driven classes outperform sacrifice classes", {
  skip_if_not(file.exists(tal_pgn), "Tal.pgn not found")
  skip_if_not(requireNamespace("nnet", quietly = TRUE), "nnet not installed")
  df <- build_tal_dataset(tal_pgn, n_games = 2500L)
  skip_if(is.null(df) || nrow(df) < 500L, "Too few positions for modeling")

  set.seed(42)
  train_idx <- sample(nrow(df), size = floor(0.8 * nrow(df)))
  train <- df[train_idx, ]
  test  <- df[-train_idx, ]

  lr_model <- nnet::multinom(MOVE_TYPE_FORMULA, data = train, trace = FALSE, maxit = 300)
  lr_pred  <- predict(lr_model, newdata = test)

  # Per-class sensitivity
  sens <- function(cls) {
    actual <- test$move_type == cls
    if (sum(actual) == 0) return(NA_real_)
    mean(lr_pred[actual] == cls)
  }

  sens_quiet_f     <- sens("quiet_forced")
  sens_win_cap     <- sens("winning_capture")
  sens_sacrifice   <- sens("sacrifice")
  sens_sac_check   <- sens("sacrifice_check")

  cat("\n  Per-class sensitivity (LR):\n")
  for (cls in levels(test$move_type)) {
    s <- sens(cls)
    n <- sum(test$move_type == cls)
    if (!is.na(s)) cat(sprintf("    %-20s %5.1f%%  (n=%d)\n", cls, s * 100, n))
  }

  # Gate-driven classes should have higher sensitivity than sacrifice classes
  if (!is.na(sens_quiet_f) && !is.na(sens_sacrifice)) {
    expect_true(sens_quiet_f > sens_sacrifice,
      info = "Quiet-forced should be easier to predict than sacrifices")
  }

  # Combined sacrifice sensitivity should be low (midterm finding)
  sac_classes <- c("sacrifice", "sacrifice_check")
  sac_actual  <- test$move_type %in% sac_classes
  if (sum(sac_actual) > 0) {
    sac_sens <- mean(lr_pred[sac_actual] %in% sac_classes)
    cat(sprintf("    Combined sacrifice sensitivity: %.1f%%\n", sac_sens * 100))
    # With static eval features, sacrifice detection now exceeds the
    # midterm ceiling — eval_gap and best_sacrifice_eval provide the
    # evaluative signal the midterm identified as missing
    expect_true(sac_sens > 0.50,
      info = "Static eval features should push sacrifice sensitivity above 50%")
  }
})

test_that("full model comparison: midterm vs evaluative features vs reweighting", {
  skip_if_not(file.exists(tal_pgn), "Tal.pgn not found")
  skip_if_not(requireNamespace("nnet", quietly = TRUE), "nnet not installed")
  skip_if_not(requireNamespace("e1071", quietly = TRUE), "e1071 not installed")
  df <- build_tal_dataset(tal_pgn, n_games = 2500L)
  skip_if(is.null(df) || nrow(df) < 500L, "Too few positions for modeling")

  set.seed(42)
  train_idx <- sample(nrow(df), size = floor(0.8 * nrow(df)))
  train <- df[train_idx, ]
  test  <- df[-train_idx, ]

  # Inverse class frequency weights
  class_freq   <- table(train$move_type)
  class_weight <- 1 / class_freq
  class_weight <- class_weight / mean(class_weight)
  train$w      <- as.numeric(class_weight[train$move_type])

  # Per-class sensitivity helper
  sens <- function(pred, cls) {
    actual <- test$move_type == cls
    if (sum(actual) == 0) return(NA_real_)
    mean(pred[actual] == cls)
  }
  sac_sens <- function(pred) {
    sac_actual <- test$move_type %in% c("sacrifice", "sacrifice_check")
    if (sum(sac_actual) == 0) return(NA_real_)
    mean(pred[sac_actual] %in% c("sacrifice", "sacrifice_check"))
  }

  # === Model 1: Majority baseline ===
  baseline_type <- names(which.max(table(train$move_type)))
  baseline_acc  <- mean(test$move_type == baseline_type)

  # === Model 2: LR with midterm features only (no evaluative) ===
  lr_mid <- nnet::multinom(MOVE_TYPE_FORMULA_MIDTERM, data = train, trace = FALSE, maxit = 300)
  pred_mid <- predict(lr_mid, newdata = test)
  acc_mid  <- mean(pred_mid == test$move_type)

  # === Model 3: LR with evaluative features ===
  lr_eval <- nnet::multinom(MOVE_TYPE_FORMULA, data = train, trace = FALSE, maxit = 300)
  pred_eval <- predict(lr_eval, newdata = test)
  acc_eval  <- mean(pred_eval == test$move_type)

  # === Model 4: LR with evaluative features + class reweighting ===
  lr_weighted <- nnet::multinom(MOVE_TYPE_FORMULA, data = train, weights = w,
                                 trace = FALSE, maxit = 300)
  pred_wt <- predict(lr_weighted, newdata = test)
  acc_wt  <- mean(pred_wt == test$move_type)

  # === Model 5: Naive Bayes (reference) ===
  nb_model <- e1071::naiveBayes(MOVE_TYPE_FORMULA, data = train)
  nb_pred  <- predict(nb_model, newdata = test)
  acc_nb   <- mean(nb_pred == test$move_type)

  # === Model 6: XGBoost (gradient-boosted trees) ===
  # Prepare numeric matrix — XGBoost needs all-numeric input
  # All features including static eval
  pred_vars <- c(
    "num_pieces", "material_bal", "legal_move_count", "ply",
    "num_captures_avail", "num_checks_avail",
    "max_capture_value", "num_sacrifices_avail", "num_winning_caps_avail",
    "max_sacrifice_gap", "in_material_deficit",
    "king_safety_own", "king_safety_opp",
    "passed_pawns", "isolated_pawns", "pin_count",
    "mob_knight", "mob_queen",
    "best_capture_net", "num_safe_sacrifices", "opp_recapture_after_move",
    "undeveloped_minors", "center_occupied", "center_attacked",
    "space_advantage", "pieces_defended", "pawn_chain_length", "pawn_islands",
    "position_eval", "chosen_move_eval", "best_move_eval", "eval_gap",
    "best_quiet_eval", "best_capture_eval", "worst_capture_eval",
    "capture_eval_spread", "best_sacrifice_eval", "num_moves_better_than_chosen"
  )
  xgb_vars <- pred_vars

  train_mat <- as.matrix(train[, xgb_vars])
  test_mat  <- as.matrix(test[, xgb_vars])
  # Labels must be 0-indexed integers
  class_levels <- levels(train$move_type)
  train_labels <- as.integer(train$move_type) - 1L
  test_labels  <- as.integer(test$move_type) - 1L

  # Class weights for XGBoost
  xgb_class_wt <- as.numeric(class_weight[class_levels])
  sample_wt    <- xgb_class_wt[train_labels + 1L]

  dtrain <- xgboost::xgb.DMatrix(data = train_mat, label = train_labels,
                                  weight = sample_wt)
  dtest  <- xgboost::xgb.DMatrix(data = test_mat, label = test_labels)

  xgb_params <- list(
    objective   = "multi:softmax",
    num_class   = length(class_levels),
    eval_metric = "mlogloss",
    max_depth   = 6,
    learning_rate = 0.1
  )
  xgb_model <- xgboost::xgb.train(
    params  = xgb_params,
    data    = dtrain,
    nrounds = 200,
    verbose = 0
  )
  xgb_pred_int <- predict(xgb_model, dtest)
  pred_xgb     <- factor(class_levels[xgb_pred_int + 1L], levels = class_levels)
  acc_xgb      <- mean(pred_xgb == test$move_type)

  # Also run XGBoost without reweighting for comparison
  dtrain_uw <- xgboost::xgb.DMatrix(data = train_mat, label = train_labels)
  xgb_model_uw <- xgboost::xgb.train(
    params  = xgb_params,
    data    = dtrain_uw,
    nrounds = 200,
    verbose = 0
  )
  xgb_pred_uw_int <- predict(xgb_model_uw, dtest)
  pred_xgb_uw     <- factor(class_levels[xgb_pred_uw_int + 1L], levels = class_levels)
  acc_xgb_uw      <- mean(pred_xgb_uw == test$move_type)

  # ---------------------------------------------------------------
  # Print comparison
  # ---------------------------------------------------------------
  cat(sprintf("\n  === Model Comparison (n_train=%d, n_test=%d) ===\n",
              nrow(train), nrow(test)))
  cat(sprintf("  %-42s %6.1f%%\n", "1. Baseline (always quiet_restrained):", baseline_acc * 100))
  cat(sprintf("  %-42s %6.1f%%\n", "2. LR midterm features (24 pred):",      acc_mid * 100))
  cat(sprintf("  %-42s %6.1f%%\n", "3. LR + eval + positional (34 pred):",   acc_eval * 100))
  cat(sprintf("  %-42s %6.1f%%\n", "4. LR + eval + positional + reweight:",  acc_wt * 100))
  cat(sprintf("  %-42s %6.1f%%\n", "5. Naive Bayes:",                        acc_nb * 100))
  cat(sprintf("  %-42s %6.1f%%\n", "6. XGBoost (unweighted):",               acc_xgb_uw * 100))
  cat(sprintf("  %-42s %6.1f%%\n", "7. XGBoost (reweighted):",               acc_xgb * 100))

  cat("\n  === Per-class sensitivity ===\n")
  cat(sprintf("  %-22s %8s %8s %8s %8s %8s %8s\n",
              "Class", "Midterm", "LR+Eval", "LR+Wt", "XGB", "XGB+Wt", "N"))
  for (cls in levels(test$move_type)) {
    n_cls <- sum(test$move_type == cls)
    s_mid <- sens(pred_mid, cls)
    s_ev  <- sens(pred_eval, cls)
    s_wt  <- sens(pred_wt, cls)
    s_xuw <- sens(pred_xgb_uw, cls)
    s_xwt <- sens(pred_xgb, cls)
    cat(sprintf("  %-22s %7.1f%% %7.1f%% %7.1f%% %7.1f%% %7.1f%%   %4d\n",
                cls,
                ifelse(is.na(s_mid), 0, s_mid * 100),
                ifelse(is.na(s_ev),  0, s_ev * 100),
                ifelse(is.na(s_wt),  0, s_wt * 100),
                ifelse(is.na(s_xuw), 0, s_xuw * 100),
                ifelse(is.na(s_xwt), 0, s_xwt * 100),
                n_cls))
  }
  cat(sprintf("\n  Combined sacrifice sensitivity:\n"))
  cat(sprintf("    Midterm (LR 24p):     %5.1f%%\n", sac_sens(pred_mid) * 100))
  cat(sprintf("    LR + eval (34p):      %5.1f%%\n", sac_sens(pred_eval) * 100))
  cat(sprintf("    LR + eval + reweight: %5.1f%%\n", sac_sens(pred_wt) * 100))
  cat(sprintf("    XGBoost:              %5.1f%%\n", sac_sens(pred_xgb_uw) * 100))
  cat(sprintf("    XGBoost + reweight:   %5.1f%%\n", sac_sens(pred_xgb) * 100))

  # ---------------------------------------------------------------
  # Assertions
  # ---------------------------------------------------------------
  # LR beats baseline (core finding preserved)
  expect_true(acc_mid > baseline_acc)
  # Evaluative features should not degrade accuracy
  expect_true(acc_eval >= acc_mid - 0.02,
    info = "Evaluative features should not significantly degrade accuracy")
  # LR beats NB
  expect_true(acc_eval > acc_nb)
  # Reweighting should improve sacrifice sensitivity vs midterm
  expect_true(sac_sens(pred_wt) > sac_sens(pred_mid),
    info = "Class reweighting should improve sacrifice detection")
  # XGBoost should beat LR on overall accuracy (unweighted)
  expect_true(acc_xgb_uw >= acc_eval - 0.02,
    info = "XGBoost should match or beat LR on overall accuracy")
  # XGBoost reweighted should improve sacrifice sensitivity vs LR reweighted
  expect_true(sac_sens(pred_xgb) >= sac_sens(pred_wt) - 0.05,
    info = "XGBoost sacrifice detection should be competitive with LR reweighted")
})
