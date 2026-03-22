# 10_combined_insight.R — Combining classifier + RL for deeper insight
#
# The classifier says WHAT happened. RL says what SHOULD happen.
# Together they answer: WHEN does intuition diverge from optimality,
# and WHAT determines whether the divergence succeeds?
#
# Run from project root:
#   Rscript analysis/10_combined_insight.R

source("analysis/00_load_data.R")

cat("============================================================\n")
cat("  Tal Case Study — Step 10: Combining Classification + RL\n")
cat("============================================================\n\n")

# ---- Setup: same as steps 8-9 ----
df <- df[order(df$game_id, df$ply_num), ]
df$reward <- ifelse(df$tal_won, 1, ifelse(df$tal_lost, -1, 0))

discretize <- function(x, breaks) {
  cut(x, breaks = breaks, labels = FALSE, include.lowest = TRUE)
}

df$state_key <- paste(
  discretize(df$material_bal, c(-Inf, -1, 1, Inf)),
  discretize(df$num_pieces, c(0, 15, 25, Inf)),
  discretize(df$king_safety_own, c(-Inf, 1, 3, Inf)),
  discretize(df$king_safety_opp, c(-Inf, 1, 3, Inf)),
  discretize(df$num_captures_avail, c(-Inf, 0, 2, 5, Inf)),
  discretize(df$num_checks_avail, c(-Inf, 0, 1, Inf)),
  sep = "|"
)

actions <- levels(df$move_type)

# Run MC Q-learning
gamma <- 0.95
df$steps_to_end <- ave(df$ply_num, df$game_id, FUN = function(x) max(x) - x)
df$G_t <- (gamma ^ df$steps_to_end) * df$reward

Q <- new.env(parent = emptyenv())
N <- new.env(parent = emptyenv())
for (i in seq_len(nrow(df))) {
  key <- paste0(df$state_key[i], "||", as.character(df$move_type[i]))
  G <- df$G_t[i]
  n_old <- if (!is.null(N[[key]])) N[[key]] else 0L
  q_old <- if (!is.null(Q[[key]])) Q[[key]] else 0
  N[[key]] <- n_old + 1L
  Q[[key]] <- q_old + (G - q_old) / (n_old + 1L)
}

# For each position: what Tal played, what RL recommends, Q-values
df$tal_action <- as.character(df$move_type)
df$q_played <- sapply(seq_len(nrow(df)), function(i) {
  key <- paste0(df$state_key[i], "||", df$tal_action[i])
  if (!is.null(Q[[key]])) Q[[key]] else NA_real_
})

df$rl_action <- sapply(df$state_key, function(s) {
  qs <- sapply(actions, function(a) {
    key <- paste0(s, "||", a)
    if (!is.null(Q[[key]])) Q[[key]] else NA_real_
  })
  valid <- !is.na(qs)
  if (sum(valid) == 0) return(NA_character_)
  names(which.max(qs[valid]))
})

df$q_best <- sapply(df$state_key, function(s) {
  qs <- sapply(actions, function(a) {
    key <- paste0(s, "||", a)
    if (!is.null(Q[[key]])) Q[[key]] else NA_real_
  })
  if (all(is.na(qs))) return(NA_real_)
  max(qs, na.rm = TRUE)
})

df$rl_agrees <- df$tal_action == df$rl_action
df$q_regret <- df$q_best - df$q_played  # how much Q Tal "left on the table"

# =========================================================================
# 1. Four quadrants of play
# =========================================================================

cat("============================================================\n")
cat("  1. FOUR QUADRANTS OF PLAY\n")
cat("============================================================\n\n")

# Aggressive = sacrifice, sacrifice_check, check
# Quiet = quiet_forced, quiet_restrained, trade, winning_capture
is_aggressive_played <- df$tal_action %in% c("sacrifice", "sacrifice_check", "check")
is_aggressive_recommended <- df$rl_action %in% c("sacrifice", "sacrifice_check", "check")

df$quadrant <- ifelse(
  is_aggressive_played & is_aggressive_recommended, "A: Aggressive + RL agrees",
  ifelse(is_aggressive_played & !is_aggressive_recommended, "B: Aggressive + RL says quiet",
  ifelse(!is_aggressive_played & is_aggressive_recommended, "C: Quiet + RL says aggressive",
  "D: Quiet + RL agrees")))

quad_tab <- table(df$quadrant)
cat("  Quadrant                          Count      Pct   Win%  Loss%\n")
cat(paste0("  ", paste(rep("-", 68), collapse = "")), "\n")

for (q in sort(names(quad_tab))) {
  sub <- df[df$quadrant == q, ]
  cat(sprintf("  %-35s %6s  %5.1f%%  %5.1f%%  %5.1f%%\n",
              q, format(nrow(sub), big.mark = ","),
              100 * nrow(sub) / nrow(df),
              100 * mean(sub$reward == 1),
              100 * mean(sub$reward == -1)))
}

# =========================================================================
# 2. Quadrant B: Tal over-aggressive (speculative gambles)
# =========================================================================

cat("\n============================================================\n")
cat("  2. SPECULATIVE GAMBLES (Tal aggressive, RL says don't)\n")
cat("============================================================\n\n")

gambles <- df[df$quadrant == "B: Aggressive + RL says quiet", ]
non_gambles <- df[df$quadrant == "A: Aggressive + RL agrees", ]

cat(sprintf("  Speculative gambles: %s positions (%.1f%% of aggressive play)\n",
            format(nrow(gambles), big.mark = ","),
            100 * nrow(gambles) / sum(is_aggressive_played)))
cat(sprintf("  RL-approved aggression: %s positions\n\n",
            format(nrow(non_gambles), big.mark = ",")))

cat(sprintf("  Win rate:\n"))
cat(sprintf("    Gambles (RL disagrees):     %.1f%%\n", 100 * mean(gambles$reward == 1)))
cat(sprintf("    RL-approved aggression:     %.1f%%\n", 100 * mean(non_gambles$reward == 1)))
cat(sprintf("    All positions:              %.1f%%\n\n", 100 * mean(df$reward == 1)))

cat(sprintf("  Loss rate:\n"))
cat(sprintf("    Gambles:                    %.1f%%\n", 100 * mean(gambles$reward == -1)))
cat(sprintf("    RL-approved:                %.1f%%\n", 100 * mean(non_gambles$reward == -1)))

cat(sprintf("\n  Mean Q-regret (Q_best - Q_played):\n"))
cat(sprintf("    Gambles:     %+.3f (gave up this much expected value)\n",
            mean(gambles$q_regret, na.rm = TRUE)))
cat(sprintf("    RL-approved: %+.3f\n", mean(non_gambles$q_regret, na.rm = TRUE)))

# =========================================================================
# 3. Quadrant C: Missed opportunities (Tal quiet, RL says attack)
# =========================================================================

cat("\n============================================================\n")
cat("  3. MISSED OPPORTUNITIES (Tal quiet, RL says attack)\n")
cat("============================================================\n\n")

missed <- df[df$quadrant == "C: Quiet + RL says aggressive", ]
cat(sprintf("  Missed opportunities: %s positions (%.1f%% of quiet play)\n",
            format(nrow(missed), big.mark = ","),
            100 * nrow(missed) / sum(!is_aggressive_played)))

cat(sprintf("\n  Win rate when Tal missed the attack: %.1f%%\n",
            100 * mean(missed$reward == 1)))
cat(sprintf("  Win rate when Tal played quiet (RL agrees): %.1f%%\n",
            100 * mean(df$reward[df$quadrant == "D: Quiet + RL agrees"] == 1)))

cat(sprintf("  Q-regret: %+.3f (value left on the table)\n",
            mean(missed$q_regret, na.rm = TRUE)))

# What did the RL want Tal to do?
cat("\n  RL recommended instead:\n")
rl_tab <- table(missed$rl_action)
for (a in names(sort(rl_tab, decreasing = TRUE))) {
  cat(sprintf("    %-22s %d (%.1f%%)\n", a, rl_tab[a], 100 * rl_tab[a] / sum(rl_tab)))
}

# =========================================================================
# 4. Phase analysis of quadrants
# =========================================================================

cat("\n============================================================\n")
cat("  4. QUADRANTS BY GAME PHASE\n")
cat("============================================================\n\n")

df$phase <- vapply(df$fen, function(fen) {
  parts <- strsplit(fen, " ", fixed = TRUE)[[1]]
  fullmove <- as.integer(parts[6])
  if (!is.na(fullmove) && fullmove < 10) return("opening")
  board <- parts[1]
  pieces <- nchar(gsub("[^pnbrqkPNBRQK]", "", board))
  queens <- nchar(gsub("[^qQ]", "", board))
  if (pieces > 20 || queens >= 2) "middlegame" else "endgame"
}, character(1), USE.NAMES = FALSE)

cat(sprintf("  %-35s %8s %8s %8s\n", "Quadrant", "Opening", "Middle", "Endgame"))
cat(paste0("  ", paste(rep("-", 65), collapse = "")), "\n")

for (q in sort(unique(df$quadrant))) {
  rates <- sapply(c("opening", "middlegame", "endgame"), function(ph) {
    sub <- df[df$phase == ph, ]
    100 * mean(sub$quadrant == q)
  })
  cat(sprintf("  %-35s %7.1f%% %7.1f%% %7.1f%%\n", q, rates[1], rates[2], rates[3]))
}

# =========================================================================
# 5. Q-regret distribution
# =========================================================================

cat("\n============================================================\n")
cat("  5. Q-REGRET ANALYSIS\n")
cat("============================================================\n\n")

cat("  Q-regret = Q(best action) - Q(Tal's action)\n")
cat("  Higher regret = Tal chose further from RL's recommendation\n\n")

cat(sprintf("  Overall mean regret:     %+.3f\n", mean(df$q_regret, na.rm = TRUE)))
cat(sprintf("  Zero regret (RL agrees): %.1f%%\n", 100 * mean(df$q_regret == 0, na.rm = TRUE)))

cat("\n  Regret by game outcome:\n")
for (outcome in c("Win", "Draw", "Loss")) {
  sub <- switch(outcome, Win = df[df$reward == 1,], Draw = df[df$reward == 0,],
                Loss = df[df$reward == -1,])
  cat(sprintf("    %-6s  mean_regret=%+.3f  (Tal deviated more in %s)\n",
              outcome, mean(sub$q_regret, na.rm = TRUE),
              if (mean(sub$q_regret, na.rm = TRUE) > mean(df$q_regret, na.rm = TRUE))
                "these games" else "other games"))
}

cat("\n  Regret by move type:\n")
for (a in actions) {
  sub <- df[df$move_type == a, ]
  cat(sprintf("    %-22s  mean_regret=%+.3f\n", a, mean(sub$q_regret, na.rm = TRUE)))
}

# =========================================================================
# 6. The Combined Coaching Model
# =========================================================================

cat("\n============================================================\n")
cat("  6. THE COMBINED COACHING MODEL\n")
cat("============================================================\n\n")

cat("  For any position, we can now provide THREE pieces of information:\n\n")

cat("  1. CLASSIFIER: 'What type of move is this?' (96.5% accuracy)\n")
cat("     → Labels the actual move played after seeing it\n\n")

cat("  2. RL POLICY:  'What type should be played?' (Q-values)\n")
cat("     → Recommends the move type with highest expected return\n\n")

cat("  3. COMBINED:   'Was this the right choice?'\n")
cat("     → Compares actual choice to RL recommendation\n")
cat("     → Measures Q-regret (value left on the table)\n")
cat("     → Classifies the decision into four quadrants:\n\n")

cat("     ┌─────────────────────────────────────────────┐\n")
cat("     │                    RL says:                  │\n")
cat("     │            Aggressive    Quiet               │\n")
cat("     │  Tal    ┌────────────┬────────────┐          │\n")
cat("     │  played:│  GENIUS    │  GAMBLE    │          │\n")
cat("     │  Aggr   │  (Q agrees)│  (Q warns) │          │\n")
cat("     │         ├────────────┼────────────┤          │\n")
cat("     │  Quiet  │  MISSED    │  SOLID     │          │\n")
cat("     │         │  (Q wants) │  (Q agrees)│          │\n")
cat("     │         └────────────┴────────────┘          │\n")
cat("     └─────────────────────────────────────────────┘\n\n")

# Final stats
n_genius <- sum(df$quadrant == "A: Aggressive + RL agrees", na.rm = TRUE)
n_gamble <- sum(df$quadrant == "B: Aggressive + RL says quiet", na.rm = TRUE)
n_missed <- sum(df$quadrant == "C: Quiet + RL says aggressive", na.rm = TRUE)
n_solid  <- sum(df$quadrant == "D: Quiet + RL agrees", na.rm = TRUE)

cat(sprintf("  Tal's play distribution:\n"))
cat(sprintf("    GENIUS moves:   %5.1f%%  (aggressive, RL agrees)\n", 100 * n_genius / nrow(df)))
cat(sprintf("    GAMBLE moves:   %5.1f%%  (aggressive, RL warns)\n", 100 * n_gamble / nrow(df)))
cat(sprintf("    MISSED attacks: %5.1f%%  (quiet, RL wanted aggression)\n", 100 * n_missed / nrow(df)))
cat(sprintf("    SOLID moves:    %5.1f%%  (quiet, RL agrees)\n", 100 * n_solid / nrow(df)))

cat(sprintf("\n  Win rates:\n"))
cat(sprintf("    GENIUS:  %.1f%%\n", 100 * mean(df$reward[df$quadrant == "A: Aggressive + RL agrees"] == 1)))
cat(sprintf("    GAMBLE:  %.1f%%\n", 100 * mean(df$reward[df$quadrant == "B: Aggressive + RL says quiet"] == 1)))
cat(sprintf("    MISSED:  %.1f%%\n", 100 * mean(df$reward[df$quadrant == "C: Quiet + RL says aggressive"] == 1)))
cat(sprintf("    SOLID:   %.1f%%\n", 100 * mean(df$reward[df$quadrant == "D: Quiet + RL agrees"] == 1)))

cat("\n============================================================\n")
cat("  Combined insight analysis complete.\n")
cat("============================================================\n")
