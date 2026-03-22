# 08_reinforcement_learning.R — RL for chess move selection
#
# Step 8: Apply Monte Carlo Q-learning (from Class 5) to chess.
# Learn Q(state_features, move_type) from Tal's game outcomes.
#
# Parallels the tic-tac-toe example:
#   - State  = discretized position features
#   - Action = move type (6 classes)
#   - Reward = game outcome (+1 win, 0 draw, -1 loss)
#   - Policy = which move type maximizes expected return
#
# Run from project root:
#   Rscript analysis/08_reinforcement_learning.R

source("analysis/00_load_data.R")

cat("============================================================\n")
cat("  Tal Case Study — Step 8: Reinforcement Learning\n")
cat("============================================================\n\n")

# =========================================================================
# 1. Frame chess as an MDP
# =========================================================================

cat("============================================================\n")
cat("  1. FRAMING CHESS AS AN MDP\n")
cat("============================================================\n\n")

cat("  State:   Position features (discretized)\n")
cat("  Action:  Move type (6 classes)\n")
cat("  Reward:  Game outcome (+1 win, 0 draw, -1 loss)\n")
cat("  Episode: One complete game\n\n")

# Assign terminal reward per game
df$reward <- ifelse(df$tal_won, 1, ifelse(df$tal_lost, -1, 0))

# Sort by game and ply for correct episode ordering
df <- df[order(df$game_id, df$ply_num), ]

cat(sprintf("  Positions: %s\n", format(nrow(df), big.mark = ",")))
cat(sprintf("  Games:     %d\n", length(unique(df$game_id))))
cat(sprintf("  Outcomes:  Win=%.1f%%  Draw=%.1f%%  Loss=%.1f%%\n\n",
            100 * mean(df$reward == 1), 100 * mean(df$reward == 0),
            100 * mean(df$reward == -1)))

# =========================================================================
# 2. Compute discounted returns G_t for each position
# =========================================================================

cat("============================================================\n")
cat("  2. COMPUTING DISCOUNTED RETURNS\n")
cat("============================================================\n\n")

gamma <- 0.95  # discount factor

# For each position, G_t = gamma^(T-t) * reward
# where T = last ply of the game, t = current ply
df$steps_to_end <- ave(df$ply_num, df$game_id, FUN = function(x) max(x) - x)
df$G_t <- (gamma ^ df$steps_to_end) * df$reward

cat(sprintf("  Discount factor (gamma): %.2f\n", gamma))
cat(sprintf("  Mean return (G_t):       %+.3f\n", mean(df$G_t)))
cat(sprintf("  G_t by outcome:\n"))
cat(sprintf("    Wins:   %+.3f (mean steps_to_end=%.0f)\n",
            mean(df$G_t[df$reward == 1]), mean(df$steps_to_end[df$reward == 1])))
cat(sprintf("    Draws:  %+.3f\n", mean(df$G_t[df$reward == 0])))
cat(sprintf("    Losses: %+.3f (mean steps_to_end=%.0f)\n\n",
            mean(df$G_t[df$reward == -1]), mean(df$steps_to_end[df$reward == -1])))

# =========================================================================
# 3. Discretize state space
# =========================================================================

cat("============================================================\n")
cat("  3. DISCRETIZING STATE SPACE\n")
cat("============================================================\n\n")

# Create a coarse state representation from key features
# (following the tic-tac-toe approach of using string state keys)
discretize <- function(x, breaks) {
  cut(x, breaks = breaks, labels = FALSE, include.lowest = TRUE)
}

df$state_key <- paste(
  # Material: behind, equal, ahead
  discretize(df$material_bal, c(-Inf, -1, 1, Inf)),
  # Game phase proxy: pieces on board
  discretize(df$num_pieces, c(0, 15, 25, Inf)),
  # King safety: own king safe/danger
  discretize(df$king_safety_own, c(-Inf, 1, 3, Inf)),
  # Opponent king exposure
  discretize(df$king_safety_opp, c(-Inf, 1, 3, Inf)),
  # Tactical tension: captures available
  discretize(df$num_captures_avail, c(-Inf, 0, 2, 5, Inf)),
  # Checks available
  discretize(df$num_checks_avail, c(-Inf, 0, 1, Inf)),
  sep = "|"
)

n_states <- length(unique(df$state_key))
cat(sprintf("  Discrete states: %d\n", n_states))
cat(sprintf("  Actions (move types): %d\n", nlevels(df$move_type)))
cat(sprintf("  State-action pairs: %d (theoretical max: %d)\n\n",
            length(unique(paste(df$state_key, df$move_type))),
            n_states * nlevels(df$move_type)))

# =========================================================================
# 4. Monte Carlo Q-learning (every-visit)
# =========================================================================

cat("============================================================\n")
cat("  4. MONTE CARLO Q-LEARNING\n")
cat("============================================================\n\n")

# Following the tic-tac-toe code: incremental mean update
# Q(s,a) <- Q(s,a) + (1/N)(G_t - Q(s,a))

Q <- new.env(parent = emptyenv())
N <- new.env(parent = emptyenv())

for (i in seq_len(nrow(df))) {
  s <- df$state_key[i]
  a <- as.character(df$move_type[i])
  key <- paste(s, "||", a, sep = "")
  G <- df$G_t[i]

  n_old <- if (!is.null(N[[key]])) N[[key]] else 0L
  q_old <- if (!is.null(Q[[key]])) Q[[key]] else 0
  n_new <- n_old + 1L
  q_new <- q_old + (G - q_old) / n_new
  N[[key]] <- n_new
  Q[[key]] <- q_new
}

q_keys <- ls(Q)
cat(sprintf("  Q-values learned: %d state-action pairs\n", length(q_keys)))
cat(sprintf("  Total visits: %s\n\n", format(nrow(df), big.mark = ",")))

# =========================================================================
# 5. Extract greedy policy
# =========================================================================

cat("============================================================\n")
cat("  5. GREEDY POLICY\n")
cat("============================================================\n\n")

states <- unique(df$state_key)
actions <- levels(df$move_type)

policy <- data.frame(state = character(), best_action = character(),
                      Q_best = numeric(), n_visits = integer(),
                      stringsAsFactors = FALSE)

for (s in states) {
  qs <- sapply(actions, function(a) {
    key <- paste(s, "||", a, sep = "")
    if (!is.null(Q[[key]])) Q[[key]] else NA_real_
  })
  ns <- sapply(actions, function(a) {
    key <- paste(s, "||", a, sep = "")
    if (!is.null(N[[key]])) N[[key]] else 0L
  })

  # Only consider actions actually observed
  valid <- !is.na(qs)
  if (sum(valid) == 0) next

  best <- names(which.max(qs[valid]))
  policy <- rbind(policy, data.frame(
    state = s, best_action = best,
    Q_best = qs[best], n_visits = sum(ns),
    stringsAsFactors = FALSE
  ))
}

cat(sprintf("  States with policy: %d / %d\n", nrow(policy), n_states))

# What does the policy recommend most often?
pol_tab <- table(policy$best_action)
cat("\n  Recommended action distribution:\n")
for (a in names(sort(pol_tab, decreasing = TRUE))) {
  cat(sprintf("    %-22s %d states (%.1f%%)\n", a, pol_tab[a],
              100 * pol_tab[a] / sum(pol_tab)))
}

# =========================================================================
# 6. Q-values by move type (averaged across states)
# =========================================================================

cat("\n============================================================\n")
cat("  6. AVERAGE Q-VALUES BY MOVE TYPE\n")
cat("============================================================\n\n")

cat(sprintf("  %-22s %8s %8s %8s\n", "Move Type", "Mean Q", "N", "% Positive"))
cat(paste0("  ", paste(rep("-", 52), collapse = "")), "\n")

for (a in actions) {
  # Collect all Q(s,a) for this action
  q_vals <- numeric()
  for (s in states) {
    key <- paste(s, "||", a, sep = "")
    if (!is.null(Q[[key]])) q_vals <- c(q_vals, Q[[key]])
  }
  if (length(q_vals) > 0) {
    cat(sprintf("  %-22s %+7.3f %8d %7.1f%%\n",
                a, mean(q_vals), length(q_vals), 100 * mean(q_vals > 0)))
  }
}

# =========================================================================
# 7. Q-values by state context
# =========================================================================

cat("\n============================================================\n")
cat("  7. Q-VALUES IN KEY SITUATIONS\n")
cat("============================================================\n\n")

# Material behind, middlegame, opponent king exposed, captures available
key_states <- list(
  "Attacking (behind, opp king exposed, captures)" = c("1|2|1|3|3|2", "1|2|1|3|4|2", "1|2|1|3|3|3"),
  "Quiet (equal, safe kings, few captures)" = c("2|2|1|1|2|1", "2|2|1|1|1|1", "2|3|1|1|2|1"),
  "Endgame (ahead, few pieces)" = c("3|1|1|1|2|1", "3|1|1|1|1|1", "3|1|1|2|2|1"),
  "Under pressure (behind, own king exposed)" = c("1|2|3|1|2|1", "1|2|2|1|2|1", "1|2|3|1|3|1")
)

for (situation in names(key_states)) {
  cat(sprintf("  --- %s ---\n", situation))
  s_keys <- key_states[[situation]]
  found <- FALSE
  for (s in s_keys) {
    qs <- sapply(actions, function(a) {
      key <- paste(s, "||", a, sep = "")
      if (!is.null(Q[[key]])) Q[[key]] else NA_real_
    })
    valid <- !is.na(qs)
    if (sum(valid) >= 3) {
      for (a in names(sort(qs[valid], decreasing = TRUE))) {
        cat(sprintf("    %-22s Q=%+.3f\n", a, qs[a]))
      }
      found <- TRUE
      break
    }
  }
  if (!found) cat("    (insufficient data for this state)\n")
  cat("\n")
}

# =========================================================================
# 8. Policy agreement with Tal's actual choices
# =========================================================================

cat("============================================================\n")
cat("  8. POLICY vs TAL'S ACTUAL CHOICES\n")
cat("============================================================\n\n")

# For each position, does the RL policy agree with Tal?
df$rl_action <- sapply(df$state_key, function(s) {
  p <- policy$best_action[policy$state == s]
  if (length(p) == 0) NA_character_ else p[1]
})

df$rl_agrees <- df$rl_action == as.character(df$move_type)

agree_rate <- mean(df$rl_agrees, na.rm = TRUE)
cat(sprintf("  Overall policy agreement with Tal: %.1f%%\n\n", 100 * agree_rate))

# Agreement by game outcome
cat("  Agreement by game outcome:\n")
for (outcome in c("Wins", "Draws", "Losses")) {
  sub <- switch(outcome,
    Wins = df[df$reward == 1, ],
    Draws = df[df$reward == 0, ],
    Losses = df[df$reward == -1, ])
  rate <- mean(sub$rl_agrees, na.rm = TRUE)
  cat(sprintf("    %-8s %.1f%%\n", outcome, 100 * rate))
}

# Agreement by move type
cat("\n  Agreement by move type:\n")
for (a in actions) {
  sub <- df[df$move_type == a, ]
  rate <- mean(sub$rl_agrees, na.rm = TRUE)
  cat(sprintf("    %-22s %.1f%%  (n=%s)\n", a, 100 * rate,
              format(nrow(sub), big.mark = ",")))
}

# =========================================================================
# 9. When RL disagrees with Tal, who is right?
# =========================================================================

cat("\n============================================================\n")
cat("  9. WHEN RL DISAGREES, WHO WINS?\n")
cat("============================================================\n\n")

disagree <- df[!df$rl_agrees & !is.na(df$rl_agrees), ]
agree    <- df[df$rl_agrees & !is.na(df$rl_agrees), ]

cat(sprintf("  When RL agrees with Tal:     win rate = %.1f%%  (n=%s)\n",
            100 * mean(agree$reward == 1), format(nrow(agree), big.mark = ",")))
cat(sprintf("  When RL disagrees with Tal:  win rate = %.1f%%  (n=%s)\n",
            100 * mean(disagree$reward == 1), format(nrow(disagree), big.mark = ",")))
cat(sprintf("  When RL agrees:              loss rate = %.1f%%\n",
            100 * mean(agree$reward == -1)))
cat(sprintf("  When RL disagrees:           loss rate = %.1f%%\n",
            100 * mean(disagree$reward == -1)))

# =========================================================================
# 10. Compare: Supervised classifier vs RL policy
# =========================================================================

cat("\n============================================================\n")
cat("  10. SUPERVISED CLASSIFIER vs RL POLICY\n")
cat("============================================================\n\n")

cat("  Supervised (XGBoost): 'What type of move IS this?'\n")
cat("    → Classifies the actual move played (96.5% accuracy)\n")
cat("    → Requires knowing the move (post-hoc)\n\n")
cat("  RL (Q-learning): 'What type of move SHOULD be played?'\n")
cat(sprintf("    → Recommends the move type that maximizes expected return\n"))
cat(sprintf("    → Agreement with Tal: %.1f%%\n", 100 * agree_rate))
cat(sprintf("    → Position-only (no move information needed)\n\n"))

cat("  Key difference: The classifier tells you WHAT happened.\n")
cat("  The RL policy tells you what SHOULD happen to maximize winning.\n")

# =========================================================================
# 11. Summary
# =========================================================================

cat("\n============================================================\n")
cat("  11. SUMMARY\n")
cat("============================================================\n\n")

cat(sprintf("  States: %d | Actions: %d | Q-values: %d\n",
            n_states, nlevels(df$move_type), length(q_keys)))
cat(sprintf("  Discount factor: %.2f\n", gamma))
cat(sprintf("  Policy agreement with Tal: %.1f%%\n", 100 * agree_rate))
cat(sprintf("  Win rate when RL agrees: %.1f%%\n", 100 * mean(agree$reward == 1)))
cat(sprintf("  Win rate when RL disagrees: %.1f%%\n", 100 * mean(disagree$reward == 1)))

cat("\n============================================================\n")
cat("  RL analysis complete.\n")
cat("============================================================\n")
