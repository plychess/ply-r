# 07_style_comparison.R — Tal vs Karpov style comparison with SF evals
#
# Step 7: Compare two contrasting styles using Stockfish ground truth.
# This informs the style-conditioned neural network design.
#
# Run from project root:
#   Rscript analysis/07_style_comparison.R

cat("============================================================\n")
cat("  Tal vs Karpov — Style Comparison with Stockfish Evals\n")
cat("============================================================\n\n")

# ---- Load training data ----
tal <- readRDS("training/data/Tal_d8.rds")
kar <- readRDS("training/data/Karpov_d8.rds")

tal$player <- "Tal"
kar$player <- "Karpov"

cat(sprintf("  Tal:    %s positions\n", format(nrow(tal), big.mark = ",")))
cat(sprintf("  Karpov: %s positions\n\n", format(nrow(kar), big.mark = ",")))

# =========================================================================
# 1. Stockfish eval distributions
# =========================================================================

cat("============================================================\n")
cat("  1. STOCKFISH EVAL DISTRIBUTIONS\n")
cat("============================================================\n\n")

sf_summary <- function(name, evals) {
  cat(sprintf("  %-8s mean=%+6.0f  median=%+5.0f  sd=%6.0f  range=[%+d, %+d]\n",
              name, mean(evals), median(evals), sd(evals), min(evals), max(evals)))
}

sf_summary("Tal", tal$sf_eval)
sf_summary("Karpov", kar$sf_eval)

# Filter out mate scores for cleaner stats
tal_clean <- tal$sf_eval[abs(tal$sf_eval) < 10000]
kar_clean <- kar$sf_eval[abs(kar$sf_eval) < 10000]
cat("\n  Without mate scores:\n")
sf_summary("Tal", tal_clean)
sf_summary("Karpov", kar_clean)

# =========================================================================
# 2. How often does each player deviate from Stockfish?
# =========================================================================

cat("\n============================================================\n")
cat("  2. DEVIATION FROM STOCKFISH (eval_gap)\n")
cat("============================================================\n\n")

cat(sprintf("  %-8s mean_gap=%+6.0f  median=%+5.0f  pct_suboptimal=%.1f%%\n",
            "Tal", mean(tal$eval_gap), median(tal$eval_gap),
            100 * mean(tal$eval_gap > 50)))
cat(sprintf("  %-8s mean_gap=%+6.0f  median=%+5.0f  pct_suboptimal=%.1f%%\n",
            "Karpov", mean(kar$eval_gap), median(kar$eval_gap),
            100 * mean(kar$eval_gap > 50)))

cat("\n  eval_gap distribution (centipawns):\n")
breaks <- c(0, 10, 25, 50, 100, 200, 500, Inf)
labels <- c("0-10", "10-25", "25-50", "50-100", "100-200", "200-500", "500+")
tal_bins <- table(cut(tal$eval_gap, breaks = breaks, labels = labels, right = FALSE))
kar_bins <- table(cut(kar$eval_gap, breaks = breaks, labels = labels, right = FALSE))
cat(sprintf("  %-8s", ""))
for (l in labels) cat(sprintf(" %8s", l))
cat("\n")
cat(sprintf("  %-8s", "Tal"))
for (l in labels) cat(sprintf(" %7.1f%%", 100 * tal_bins[l] / sum(tal_bins)))
cat("\n")
cat(sprintf("  %-8s", "Karpov"))
for (l in labels) cat(sprintf(" %7.1f%%", 100 * kar_bins[l] / sum(kar_bins)))
cat("\n")

# =========================================================================
# 3. Move type distribution comparison
# =========================================================================

cat("\n============================================================\n")
cat("  3. MOVE TYPE DISTRIBUTION\n")
cat("============================================================\n\n")

cat(sprintf("  %-22s %8s %8s %8s\n", "Move Type", "Tal", "Karpov", "Delta"))
cat(paste0("  ", paste(rep("-", 50), collapse = "")), "\n")

# Compute move types for both
label_moves <- function(df) {
  label <- rep("quiet_restrained", nrow(df))
  label[df$num_checks_avail == 0 & df$num_captures_avail == 0 &
        df$num_sacrifices_avail == 0] <- "quiet_forced"
  label[df$is_capture] <- "trade"
  label[df$is_capture & df$captured_piece_value > df$moved_piece_value] <- "winning_capture"
  is_sac <- df$is_capture & df$moved_piece_value > df$captured_piece_value
  label[is_sac] <- "sacrifice"
  label[df$gives_check] <- "check"
  factor(label, levels = c("quiet_forced", "quiet_restrained", "trade",
                            "winning_capture", "sacrifice", "check"))
}

tal$mt <- label_moves(tal)
kar$mt <- label_moves(kar)

tal_pct <- 100 * prop.table(table(tal$mt))
kar_pct <- 100 * prop.table(table(kar$mt))

for (cls in levels(tal$mt)) {
  t <- if (cls %in% names(tal_pct)) tal_pct[cls] else 0
  k <- if (cls %in% names(kar_pct)) kar_pct[cls] else 0
  cat(sprintf("  %-22s %7.1f%% %7.1f%% %+7.1f pp\n", cls, t, k, t - k))
}

# =========================================================================
# 4. Key feature comparison
# =========================================================================

cat("\n============================================================\n")
cat("  4. KEY FEATURE COMPARISON\n")
cat("============================================================\n\n")

feats <- c("material_bal", "king_safety_opp", "king_safety_own",
           "space_advantage", "num_captures_avail", "num_checks_avail",
           "num_safe_sacrifices", "pieces_defended", "passed_pawns",
           "isolated_pawns", "eval_gap", "num_moves_better_than_chosen",
           "best_capture_net", "legal_move_count")

cat(sprintf("  %-30s %8s %8s %10s\n", "Feature", "Tal", "Karpov", "Delta"))
cat(paste0("  ", paste(rep("-", 60), collapse = "")), "\n")

for (f in feats) {
  t <- mean(tal[[f]], na.rm = TRUE)
  k <- mean(kar[[f]], na.rm = TRUE)
  cat(sprintf("  %-30s %8.2f %8.2f %+9.2f\n", f, t, k, t - k))
}

# =========================================================================
# 5. Sacrifice rate by phase
# =========================================================================

cat("\n============================================================\n")
cat("  5. SACRIFICE RATE BY PHASE\n")
cat("============================================================\n\n")

# Compute phase
phase_fn <- function(fen) {
  parts <- strsplit(fen, " ", fixed = TRUE)[[1]]
  fullmove <- as.integer(parts[6])
  if (!is.na(fullmove) && fullmove < 10) return("opening")
  board <- parts[1]
  pieces <- nchar(gsub("[^pnbrqkPNBRQK]", "", board))
  queens <- nchar(gsub("[^qQ]", "", board))
  if (pieces > 20 || queens >= 2) "middlegame" else "endgame"
}

tal$phase <- vapply(tal$fen, phase_fn, character(1), USE.NAMES = FALSE)
kar$phase <- vapply(kar$fen, phase_fn, character(1), USE.NAMES = FALSE)

cat(sprintf("  %-8s %10s %10s %10s\n", "", "Opening", "Middle", "Endgame"))
cat(paste0("  ", paste(rep("-", 42), collapse = "")), "\n")

for (player in c("Tal", "Karpov")) {
  d <- if (player == "Tal") tal else kar
  rates <- sapply(c("opening", "middlegame", "endgame"), function(ph) {
    sub <- d[d$phase == ph, ]
    100 * mean(sub$mt == "sacrifice")
  })
  cat(sprintf("  %-8s %9.1f%% %9.1f%% %9.1f%%\n", player, rates[1], rates[2], rates[3]))
}

# =========================================================================
# 6. How often does each player play the "best" move?
# =========================================================================

cat("\n============================================================\n")
cat("  6. ACCURACY (eval_gap = 0 or near-0)\n")
cat("============================================================\n\n")

for (thresh in c(0, 10, 25, 50, 100)) {
  t <- 100 * mean(tal$eval_gap <= thresh)
  k <- 100 * mean(kar$eval_gap <= thresh)
  cat(sprintf("  eval_gap <= %3d cp:   Tal %5.1f%%   Karpov %5.1f%%   delta %+5.1f pp\n",
              thresh, t, k, t - k))
}

# =========================================================================
# 7. Classic eval vs Stockfish — by player
# =========================================================================

cat("\n============================================================\n")
cat("  7. CLASSIC EVAL vs STOCKFISH\n")
cat("============================================================\n\n")

cat(sprintf("  %-8s corr(classic, SF) = %+.4f\n", "Tal", cor(tal$position_eval, tal$sf_eval)))
cat(sprintf("  %-8s corr(classic, SF) = %+.4f\n", "Karpov", cor(kar$position_eval, kar$sf_eval)))
cat(sprintf("  %-8s corr(classic, SF) = %+.4f\n", "Combined",
            cor(c(tal$position_eval, kar$position_eval), c(tal$sf_eval, kar$sf_eval))))

cat("\n  This is why we need NNUE — our classic eval has near-zero\n")
cat("  correlation with Stockfish's evaluation.\n")

# =========================================================================
# 8. Style signature summary
# =========================================================================

cat("\n============================================================\n")
cat("  8. STYLE SIGNATURE SUMMARY\n")
cat("============================================================\n\n")

cat("  Tal (The Attacker):\n")
cat(sprintf("    Sacrifice rate:     %.1f%%\n", 100 * mean(tal$mt == "sacrifice")))
cat(sprintf("    Check rate:         %.1f%%\n", 100 * mean(tal$mt == "check")))
cat(sprintf("    Mean eval_gap:      %+.0f cp (deviates more from engine)\n", mean(tal$eval_gap)))
cat(sprintf("    Suboptimal moves:   %.1f%% (eval_gap > 50cp)\n", 100 * mean(tal$eval_gap > 50)))
cat(sprintf("    King safety opp:    %.2f (attacks opponent king more)\n", mean(tal$king_safety_opp)))

cat("\n  Karpov (The Defender):\n")
cat(sprintf("    Sacrifice rate:     %.1f%%\n", 100 * mean(kar$mt == "sacrifice")))
cat(sprintf("    Check rate:         %.1f%%\n", 100 * mean(kar$mt == "check")))
cat(sprintf("    Mean eval_gap:      %+.0f cp (plays closer to engine)\n", mean(kar$eval_gap)))
cat(sprintf("    Suboptimal moves:   %.1f%% (eval_gap > 50cp)\n", 100 * mean(kar$eval_gap > 50)))
cat(sprintf("    King safety opp:    %.2f (less aggressive toward king)\n", mean(kar$king_safety_opp)))

cat("\n============================================================\n")
cat("  Style comparison complete. Ready for Phase 2: NNUE training.\n")
cat("============================================================\n")
