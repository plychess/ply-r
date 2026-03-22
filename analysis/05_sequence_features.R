# 05_sequence_features.R — Sequence features: trends over prior moves
#
# Step 5 of incremental analysis. Goal: do trends in features over
# consecutive moves help predict move type — especially checks?
#
# The hypothesis: sacrifices and checks have a BUILDUP over 3-4 moves.
# A single snapshot misses this. Sequence features capture the trajectory.
#
# Run from project root:
#   Rscript analysis/05_sequence_features.R

library(xgboost)
source("analysis/00_load_data.R")

cat("============================================================\n")
cat("  Tal Case Study — Step 5: Sequence Features\n")
cat("============================================================\n\n")

# =========================================================================
# 1. Compute sequence features (lags, deltas, trends)
# =========================================================================

cat("Computing sequence features...\n")

# Key features to track across moves (from feature importance + misclassification)
seq_feats <- c("king_safety_opp", "num_captures_avail", "space_advantage",
               "num_checks_avail", "eval_gap")

# Sort by game and ply to ensure correct ordering
df <- df[order(df$game_id, df$ply_num), ]

# For each feature, compute lag1, lag2, delta1, delta2, trend3
# within each game (Tal's moves only, already filtered)
for (feat in seq_feats) {
  vals <- df[[feat]]
  gid  <- df$game_id

  lag1 <- rep(NA_real_, nrow(df))
  lag2 <- rep(NA_real_, nrow(df))

  # Compute lags within each game
  games <- unique(gid)
  for (g in games) {
    idx <- which(gid == g)
    n <- length(idx)
    if (n >= 2) lag1[idx[2:n]] <- vals[idx[1:(n-1)]]
    if (n >= 3) lag2[idx[3:n]] <- vals[idx[1:(n-2)]]
  }

  delta1 <- vals - lag1
  delta2 <- lag1 - lag2

  # Trend: slope over last 3 values (current + 2 lags)
  trend3 <- rep(NA_real_, nrow(df))
  for (g in games) {
    idx <- which(gid == g)
    n <- length(idx)
    if (n >= 3) {
      for (k in 3:n) {
        y <- vals[idx[(k-2):k]]
        trend3[idx[k]] <- (y[3] - y[1]) / 2  # simple slope
      }
    }
  }

  # Assign to dataframe
  df[[paste0(feat, "_lag1")]]   <- lag1
  df[[paste0(feat, "_lag2")]]   <- lag2
  df[[paste0(feat, "_delta1")]] <- delta1
  df[[paste0(feat, "_delta2")]] <- delta2
  df[[paste0(feat, "_trend3")]] <- trend3
}

# Replace NAs with 0 (first 1-2 moves of each game have no history)
seq_cols <- grep("_(lag|delta|trend)", names(df), value = TRUE)
for (col in seq_cols) df[[col]][is.na(df[[col]])] <- 0

cat(sprintf("Added %d sequence features\n\n", length(seq_cols)))

# =========================================================================
# 2. Train models: with and without sequence features
# =========================================================================

set.seed(42)
train_idx <- sample(nrow(df), size = floor(0.8 * nrow(df)))
train <- df[train_idx, ]
test  <- df[-train_idx, ]

# Base features (same 28 from correlation-pruned model)
base_vars <- c(
  "material_bal", "legal_move_count",
  "num_captures_avail", "num_checks_avail",
  "max_capture_value", "num_winning_caps_avail", "in_material_deficit",
  "king_safety_own", "king_safety_opp",
  "passed_pawns", "isolated_pawns", "pin_count",
  "mob_knight", "mob_queen",
  "best_capture_net", "num_safe_sacrifices", "opp_recapture_after_move",
  "undeveloped_minors", "center_occupied", "center_attacked",
  "space_advantage", "pieces_defended", "pawn_chain_length", "pawn_islands",
  "chosen_move_eval", "eval_gap",
  "best_sacrifice_eval", "num_moves_better_than_chosen"
)

# Full features = base + sequence
full_vars <- c(base_vars, seq_cols)

class_levels <- levels(train$move_type)
train_labels <- as.integer(train$move_type) - 1L
test_labels  <- as.integer(test$move_type) - 1L

xgb_params <- list(
  objective     = "multi:softmax",
  num_class     = length(class_levels),
  eval_metric   = "mlogloss",
  max_depth     = 6,
  learning_rate = 0.1
)

# --- Model A: base features only (28) ---
cat("Training Model A: base features (28)...\n")
dtrain_a <- xgb.DMatrix(data = as.matrix(train[, base_vars]), label = train_labels)
dtest_a  <- xgb.DMatrix(data = as.matrix(test[, base_vars]),  label = test_labels)
model_a  <- xgb.train(params = xgb_params, data = dtrain_a, nrounds = 200, verbose = 0)
pred_a   <- predict(model_a, dtest_a)
pred_a_f <- factor(class_levels[pred_a + 1L], levels = class_levels)

# --- Model B: base + sequence features (28 + 25 = 53) ---
cat("Training Model B: base + sequence features...\n")
dtrain_b <- xgb.DMatrix(data = as.matrix(train[, full_vars]), label = train_labels)
dtest_b  <- xgb.DMatrix(data = as.matrix(test[, full_vars]),  label = test_labels)
model_b  <- xgb.train(params = xgb_params, data = dtrain_b, nrounds = 200, verbose = 0)
pred_b   <- predict(model_b, dtest_b)
pred_b_f <- factor(class_levels[pred_b + 1L], levels = class_levels)

# =========================================================================
# 3. Compare results
# =========================================================================

cat("\n============================================================\n")
cat("  3. MODEL COMPARISON\n")
cat("============================================================\n\n")

acc_a <- mean(pred_a_f == test$move_type)
acc_b <- mean(pred_b_f == test$move_type)

cat(sprintf("  Model A (28 base features):     %.1f%%\n", 100 * acc_a))
cat(sprintf("  Model B (28 + %d sequence):     %.1f%%\n", length(seq_cols), 100 * acc_b))
cat(sprintf("  Improvement:                    %+.1f pp\n", 100 * (acc_b - acc_a)))

# =========================================================================
# 4. Per-class sensitivity comparison
# =========================================================================

cat("\n============================================================\n")
cat("  4. PER-CLASS SENSITIVITY\n")
cat("============================================================\n\n")

cat(sprintf("  %-22s %10s %10s %10s %6s\n", "Class", "Base", "+Sequence", "Delta", "N"))
cat(paste0("  ", paste(rep("-", 62), collapse = "")), "\n")

for (cls in class_levels) {
  actual <- test$move_type == cls
  n_cls  <- sum(actual)
  if (n_cls == 0) next
  sens_a <- mean(pred_a_f[actual] == cls)
  sens_b <- mean(pred_b_f[actual] == cls)
  delta  <- sens_b - sens_a
  cat(sprintf("  %-22s %9.1f%% %9.1f%% %+9.1f%%  %5d\n",
              cls, 100 * sens_a, 100 * sens_b, 100 * delta, n_cls))
}

# =========================================================================
# 5. Feature importance of sequence features
# =========================================================================

cat("\n============================================================\n")
cat("  5. SEQUENCE FEATURE IMPORTANCE (in Model B)\n")
cat("============================================================\n\n")

imp_b <- xgb.importance(feature_names = full_vars, model = model_b)

# Show only sequence features
seq_imp <- imp_b[imp_b$Feature %in% seq_cols, ]
cat(sprintf("  %-35s %8s %8s\n", "Feature", "Gain", "Rank"))
cat(paste0("  ", paste(rep("-", 55), collapse = "")), "\n")

for (i in seq_len(nrow(seq_imp))) {
  rank <- which(imp_b$Feature == seq_imp$Feature[i])
  cat(sprintf("  %-35s %7.2f%% %5d/%d\n",
              seq_imp$Feature[i], 100 * seq_imp$Gain[i],
              rank, nrow(imp_b)))
}

total_seq_gain <- sum(seq_imp$Gain)
cat(sprintf("\n  Total sequence feature gain: %.1f%% of model\n", 100 * total_seq_gain))

# =========================================================================
# 6. Check class deep dive — did sequence features help?
# =========================================================================

cat("\n============================================================\n")
cat("  6. CHECK CLASS DEEP DIVE\n")
cat("============================================================\n\n")

check_actual <- test$move_type == "check"
n_check <- sum(check_actual)

check_sens_a <- mean(pred_a_f[check_actual] == "check")
check_sens_b <- mean(pred_b_f[check_actual] == "check")

# How many checks did we RECOVER (missed by A, caught by B)?
missed_a <- check_actual & pred_a_f != "check"
caught_b <- check_actual & pred_b_f == "check"
recovered <- sum(missed_a & caught_b)
lost <- sum(!missed_a & check_actual & pred_b_f != "check")

cat(sprintf("  Check sensitivity (base):     %.1f%% (%d/%d)\n",
            100 * check_sens_a, sum(pred_a_f[check_actual] == "check"), n_check))
cat(sprintf("  Check sensitivity (+seq):     %.1f%% (%d/%d)\n",
            100 * check_sens_b, sum(pred_b_f[check_actual] == "check"), n_check))
cat(sprintf("  Recovered (missed→caught):    %d positions\n", recovered))
cat(sprintf("  Lost (caught→missed):         %d positions\n", lost))
cat(sprintf("  Net gain:                     %+d positions\n", recovered - lost))

# Feature means for recovered checks
if (recovered > 0) {
  rec_idx <- which(missed_a & caught_b)
  cat("\n  What do recovered checks look like?\n")
  for (sf in seq_cols[1:min(10, length(seq_cols))]) {
    rec_mean <- mean(test[[sf]][rec_idx])
    all_mean <- mean(test[[sf]][check_actual])
    cat(sprintf("    %-35s recovered=%.2f  all_checks=%.2f\n", sf, rec_mean, all_mean))
  }
}

# =========================================================================
# 7. Summary
# =========================================================================

cat("\n============================================================\n")
cat("  7. SUMMARY\n")
cat("============================================================\n\n")

cat(sprintf("  Base model:       %d features, %.1f%% accuracy\n", length(base_vars), 100 * acc_a))
cat(sprintf("  +Sequence model:  %d features, %.1f%% accuracy\n", length(full_vars), 100 * acc_b))
cat(sprintf("  Improvement:      %+.1f pp overall\n", 100 * (acc_b - acc_a)))
cat(sprintf("  Check improvement: %+.1f pp\n", 100 * (check_sens_b - check_sens_a)))
cat(sprintf("  Sequence gain:    %.1f%% of total model gain\n", 100 * total_seq_gain))

cat("\n============================================================\n")
cat("  Sequence feature analysis complete.\n")
cat("============================================================\n")
