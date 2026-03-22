# 06_position_only.R — Position-only vs move-aware features
#
# Step 6 of incremental analysis. The honest test: how much of our
# 96.5% accuracy is genuine prediction from the position, vs post-hoc
# classification that requires knowing which move was played?
#
# Run from project root:
#   Rscript analysis/06_position_only.R

library(xgboost)
source("analysis/00_load_data.R")

cat("============================================================\n")
cat("  Tal Case Study — Step 6: Position-Only vs Move-Aware\n")
cat("============================================================\n\n")

# =========================================================================
# 1. Split features into position-only vs move-aware
# =========================================================================

# Position-only: things you know BEFORE seeing the move
# These describe the board state, not the chosen move
position_only <- c(
  "material_bal", "legal_move_count",
  "num_captures_avail", "num_checks_avail",
  "max_capture_value", "num_winning_caps_avail", "in_material_deficit",
  "king_safety_own", "king_safety_opp",
  "passed_pawns", "isolated_pawns", "pin_count",
  "mob_knight", "mob_queen",
  "best_capture_net", "num_safe_sacrifices",   # available options (position)
  "undeveloped_minors", "center_occupied", "center_attacked",
  "space_advantage", "pieces_defended", "pawn_chain_length", "pawn_islands",
  "best_sacrifice_eval",                       # eval of best sacrifice (position)
  "num_moves_better_than_chosen"               # requires knowing the move — MOVE-AWARE
)

# Actually, let's be strict. These require knowing the chosen move:
#   chosen_move_eval     — eval AFTER the chosen move
#   eval_gap             — best_move_eval - chosen_move_eval
#   opp_recapture_after_move — can opponent recapture the moved piece
#   num_moves_better_than_chosen — how many moves score higher than chosen
#
# These describe available options (position-only):
#   num_safe_sacrifices  — count of safe sacrifices available
#   best_sacrifice_eval  — eval of best available sacrifice
#   best_capture_net     — best net capture value available

pos_vars <- c(
  "material_bal", "legal_move_count",
  "num_captures_avail", "num_checks_avail",
  "max_capture_value", "num_winning_caps_avail", "in_material_deficit",
  "king_safety_own", "king_safety_opp",
  "passed_pawns", "isolated_pawns", "pin_count",
  "mob_knight", "mob_queen",
  "best_capture_net", "num_safe_sacrifices",
  "undeveloped_minors", "center_occupied", "center_attacked",
  "space_advantage", "pieces_defended", "pawn_chain_length", "pawn_islands",
  "best_sacrifice_eval"
)

# Move-aware: require knowing which move was played
move_aware <- c(
  "chosen_move_eval",
  "eval_gap",
  "opp_recapture_after_move",
  "num_moves_better_than_chosen"
)

# All features (current model)
all_vars <- c(pos_vars, move_aware)

cat(sprintf("  Position-only features: %d\n", length(pos_vars)))
cat(sprintf("  Move-aware features:    %d\n", length(move_aware)))
cat(sprintf("  Total (current model):  %d\n\n", length(all_vars)))

cat("  Move-aware features (require knowing the chosen move):\n")
for (f in move_aware) cat(sprintf("    - %s\n", f))
cat("\n")

# =========================================================================
# 2. Train three models
# =========================================================================

set.seed(42)
train_idx <- sample(nrow(df), size = floor(0.8 * nrow(df)))
train <- df[train_idx, ]
test  <- df[-train_idx, ]

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

# --- Model A: position-only (24 features) ---
cat("Training Model A: position-only...\n")
dtrain_a <- xgb.DMatrix(data = as.matrix(train[, pos_vars]), label = train_labels)
dtest_a  <- xgb.DMatrix(data = as.matrix(test[, pos_vars]),  label = test_labels)
model_a  <- xgb.train(params = xgb_params, data = dtrain_a, nrounds = 200, verbose = 0)
pred_a   <- factor(class_levels[predict(model_a, dtest_a) + 1L], levels = class_levels)

# --- Model B: move-aware only (4 features) ---
cat("Training Model B: move-aware only...\n")
dtrain_b <- xgb.DMatrix(data = as.matrix(train[, move_aware]), label = train_labels)
dtest_b  <- xgb.DMatrix(data = as.matrix(test[, move_aware]),  label = test_labels)
model_b  <- xgb.train(params = xgb_params, data = dtrain_b, nrounds = 200, verbose = 0)
pred_b   <- factor(class_levels[predict(model_b, dtest_b) + 1L], levels = class_levels)

# --- Model C: all features (28) ---
cat("Training Model C: all features...\n")
dtrain_c <- xgb.DMatrix(data = as.matrix(train[, all_vars]), label = train_labels)
dtest_c  <- xgb.DMatrix(data = as.matrix(test[, all_vars]),  label = test_labels)
model_c  <- xgb.train(params = xgb_params, data = dtrain_c, nrounds = 200, verbose = 0)
pred_c   <- factor(class_levels[predict(model_c, dtest_c) + 1L], levels = class_levels)

# Baseline
baseline_acc <- mean(test$move_type == names(which.max(table(train$move_type))))

# =========================================================================
# 3. Overall comparison
# =========================================================================

acc_a <- mean(pred_a == test$move_type)
acc_b <- mean(pred_b == test$move_type)
acc_c <- mean(pred_c == test$move_type)

cat("\n============================================================\n")
cat("  3. OVERALL COMPARISON\n")
cat("============================================================\n\n")

cat(sprintf("  %-40s %6.1f%%\n", "Baseline (always quiet_restrained):", 100 * baseline_acc))
cat(sprintf("  %-40s %6.1f%%  (%d features)\n", "Model A: position-only:", 100 * acc_a, length(pos_vars)))
cat(sprintf("  %-40s %6.1f%%  (%d features)\n", "Model B: move-aware only:", 100 * acc_b, length(move_aware)))
cat(sprintf("  %-40s %6.1f%%  (%d features)\n", "Model C: all features:", 100 * acc_c, length(all_vars)))

cat(sprintf("\n  Position-only contribution:  %.1f pp above baseline\n",
            100 * (acc_a - baseline_acc)))
cat(sprintf("  Move-aware boost:            +%.1f pp above position-only\n",
            100 * (acc_c - acc_a)))
cat(sprintf("  Move-aware alone:            %.1f pp above baseline\n",
            100 * (acc_b - baseline_acc)))

pct_position <- (acc_a - baseline_acc) / (acc_c - baseline_acc) * 100
cat(sprintf("\n  Of the total lift (%.1f pp), position features explain %.0f%%\n",
            100 * (acc_c - baseline_acc), pct_position))

# =========================================================================
# 4. Per-class sensitivity comparison
# =========================================================================

cat("\n============================================================\n")
cat("  4. PER-CLASS SENSITIVITY\n")
cat("============================================================\n\n")

cat(sprintf("  %-22s %10s %10s %10s %6s\n",
            "Class", "Pos-Only", "Move-Aware", "All", "N"))
cat(paste0("  ", paste(rep("-", 62), collapse = "")), "\n")

for (cls in class_levels) {
  actual <- test$move_type == cls
  n_cls  <- sum(actual)
  if (n_cls == 0) next
  s_a <- 100 * mean(pred_a[actual] == cls)
  s_b <- 100 * mean(pred_b[actual] == cls)
  s_c <- 100 * mean(pred_c[actual] == cls)
  cat(sprintf("  %-22s %9.1f%% %9.1f%% %9.1f%%  %5d\n",
              cls, s_a, s_b, s_c, n_cls))
}

# =========================================================================
# 5. What does position-only get right/wrong?
# =========================================================================

cat("\n============================================================\n")
cat("  5. WHAT POSITION-ONLY GETS RIGHT\n")
cat("============================================================\n\n")

# Classes where position-only is close to full model
for (cls in class_levels) {
  actual <- test$move_type == cls
  n_cls  <- sum(actual)
  if (n_cls == 0) next
  s_pos <- mean(pred_a[actual] == cls)
  s_all <- mean(pred_c[actual] == cls)
  gap <- s_all - s_pos
  status <- if (gap < 0.05) "GOOD (pos-only sufficient)" else
            if (gap < 0.15) "MODERATE (move-aware helps)" else
            "WEAK (needs move-aware)"
  cat(sprintf("  %-22s pos=%.1f%%  all=%.1f%%  gap=%+.1f pp  → %s\n",
              cls, 100 * s_pos, 100 * s_all, 100 * gap, status))
}

# =========================================================================
# 6. Feature importance: position-only model
# =========================================================================

cat("\n============================================================\n")
cat("  6. FEATURE IMPORTANCE (position-only model)\n")
cat("============================================================\n\n")

imp_a <- xgb.importance(feature_names = pos_vars, model = model_a)
cat(sprintf("  %-30s %8s\n", "Feature", "Gain"))
cat(paste0("  ", paste(rep("-", 42), collapse = "")), "\n")
for (i in seq_len(min(15, nrow(imp_a)))) {
  cat(sprintf("  %-30s %7.1f%%\n", imp_a$Feature[i], 100 * imp_a$Gain[i]))
}

# =========================================================================
# 7. The honest answer
# =========================================================================

cat("\n============================================================\n")
cat("  7. THE HONEST ANSWER\n")
cat("============================================================\n\n")

cat(sprintf("  Can we predict move type from the position alone?\n\n"))
cat(sprintf("  Position-only accuracy:  %.1f%% (vs %.1f%% baseline)\n", 100 * acc_a, 100 * baseline_acc))
cat(sprintf("  Full model accuracy:     %.1f%%\n", 100 * acc_c))
cat(sprintf("  Gap:                     %.1f pp\n\n", 100 * (acc_c - acc_a)))

if (pct_position > 70) {
  cat("  VERDICT: YES — position features carry most of the signal.\n")
  cat(sprintf("  %.0f%% of the model's lift comes from position-only features.\n", pct_position))
  cat("  The move-aware features add refinement, not the core signal.\n")
} else if (pct_position > 40) {
  cat("  VERDICT: PARTIALLY — position features carry significant signal,\n")
  cat("  but move-aware features provide a substantial boost.\n")
  cat(sprintf("  %.0f%% position / %.0f%% move-aware split.\n", pct_position, 100 - pct_position))
} else {
  cat("  VERDICT: NO — most accuracy comes from knowing the move.\n")
  cat(sprintf("  Only %.0f%% of lift is from position features.\n", pct_position))
  cat("  The model is classifying, not predicting.\n")
}

cat("\n============================================================\n")
cat("  Position-only analysis complete.\n")
cat("============================================================\n")
