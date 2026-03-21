# 02_feature_importance.R — Feature importance from XGBoost
#
# Step 2 of incremental analysis. Goal: which features actually drive
# the model's decisions? Confirms or challenges our EDA intuitions.
#
# Run from project root:
#   Rscript analysis/02_feature_importance.R

library(xgboost)

source("analysis/00_load_data.R")  # loads df if not already loaded

cat("============================================================\n")
cat("  Tal Case Study — Step 2: Feature Importance\n")
cat("============================================================\n\n")

set.seed(42)
train_idx <- sample(nrow(df), size = floor(0.8 * nrow(df)))
train <- df[train_idx, ]
test  <- df[-train_idx, ]

cat(sprintf("\nTrain: %s  |  Test: %s\n\n",
            format(nrow(train), big.mark = ","),
            format(nrow(test), big.mark = ",")))

# =========================================================================
# 2. Prepare XGBoost matrices
# =========================================================================

# Pruned by feature importance — removed <0.1% gain features
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
  "space_advantage", "pieces_defended", "pawn_chain_length", "pawn_islands"
)
xgb_vars <- pred_vars

train_mat <- as.matrix(train[, xgb_vars])
test_mat  <- as.matrix(test[, xgb_vars])

class_levels <- levels(train$move_type)
train_labels <- as.integer(train$move_type) - 1L
test_labels  <- as.integer(test$move_type) - 1L

# Class weights
class_freq   <- table(train$move_type)
class_weight <- 1 / class_freq
class_weight <- class_weight / mean(class_weight)
sample_wt    <- as.numeric(class_weight[train$move_type])

# =========================================================================
# 3. Train XGBoost (unweighted — for clean importance signal)
# =========================================================================

cat("Training XGBoost (unweighted)...\n")
dtrain <- xgb.DMatrix(data = train_mat, label = train_labels)
dtest  <- xgb.DMatrix(data = test_mat, label = test_labels)

xgb_params <- list(
  objective     = "multi:softmax",
  num_class     = length(class_levels),
  eval_metric   = "mlogloss",
  max_depth     = 6,
  learning_rate = 0.1
)

model_uw <- xgb.train(params = xgb_params, data = dtrain, nrounds = 200, verbose = 0)

# Also train weighted version
cat("Training XGBoost (reweighted)...\n")
dtrain_wt <- xgb.DMatrix(data = train_mat, label = train_labels, weight = sample_wt)
model_wt  <- xgb.train(params = xgb_params, data = dtrain_wt, nrounds = 200, verbose = 0)

# =========================================================================
# 4. Global feature importance (gain-based)
# =========================================================================

cat("\n============================================================\n")
cat("  4. GLOBAL FEATURE IMPORTANCE (by information gain)\n")
cat("============================================================\n\n")

imp_uw <- xgb.importance(feature_names = xgb_vars, model = model_uw)
imp_wt <- xgb.importance(feature_names = xgb_vars, model = model_wt)

cat("  --- Unweighted model (optimizes accuracy) ---\n\n")
cat(sprintf("  %-30s %8s %8s %8s\n", "Feature", "Gain", "Cover", "Freq"))
cat(paste0("  ", paste(rep("-", 58), collapse = "")), "\n")
for (i in seq_len(min(20, nrow(imp_uw)))) {
  cat(sprintf("  %-30s %7.1f%% %7.1f%% %7.1f%%\n",
              imp_uw$Feature[i],
              100 * imp_uw$Gain[i],
              100 * imp_uw$Cover[i],
              100 * imp_uw$Frequency[i]))
}

cat("\n  --- Reweighted model (optimizes rare-class detection) ---\n\n")
cat(sprintf("  %-30s %8s %8s %8s\n", "Feature", "Gain", "Cover", "Freq"))
cat(paste0("  ", paste(rep("-", 58), collapse = "")), "\n")
for (i in seq_len(min(20, nrow(imp_wt)))) {
  cat(sprintf("  %-30s %7.1f%% %7.1f%% %7.1f%%\n",
              imp_wt$Feature[i],
              100 * imp_wt$Gain[i],
              100 * imp_wt$Cover[i],
              100 * imp_wt$Frequency[i]))
}

# =========================================================================
# 5. Compare top features across both models
# =========================================================================

cat("\n============================================================\n")
cat("  5. TOP FEATURES COMPARISON\n")
cat("============================================================\n\n")

top_uw <- imp_uw$Feature[1:10]
top_wt <- imp_wt$Feature[1:10]

cat("  Rank   Unweighted (accuracy)          Reweighted (rare-class)\n")
cat(paste0("  ", paste(rep("-", 66), collapse = "")), "\n")
for (i in 1:10) {
  cat(sprintf("  %2d.    %-30s %-30s\n", i, top_uw[i], top_wt[i]))
}

both <- intersect(top_uw, top_wt)
uw_only <- setdiff(top_uw, top_wt)
wt_only <- setdiff(top_wt, top_uw)

cat(sprintf("\n  In both top-10:       %s\n", paste(both, collapse = ", ")))
if (length(uw_only) > 0)
  cat(sprintf("  Unweighted only:      %s\n", paste(uw_only, collapse = ", ")))
if (length(wt_only) > 0)
  cat(sprintf("  Reweighted only:      %s\n", paste(wt_only, collapse = ", ")))

# =========================================================================
# 6. Bottom features — candidates for removal
# =========================================================================

cat("\n============================================================\n")
cat("  6. LOWEST IMPORTANCE (candidates for removal)\n")
cat("============================================================\n\n")

cat("  --- Unweighted ---\n")
n_imp <- nrow(imp_uw)
for (i in seq(n_imp, max(1, n_imp - 9), by = -1)) {
  cat(sprintf("    %-30s  gain=%.3f%%\n", imp_uw$Feature[i], 100 * imp_uw$Gain[i]))
}

cat("\n  --- Reweighted ---\n")
n_imp_wt <- nrow(imp_wt)
for (i in seq(n_imp_wt, max(1, n_imp_wt - 9), by = -1)) {
  cat(sprintf("    %-30s  gain=%.3f%%\n", imp_wt$Feature[i], 100 * imp_wt$Gain[i]))
}

# =========================================================================
# 7. Feature importance for SACRIFICE class specifically
# =========================================================================

cat("\n============================================================\n")
cat("  7. WHAT DRIVES SACRIFICE DETECTION?\n")
cat("============================================================\n\n")

# Train a binary sacrifice detector
train$is_sacrifice <- as.integer(train$move_type %in% c("sacrifice", "sacrifice_check"))
test$is_sacrifice  <- as.integer(test$move_type %in% c("sacrifice", "sacrifice_check"))

dtrain_sac <- xgb.DMatrix(data = train_mat, label = train$is_sacrifice)
dtest_sac  <- xgb.DMatrix(data = test_mat, label = test$is_sacrifice)

# Reweight for imbalance
pos_wt <- sum(train$is_sacrifice == 0) / sum(train$is_sacrifice == 1)
sac_params <- list(
  objective      = "binary:logistic",
  eval_metric    = "auc",
  max_depth      = 6,
  learning_rate  = 0.1,
  scale_pos_weight = pos_wt
)

model_sac <- xgb.train(params = sac_params, data = dtrain_sac, nrounds = 200, verbose = 0)
imp_sac   <- xgb.importance(feature_names = xgb_vars, model = model_sac)

cat("  Binary sacrifice detector — top features by gain:\n\n")
cat(sprintf("  %-30s %8s %8s\n", "Feature", "Gain", "Freq"))
cat(paste0("  ", paste(rep("-", 50), collapse = "")), "\n")
for (i in seq_len(min(15, nrow(imp_sac)))) {
  cat(sprintf("  %-30s %7.1f%% %7.1f%%\n",
              imp_sac$Feature[i],
              100 * imp_sac$Gain[i],
              100 * imp_sac$Frequency[i]))
}

# Binary sacrifice accuracy
sac_prob <- predict(model_sac, dtest_sac)
sac_pred <- as.integer(sac_prob > 0.5)
sac_actual <- test$is_sacrifice
sac_tp <- sum(sac_pred == 1 & sac_actual == 1)
sac_fp <- sum(sac_pred == 1 & sac_actual == 0)
sac_fn <- sum(sac_pred == 0 & sac_actual == 1)
sac_precision <- sac_tp / (sac_tp + sac_fp)
sac_recall    <- sac_tp / (sac_tp + sac_fn)
sac_f1        <- 2 * sac_precision * sac_recall / (sac_precision + sac_recall)

cat(sprintf("\n  Binary sacrifice detection (threshold=0.5):\n"))
cat(sprintf("    Precision: %.1f%%  (of predicted sacrifices, how many are real)\n", 100 * sac_precision))
cat(sprintf("    Recall:    %.1f%%  (of real sacrifices, how many caught)\n", 100 * sac_recall))
cat(sprintf("    F1:        %.1f%%\n", 100 * sac_f1))

# =========================================================================
# 8. Evaluative features — did they earn their keep?
# =========================================================================

cat("\n============================================================\n")
cat("  8. EVALUATIVE FEATURES — DID THEY EARN THEIR KEEP?\n")
cat("============================================================\n\n")

eval_feats <- c("best_capture_net", "num_safe_sacrifices", "opp_recapture_after_move")
cat("  Feature importance rank across models:\n\n")
cat(sprintf("  %-30s %12s %12s %12s\n", "Feature", "Unweighted", "Reweighted", "Sacrifice"))
cat(paste0("  ", paste(rep("-", 70), collapse = "")), "\n")
for (f in eval_feats) {
  rank_uw  <- which(imp_uw$Feature == f)
  rank_wt  <- which(imp_wt$Feature == f)
  rank_sac <- which(imp_sac$Feature == f)
  cat(sprintf("  %-30s %8d/%-3d %8d/%-3d %8d/%-3d\n",
              f,
              ifelse(length(rank_uw) > 0, rank_uw, NA), nrow(imp_uw),
              ifelse(length(rank_wt) > 0, rank_wt, NA), nrow(imp_wt),
              ifelse(length(rank_sac) > 0, rank_sac, NA), nrow(imp_sac)))
}

cat("\n============================================================\n")
cat("  Feature importance complete. Next step: 03_correlation.R\n")
cat("============================================================\n")
