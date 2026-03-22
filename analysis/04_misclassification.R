# 04_misclassification.R — Misclassification deep dive
#
# Step 4 of incremental analysis. Goal: what do the errors look like?
# Which sacrifices do we miss? Which quiet moves get mislabeled?
# This tells us exactly where improvement is still possible.
#
# Run from project root:
#   Rscript analysis/04_misclassification.R

library(xgboost)
source("analysis/00_load_data.R")

cat("============================================================\n")
cat("  Tal Case Study — Step 4: Misclassification Deep Dive\n")
cat("============================================================\n\n")

# =========================================================================
# 1. Train XGBoost and get predictions
# =========================================================================

set.seed(42)
train_idx <- sample(nrow(df), size = floor(0.8 * nrow(df)))
train <- df[train_idx, ]
test  <- df[-train_idx, ]

pred_vars <- c(
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

train_mat <- as.matrix(train[, pred_vars])
test_mat  <- as.matrix(test[, pred_vars])
class_levels <- levels(train$move_type)
train_labels <- as.integer(train$move_type) - 1L
test_labels  <- as.integer(test$move_type) - 1L

dtrain <- xgb.DMatrix(data = train_mat, label = train_labels)
dtest  <- xgb.DMatrix(data = test_mat, label = test_labels)

xgb_params <- list(
  objective     = "multi:softmax",
  num_class     = length(class_levels),
  eval_metric   = "mlogloss",
  max_depth     = 6,
  learning_rate = 0.1
)

cat("Training XGBoost (unweighted)...\n")
model <- xgb.train(params = xgb_params, data = dtrain, nrounds = 200, verbose = 0)
pred_int <- predict(model, dtest)
test$predicted <- factor(class_levels[pred_int + 1L], levels = class_levels)
test$correct   <- test$predicted == test$move_type

cat(sprintf("Overall accuracy: %.1f%%\n\n", 100 * mean(test$correct)))

# =========================================================================
# 2. Confusion matrix
# =========================================================================

cat("============================================================\n")
cat("  2. CONFUSION MATRIX\n")
cat("============================================================\n\n")

conf <- table(Actual = test$move_type, Predicted = test$predicted)
cat(sprintf("%-22s", "Actual \\ Predicted"))
for (cls in class_levels) cat(sprintf(" %8s", substr(cls, 1, 8)))
cat("  | Sens.\n")
cat(paste0(paste(rep("-", 22 + length(class_levels) * 9 + 10), collapse = ""), "\n"))

for (actual_cls in class_levels) {
  cat(sprintf("%-22s", actual_cls))
  row_total <- sum(conf[actual_cls, ])
  for (pred_cls in class_levels) {
    val <- conf[actual_cls, pred_cls]
    if (val > 0) {
      cat(sprintf(" %8d", val))
    } else {
      cat(sprintf(" %8s", "."))
    }
  }
  sens <- conf[actual_cls, actual_cls] / row_total
  cat(sprintf("  | %5.1f%%\n", 100 * sens))
}

cat(paste0(paste(rep("-", 22 + length(class_levels) * 9 + 10), collapse = ""), "\n"))
cat(sprintf("%-22s", "Precision"))
for (pred_cls in class_levels) {
  col_total <- sum(conf[, pred_cls])
  if (col_total > 0) {
    prec <- conf[pred_cls, pred_cls] / col_total
    cat(sprintf(" %7.1f%%", 100 * prec))
  } else {
    cat(sprintf(" %8s", "-"))
  }
}
cat("\n")

# =========================================================================
# 3. Where do errors go? (misclassification flow)
# =========================================================================

cat("\n============================================================\n")
cat("  3. MISCLASSIFICATION FLOW (where do errors go?)\n")
cat("============================================================\n\n")

for (actual_cls in class_levels) {
  wrong <- test[test$move_type == actual_cls & !test$correct, ]
  if (nrow(wrong) == 0) next
  n_actual <- sum(test$move_type == actual_cls)
  cat(sprintf("  %s — %d/%d misclassified (%.1f%% error rate)\n",
              actual_cls, nrow(wrong), n_actual, 100 * nrow(wrong) / n_actual))
  flow <- sort(table(wrong$predicted), decreasing = TRUE)
  for (dest in names(flow)) {
    cat(sprintf("    → %-22s %4d  (%.1f%% of errors)\n",
                dest, flow[dest], 100 * flow[dest] / nrow(wrong)))
  }
  cat("\n")
}

# =========================================================================
# 4. Feature comparison: correct vs missed (per class)
# =========================================================================

cat("============================================================\n")
cat("  4. FEATURE COMPARISON: CORRECT vs MISSED\n")
cat("============================================================\n\n")

key_feats <- c("eval_gap", "num_safe_sacrifices", "chosen_move_eval",
               "num_captures_avail", "num_checks_avail", "king_safety_opp",
               "material_bal", "legal_move_count", "best_sacrifice_eval",
               "opp_recapture_after_move")

for (cls in c("sacrifice", "check", "trade", "winning_capture")) {
  actual <- test[test$move_type == cls, ]
  caught <- actual[actual$correct, ]
  missed <- actual[!actual$correct, ]
  if (nrow(missed) == 0 || nrow(caught) == 0) next

  cat(sprintf("  --- %s: %d caught, %d missed ---\n", cls, nrow(caught), nrow(missed)))
  cat(sprintf("  %-28s %10s %10s %10s\n", "Feature", "Caught", "Missed", "Delta"))
  cat(paste0("  ", paste(rep("-", 62), collapse = "")), "\n")

  for (f in key_feats) {
    m_caught <- mean(caught[[f]], na.rm = TRUE)
    m_missed <- mean(missed[[f]], na.rm = TRUE)
    delta <- m_caught - m_missed
    cat(sprintf("  %-28s %10.2f %10.2f %+9.2f\n", f, m_caught, m_missed, delta))
  }
  cat("\n")
}

# =========================================================================
# 5. Game phase breakdown of errors
# =========================================================================

cat("============================================================\n")
cat("  5. ERROR RATE BY GAME PHASE\n")
cat("============================================================\n\n")

cat(sprintf("  %-22s %10s %10s %10s\n", "Class", "Opening", "Middle", "Endgame"))
cat(paste0("  ", paste(rep("-", 56), collapse = "")), "\n")

for (cls in class_levels) {
  rates <- sapply(c("opening", "middlegame", "endgame"), function(ph) {
    sub <- test[test$move_type == cls & test$phase == ph, ]
    if (nrow(sub) == 0) return(NA)
    100 * (1 - mean(sub$correct))
  })
  cat(sprintf("  %-22s %9.1f%% %9.1f%% %9.1f%%\n",
              cls, rates[1], rates[2], rates[3]))
}

# =========================================================================
# 6. Wins vs losses — do errors cluster in losses?
# =========================================================================

cat("\n============================================================\n")
cat("  6. ERROR RATE BY GAME OUTCOME\n")
cat("============================================================\n\n")

cat(sprintf("  %-22s %10s %10s %10s\n", "Class", "Wins", "Losses", "Draws"))
cat(paste0("  ", paste(rep("-", 56), collapse = "")), "\n")

for (cls in class_levels) {
  rates <- sapply(c("tal_won", "tal_lost", "tal_drew"), function(outcome) {
    sub <- test[test$move_type == cls & test[[outcome]], ]
    if (nrow(sub) == 0) return(NA)
    100 * (1 - mean(sub$correct))
  })
  cat(sprintf("  %-22s %9.1f%% %9.1f%% %9.1f%%\n",
              cls, rates[1], rates[2], rates[3]))
}

# =========================================================================
# 7. Hardest positions — highest confidence wrong predictions
# =========================================================================

cat("\n============================================================\n")
cat("  7. MOST CONFIDENT WRONG PREDICTIONS\n")
cat("============================================================\n\n")

# Get probabilities
xgb_params_prob <- xgb_params
xgb_params_prob$objective <- "multi:softprob"
model_prob <- xgb.train(params = xgb_params_prob, data = dtrain, nrounds = 200, verbose = 0)
probs <- predict(model_prob, dtest)
prob_mat <- matrix(probs, ncol = length(class_levels), byrow = TRUE)

test$pred_confidence <- apply(prob_mat, 1, max)
wrong <- test[!test$correct, ]
wrong <- wrong[order(-wrong$pred_confidence), ]

cat("  Top 10 most confident errors:\n\n")
cat(sprintf("  %-6s %-18s %-18s %6s %6s  %-8s  %s\n",
            "Game", "Actual", "Predicted", "Conf", "Eval", "Phase", "FEN (first 40)"))
cat(paste0("  ", paste(rep("-", 95), collapse = "")), "\n")

for (i in seq_len(min(10, nrow(wrong)))) {
  row <- wrong[i, ]
  cat(sprintf("  %-6d %-18s %-18s %5.1f%% %+5d  %-8s  %s\n",
              row$game_id, as.character(row$move_type), as.character(row$predicted),
              100 * row$pred_confidence, row$material_bal,
              row$phase, substr(row$fen, 1, 40)))
}

# =========================================================================
# 8. Summary
# =========================================================================

cat("\n============================================================\n")
cat("  8. SUMMARY\n")
cat("============================================================\n\n")

n_wrong <- sum(!test$correct)
n_total <- nrow(test)
cat(sprintf("  Total errors:     %d / %d (%.1f%%)\n", n_wrong, n_total, 100 * n_wrong / n_total))

for (cls in class_levels) {
  actual <- test[test$move_type == cls, ]
  missed <- actual[!actual$correct, ]
  if (nrow(actual) == 0) next
  cat(sprintf("  %-22s %4d errors / %5d  (%.1f%% error rate)\n",
              cls, nrow(missed), nrow(actual), 100 * nrow(missed) / nrow(actual)))
}

# Key insight
cat("\n  Key patterns:\n")
sac_missed <- test[test$move_type == "sacrifice" & !test$correct, ]
sac_caught <- test[test$move_type == "sacrifice" & test$correct, ]
if (nrow(sac_missed) > 0 && nrow(sac_caught) > 0) {
  cat(sprintf("  - Missed sacrifices have %.1f fewer safe sacrifices available (%.2f vs %.2f)\n",
              mean(sac_caught$num_safe_sacrifices) - mean(sac_missed$num_safe_sacrifices),
              mean(sac_caught$num_safe_sacrifices), mean(sac_missed$num_safe_sacrifices)))
  cat(sprintf("  - Missed sacrifices have smaller eval_gap (%.0f vs %.0f cp)\n",
              mean(sac_missed$eval_gap), mean(sac_caught$eval_gap)))
}

chk_missed <- test[test$move_type == "check" & !test$correct, ]
chk_caught <- test[test$move_type == "check" & test$correct, ]
if (nrow(chk_missed) > 0 && nrow(chk_caught) > 0) {
  cat(sprintf("  - Missed checks have %.1f fewer checks available (%.2f vs %.2f)\n",
              mean(chk_caught$num_checks_avail) - mean(chk_missed$num_checks_avail),
              mean(chk_caught$num_checks_avail), mean(chk_missed$num_checks_avail)))
  cat(sprintf("  - Missed checks have lower king_safety_opp (%.2f vs %.2f)\n",
              mean(chk_missed$king_safety_opp), mean(chk_caught$king_safety_opp)))
}

cat("\n============================================================\n")
cat("  Misclassification analysis complete.\n")
cat("  Next step: 05_sequence_features.R\n")
cat("============================================================\n")
