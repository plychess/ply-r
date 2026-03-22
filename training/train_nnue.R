# train_nnue.R — Train a style-conditioned NNUE evaluation network
#
# Architecture: 768 board inputs + 2-dim style embedding → 256 → 32 → 1 eval
#
# Input:  12 bitboards × 64 squares = 768 binary features
#         + player_id (Tal=0, Karpov=1)
# Output: evaluation in centipawns (trained to match Stockfish)
#
# The style embedding lets the same network produce different evaluations
# for the same position depending on which player's style is active.
#
# Run from project root:
#   Rscript training/train_nnue.R

library(torch)

cat("============================================================\n")
cat("  NNUE Training — Style-Conditioned Evaluation Network\n")
cat("============================================================\n\n")

# =========================================================================
# 1. Load and prepare data
# =========================================================================

cat("Loading training data...\n")
tal <- readRDS("training/data/Tal_d8.rds")
kar <- readRDS("training/data/Karpov_d8.rds")

tal$player_id <- 0L
kar$player_id <- 1L

# Combine
df <- rbind(tal[, intersect(names(tal), names(kar))],
            kar[, intersect(names(tal), names(kar))])

cat(sprintf("  Total positions: %s\n", format(nrow(df), big.mark = ",")))

# Extract 768 binary features from bitboards (via FEN)
# We have the bitboard values in the enrichment — but they're not directly
# in the data. We'll use the features we already have as input instead.
# For a true NNUE we'd want raw bitboards, but our 28 scalar features
# are a strong starting point.

feature_cols <- c(
  "material_bal", "legal_move_count",
  "num_captures_avail", "num_checks_avail",
  "max_capture_value", "num_winning_caps_avail", "in_material_deficit",
  "king_safety_own", "king_safety_opp",
  "passed_pawns", "isolated_pawns", "pin_count",
  "mob_knight", "mob_queen",
  "best_capture_net", "num_safe_sacrifices", "opp_recapture_after_move",
  "undeveloped_minors", "center_occupied", "center_attacked",
  "space_advantage", "pieces_defended", "pawn_chain_length", "pawn_islands",
  "num_pieces"
)

# Ensure all columns exist in both datasets
feature_cols <- feature_cols[feature_cols %in% names(df)]
cat(sprintf("  Features: %d\n", length(feature_cols)))
cat(sprintf("  Players:  Tal (%d), Karpov (%d)\n",
            sum(df$player_id == 0), sum(df$player_id == 1)))

# Filter out mate scores for cleaner training
df <- df[abs(df$sf_eval) < 5000, ]
cat(sprintf("  After filtering mates: %s positions\n\n", format(nrow(df), big.mark = ",")))

# Train/val split
set.seed(42)
n <- nrow(df)
train_idx <- sample(n, size = floor(0.9 * n))
train_df <- df[train_idx, ]
val_df   <- df[-train_idx, ]

# Normalize features
means <- colMeans(train_df[, feature_cols])
sds   <- apply(train_df[, feature_cols], 2, sd)
sds[sds == 0] <- 1

normalize <- function(x) {
  m <- as.matrix(x[, feature_cols])
  t((t(m) - means) / sds)
}

train_x <- normalize(train_df)
val_x   <- normalize(val_df)

# Normalize target (SF eval) to roughly [-1, 1] range
eval_mean <- mean(train_df$sf_eval)
eval_sd   <- sd(train_df$sf_eval)
train_y   <- (train_df$sf_eval - eval_mean) / eval_sd
val_y     <- (val_df$sf_eval - eval_mean) / eval_sd

train_pid <- train_df$player_id
val_pid   <- val_df$player_id

cat(sprintf("  Train: %s  Val: %s\n", format(nrow(train_df), big.mark = ","),
            format(nrow(val_df), big.mark = ",")))
cat(sprintf("  Eval normalization: mean=%.1f, sd=%.1f\n\n", eval_mean, eval_sd))

# =========================================================================
# 2. Define the network
# =========================================================================

cat("Building network...\n")

n_features <- length(feature_cols)
n_players  <- 2L
style_dim  <- 8L

nnue_net <- nn_module(
  "StyleNNUE",

  initialize = function(n_features, n_players, style_dim) {
    self$style_embed <- nn_embedding(n_players, style_dim)
    self$fc1 <- nn_linear(n_features + style_dim, 256L)
    self$fc2 <- nn_linear(256L, 64L)
    self$fc3 <- nn_linear(64L, 16L)
    self$fc4 <- nn_linear(16L, 1L)
    self$relu <- nn_relu()
    self$dropout <- nn_dropout(0.1)
  },

  forward = function(x, player_id) {
    style <- self$style_embed(player_id + 1L)  # R is 1-indexed for embedding
    combined <- torch_cat(list(x, style), dim = 2L)
    out <- self$relu(self$fc1(combined))
    out <- self$dropout(out)
    out <- self$relu(self$fc2(out))
    out <- self$dropout(out)
    out <- self$relu(self$fc3(out))
    out <- self$fc4(out)
    out$squeeze(2L)
  }
)

model <- nnue_net(n_features, n_players, style_dim)
cat(sprintf("  Parameters: %d\n",
            sum(sapply(model$parameters, function(p) p$numel()))))
cat(sprintf("  Architecture: %d + %d(style) → 256 → 64 → 16 → 1\n\n",
            n_features, style_dim))

# =========================================================================
# 3. Training loop
# =========================================================================

optimizer <- optim_adam(model$parameters, lr = 0.001)
scheduler <- lr_step(optimizer, step_size = 10L, gamma = 0.5)
loss_fn   <- nn_mse_loss()

n_epochs   <- 30L
batch_size <- 4096L

# Convert to tensors
train_x_t   <- torch_tensor(train_x, dtype = torch_float())
train_y_t   <- torch_tensor(train_y, dtype = torch_float())
train_pid_t <- torch_tensor(train_pid, dtype = torch_long())
val_x_t     <- torch_tensor(val_x, dtype = torch_float())
val_y_t     <- torch_tensor(val_y, dtype = torch_float())
val_pid_t   <- torch_tensor(val_pid, dtype = torch_long())

cat("Training...\n\n")
cat(sprintf("  %-6s %10s %10s %10s %10s\n", "Epoch", "Train Loss", "Val Loss", "Val MAE(cp)", "LR"))
cat(paste0("  ", paste(rep("-", 50), collapse = "")), "\n")

best_val_loss <- Inf
best_epoch <- 0

for (epoch in 1:n_epochs) {
  model$train()

  # Shuffle
  perm <- sample(nrow(train_x))
  n_batches <- ceiling(nrow(train_x) / batch_size)
  epoch_loss <- 0

  for (b in 1:n_batches) {
    start <- (b - 1) * batch_size + 1
    end   <- min(b * batch_size, nrow(train_x))
    idx   <- perm[start:end]

    bx   <- train_x_t[idx, ]
    by   <- train_y_t[idx]
    bpid <- train_pid_t[idx]

    optimizer$zero_grad()
    pred <- model(bx, bpid)
    loss <- loss_fn(pred, by)
    loss$backward()
    optimizer$step()

    epoch_loss <- epoch_loss + loss$item() * (end - start + 1)
  }

  epoch_loss <- epoch_loss / nrow(train_x)

  # Validation
  model$eval()
  with_no_grad({
    val_pred <- model(val_x_t, val_pid_t)
    val_loss <- loss_fn(val_pred, val_y_t)$item()
    # MAE in centipawns
    val_mae_cp <- mean(abs(as.numeric(val_pred) * eval_sd + eval_mean - val_df$sf_eval))
  })

  scheduler$step()
  lr <- optimizer$param_groups[[1]]$lr

  cat(sprintf("  %-6d %10.6f %10.6f %10.1f %10.6f\n",
              epoch, epoch_loss, val_loss, val_mae_cp, lr))

  if (val_loss < best_val_loss) {
    best_val_loss <- val_loss
    best_epoch <- epoch
    torch_save(model, "training/model_nnue.pt")
  }
}

cat(sprintf("\n  Best epoch: %d (val_loss=%.6f)\n", best_epoch, best_val_loss))

# =========================================================================
# 4. Evaluate results
# =========================================================================

cat("\n============================================================\n")
cat("  4. EVALUATION\n")
cat("============================================================\n\n")

model <- torch_load("training/model_nnue.pt")
model$eval()

with_no_grad({
  val_pred_t <- model(val_x_t, val_pid_t)
  val_pred_cp <- as.numeric(val_pred_t) * eval_sd + eval_mean
})

val_actual_cp <- val_df$sf_eval

# Overall
mae <- mean(abs(val_pred_cp - val_actual_cp))
rmse <- sqrt(mean((val_pred_cp - val_actual_cp)^2))
corr <- cor(val_pred_cp, val_actual_cp)
cat(sprintf("  Overall MAE:  %.1f cp\n", mae))
cat(sprintf("  Overall RMSE: %.1f cp\n", rmse))
cat(sprintf("  Correlation:  %.4f\n", corr))

# Per player
for (pid in 0:1) {
  pname <- if (pid == 0) "Tal" else "Karpov"
  idx <- val_df$player_id == pid
  mae_p <- mean(abs(val_pred_cp[idx] - val_actual_cp[idx]))
  corr_p <- cor(val_pred_cp[idx], val_actual_cp[idx])
  cat(sprintf("  %-8s MAE: %.1f cp  corr: %.4f\n", pname, mae_p, corr_p))
}

# Compare with classic eval
classic_corr <- cor(val_df$position_eval, val_actual_cp)
cat(sprintf("\n  Classic eval correlation: %.4f\n", classic_corr))
cat(sprintf("  NNUE correlation:        %.4f\n", corr))
cat(sprintf("  Improvement:             %.1fx\n", corr / max(abs(classic_corr), 0.001)))

# =========================================================================
# 5. Style embedding analysis
# =========================================================================

cat("\n============================================================\n")
cat("  5. STYLE EMBEDDINGS\n")
cat("============================================================\n\n")

with_no_grad({
  tal_embed <- as.numeric(model$style_embed(torch_tensor(1L, dtype = torch_long())))
  kar_embed <- as.numeric(model$style_embed(torch_tensor(2L, dtype = torch_long())))
})

cat("  Tal embedding:    [", paste(sprintf("%+.3f", tal_embed), collapse = ", "), "]\n")
cat("  Karpov embedding: [", paste(sprintf("%+.3f", kar_embed), collapse = ", "), "]\n")
cat(sprintf("  Euclidean distance: %.3f\n", sqrt(sum((tal_embed - kar_embed)^2))))
cat(sprintf("  Cosine similarity:  %.3f\n",
            sum(tal_embed * kar_embed) / (sqrt(sum(tal_embed^2)) * sqrt(sum(kar_embed^2)))))

# =========================================================================
# 6. Same position, different style
# =========================================================================

cat("\n============================================================\n")
cat("  6. SAME POSITION, DIFFERENT STYLE EVAL\n")
cat("============================================================\n\n")

# Take 10 random positions and evaluate with both styles
set.seed(123)
sample_idx <- sample(nrow(val_x), 10)
sample_x <- val_x_t[sample_idx, ]

with_no_grad({
  tal_evals <- as.numeric(model(sample_x, torch_tensor(rep(0L, 10), dtype = torch_long()))) * eval_sd + eval_mean
  kar_evals <- as.numeric(model(sample_x, torch_tensor(rep(1L, 10), dtype = torch_long()))) * eval_sd + eval_mean
  sf_evals  <- val_df$sf_eval[sample_idx]
})

cat(sprintf("  %-4s %10s %10s %10s %10s\n", "#", "SF Eval", "Tal-mode", "Karpov-mode", "Diff"))
cat(paste0("  ", paste(rep("-", 48), collapse = "")), "\n")
for (i in 1:10) {
  cat(sprintf("  %-4d %+9.0f %+9.0f %+9.0f %+9.0f\n",
              i, sf_evals[i], tal_evals[i], kar_evals[i], tal_evals[i] - kar_evals[i]))
}

cat(sprintf("\n  Mean |Tal - Karpov| eval difference: %.1f cp\n",
            mean(abs(tal_evals - kar_evals))))

# =========================================================================
# 7. Save normalization params for C++ inference
# =========================================================================

cat("\n============================================================\n")
cat("  7. EXPORT FOR C++ INFERENCE\n")
cat("============================================================\n\n")

saveRDS(list(
  feature_cols = feature_cols,
  means = means,
  sds = sds,
  eval_mean = eval_mean,
  eval_sd = eval_sd,
  n_players = n_players,
  style_dim = style_dim
), "training/nnue_params.rds")

cat("  Saved: training/model_nnue.pt (PyTorch model)\n")
cat("  Saved: training/nnue_params.rds (normalization params)\n")

cat("\n============================================================\n")
cat("  NNUE training complete.\n")
cat("============================================================\n")
