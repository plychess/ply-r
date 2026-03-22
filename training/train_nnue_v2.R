# train_nnue_v2.R — NNUE v2: raw bitboard inputs (768 binary features)
#
# Architecture: 768 board bits + 2-dim style → 512 → 32 → 1
# No feature extraction needed — just memcpy bitboards into the network.
#
# Run from project root:
#   Rscript training/train_nnue_v2.R

library(torch)

cat("============================================================\n")
cat("  NNUE v2 — Raw Bitboard Inputs (768 binary features)\n")
cat("============================================================\n\n")

# =========================================================================
# 1. Load data and extract bitboard features
# =========================================================================

cat("Loading training data...\n")
tal <- readRDS("training/data/Tal_d8.rds")
kar <- readRDS("training/data/Karpov_d8.rds")

tal$player_id <- 0L
kar$player_id <- 1L

# Use common columns
common <- intersect(names(tal), names(kar))
df <- rbind(tal[, common], kar[, common])

# Filter mate scores
df <- df[abs(df$sf_eval) < 5000, ]
cat(sprintf("  Positions: %s\n", format(nrow(df), big.mark = ",")))

# Extract 768 binary features from FEN via C++ bitboards
library(ply)
ns <- getNamespace("ply")
for (fn in ls(ns, pattern = "^cpp_")) assign(fn, get(fn, envir = ns))

cat("Extracting bitboard features from FEN...\n")

# Parse each FEN and extract 12 bitboards × 64 bits = 768 binary features
# We'll do this in batches via cpp_parse_fen
extract_bitboard_matrix <- function(fens, side_to_move) {
  n <- length(fens)
  mat <- matrix(0L, nrow = n, ncol = 768L)

  for (i in seq_len(n)) {
    state <- cpp_parse_fen(fens[i])
    # state$bitboards is a character vector of 12 hex strings (16 chars each = 64 bits)
    for (bb_idx in 1:12) {
      hex <- state$bitboards[bb_idx]
      # Convert hex string to 64-bit integer bits
      val <- 0
      for (ch_idx in 1:16) {
        nibble <- strtoi(substr(hex, ch_idx, ch_idx), 16L)
        bit_offset <- (16L - ch_idx) * 4L
        for (bit in 0:3) {
          if (bitwAnd(nibble, bitwShiftL(1L, bit)) > 0L) {
            sq <- bit_offset + bit
            col_idx <- (bb_idx - 1L) * 64L + sq + 1L
            mat[i, col_idx] <- 1L
          }
        }
      }
    }

    if (i %% 10000 == 0) cat(sprintf("\r  %d / %d", i, n))
  }
  cat(sprintf("\r  %d / %d done\n", n, n))
  mat
}

board_mat <- extract_bitboard_matrix(df$fen, df$side_to_move)

# Add side-to-move as feature 769
stm <- as.integer(grepl(" b ", df$fen))
board_mat <- cbind(board_mat, stm)

cat(sprintf("  Board matrix: %d × %d\n", nrow(board_mat), ncol(board_mat)))

# =========================================================================
# 2. Prepare tensors
# =========================================================================

set.seed(42)
n <- nrow(df)
train_idx <- sample(n, floor(0.9 * n))

train_x <- board_mat[train_idx, ]
val_x   <- board_mat[-train_idx, ]
train_pid <- df$player_id[train_idx]
val_pid   <- df$player_id[-train_idx]

eval_mean <- mean(df$sf_eval[train_idx])
eval_sd   <- sd(df$sf_eval[train_idx])
train_y <- (df$sf_eval[train_idx] - eval_mean) / eval_sd
val_y   <- (df$sf_eval[-train_idx] - eval_mean) / eval_sd
val_sf  <- df$sf_eval[-train_idx]

train_x_t   <- torch_tensor(train_x, dtype = torch_float())
val_x_t     <- torch_tensor(val_x, dtype = torch_float())
train_y_t   <- torch_tensor(train_y, dtype = torch_float())
val_y_t     <- torch_tensor(val_y, dtype = torch_float())
train_pid_t <- torch_tensor(train_pid, dtype = torch_long())
val_pid_t   <- torch_tensor(val_pid, dtype = torch_long())

cat(sprintf("  Train: %s  Val: %s\n", format(length(train_y), big.mark = ","),
            format(length(val_y), big.mark = ",")))
cat(sprintf("  Eval norm: mean=%.1f, sd=%.1f\n\n", eval_mean, eval_sd))

# =========================================================================
# 3. Define network
# =========================================================================

n_input   <- 769L  # 768 board + 1 side-to-move
n_players <- 2L
style_dim <- 8L

nnue_v2 <- nn_module(
  "NNUE_v2",

  initialize = function() {
    self$style_embed <- nn_embedding(n_players, style_dim)
    self$fc1 <- nn_linear(n_input + style_dim, 512L)
    self$fc2 <- nn_linear(512L, 32L)
    self$fc3 <- nn_linear(32L, 1L)
    self$relu <- nn_relu()
  },

  forward = function(x, pid) {
    style <- self$style_embed(pid + 1L)
    h <- torch_cat(list(x, style), dim = 2L)
    h <- self$relu(self$fc1(h))
    h <- self$relu(self$fc2(h))
    self$fc3(h)$squeeze(2L)
  }
)

model <- nnue_v2()
n_params <- sum(sapply(model$parameters, function(p) p$numel()))
cat(sprintf("Network: %d + %d(style) → 512 → 32 → 1  (%s params)\n\n",
            n_input, style_dim, format(n_params, big.mark = ",")))

# =========================================================================
# 4. Train
# =========================================================================

optimizer <- optim_adam(model$parameters, lr = 0.001)
scheduler <- lr_step(optimizer, step_size = 8L, gamma = 0.5)
loss_fn   <- nn_mse_loss()

n_epochs   <- 25L
batch_size <- 4096L

cat(sprintf("%-6s %10s %10s %10s %8s\n", "Epoch", "Train", "Val Loss", "Val MAE", "LR"))
cat(paste0(paste(rep("-", 48), collapse = ""), "\n"))

best_val <- Inf

for (epoch in 1:n_epochs) {
  model$train()
  perm <- sample(nrow(train_x))
  n_batches <- ceiling(nrow(train_x) / batch_size)
  ep_loss <- 0

  for (b in 1:n_batches) {
    s <- (b-1) * batch_size + 1
    e <- min(b * batch_size, nrow(train_x))
    idx <- perm[s:e]

    optimizer$zero_grad()
    pred <- model(train_x_t[idx,], train_pid_t[idx])
    loss <- loss_fn(pred, train_y_t[idx])
    loss$backward()
    optimizer$step()
    ep_loss <- ep_loss + loss$item() * (e - s + 1)
  }
  ep_loss <- ep_loss / nrow(train_x)

  model$eval()
  with_no_grad({
    vp <- model(val_x_t, val_pid_t)
    vl <- loss_fn(vp, val_y_t)$item()
    vp_cp <- as.numeric(vp) * eval_sd + eval_mean
    mae <- mean(abs(vp_cp - val_sf))
  })

  scheduler$step()
  lr <- optimizer$param_groups[[1]]$lr
  cat(sprintf("%-6d %10.6f %10.6f %10.1f %8.6f\n", epoch, ep_loss, vl, mae, lr))

  if (vl < best_val) {
    best_val <- vl
    torch_save(model, "training/model_nnue_v2.pt")
  }
}

# =========================================================================
# 5. Evaluate
# =========================================================================

cat("\n============================================================\n")
cat("  EVALUATION\n")
cat("============================================================\n\n")

model <- torch_load("training/model_nnue_v2.pt")
model$eval()

with_no_grad({
  vp <- model(val_x_t, val_pid_t)
  vp_cp <- as.numeric(vp) * eval_sd + eval_mean
})

mae  <- mean(abs(vp_cp - val_sf))
corr <- cor(vp_cp, val_sf)
cat(sprintf("  MAE:         %.1f cp\n", mae))
cat(sprintf("  Correlation: %.4f\n", corr))

for (pid in 0:1) {
  nm <- if (pid == 0) "Tal" else "Karpov"
  idx <- val_pid == pid
  cat(sprintf("  %-8s MAE: %.1f cp  corr: %.4f\n", nm,
              mean(abs(vp_cp[idx] - val_sf[idx])), cor(vp_cp[idx], val_sf[idx])))
}

# Style difference
with_no_grad({
  sample_idx <- sample(nrow(val_x), 100)
  sx <- val_x_t[sample_idx,]
  t_eval <- as.numeric(model(sx, torch_tensor(rep(0L,100), dtype=torch_long()))) * eval_sd + eval_mean
  k_eval <- as.numeric(model(sx, torch_tensor(rep(1L,100), dtype=torch_long()))) * eval_sd + eval_mean
})
cat(sprintf("\n  Mean |Tal - Karpov| style diff: %.1f cp\n", mean(abs(t_eval - k_eval))))

# =========================================================================
# 6. Export weights for C++
# =========================================================================

cat("\n  Exporting weights...\n")

fc1_w <- as.numeric(model$fc1$weight)
fc1_b <- as.numeric(model$fc1$bias)
fc2_w <- as.numeric(model$fc2$weight)
fc2_b <- as.numeric(model$fc2$bias)
fc3_w <- as.numeric(model$fc3$weight)
fc3_b <- as.numeric(model$fc3$bias)
style_w <- as.numeric(model$style_embed$weight)

out <- file("training/nnue_v2_weights.bin", "wb")
writeBin(as.integer(0x4E4E5532), out, size = 4)  # "NNU2" magic
writeBin(as.integer(n_input), out, size = 4)
writeBin(as.integer(n_players), out, size = 4)
writeBin(as.integer(style_dim), out, size = 4)
writeBin(as.numeric(fc1_w), out, size = 4)
writeBin(as.numeric(fc1_b), out, size = 4)
writeBin(as.numeric(fc2_w), out, size = 4)
writeBin(as.numeric(fc2_b), out, size = 4)
writeBin(as.numeric(fc3_w), out, size = 4)
writeBin(as.numeric(fc3_b), out, size = 4)
writeBin(as.numeric(style_w), out, size = 4)
writeBin(as.numeric(eval_mean), out, size = 4)
writeBin(as.numeric(eval_sd), out, size = 4)
close(out)

cat(sprintf("  Saved: training/nnue_v2_weights.bin (%.1f KB)\n",
            file.size("training/nnue_v2_weights.bin") / 1024))
cat(sprintf("  Params: %s\n", format(n_params, big.mark = ",")))

cat("\n============================================================\n")
cat("  NNUE v2 training complete.\n")
cat("============================================================\n")
