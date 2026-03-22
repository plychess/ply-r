# export_weights.R — Export NNUE weights to flat binary for C++ inference
#
# Outputs: training/nnue_weights.bin
# Format: all weights/biases as consecutive float32 arrays
#   Layer 1: weights[256 * 32] + bias[256]
#   Layer 2: weights[64 * 256] + bias[64]
#   Layer 3: weights[16 * 64] + bias[16]
#   Layer 4: weights[1 * 16] + bias[1]
#   Style embed: weights[2 * 8]
#   Normalization: means[24] + sds[24] + eval_mean[1] + eval_sd[1]

library(torch)

cat("Exporting NNUE weights to binary...\n")

model <- torch_load("training/model_nnue.pt")
model$eval()
params <- readRDS("training/nnue_params.rds")

# Extract all parameters
fc1_w <- as.numeric(model$fc1$weight)  # [256, 32] (out × in)
fc1_b <- as.numeric(model$fc1$bias)    # [256]
fc2_w <- as.numeric(model$fc2$weight)  # [64, 256]
fc2_b <- as.numeric(model$fc2$bias)    # [64]
fc3_w <- as.numeric(model$fc3$weight)  # [16, 64]
fc3_b <- as.numeric(model$fc3$bias)    # [16]
fc4_w <- as.numeric(model$fc4$weight)  # [1, 16]
fc4_b <- as.numeric(model$fc4$bias)    # [1]
style_w <- as.numeric(model$style_embed$weight) # [2, 8]

cat(sprintf("  fc1: %d weights + %d bias\n", length(fc1_w), length(fc1_b)))
cat(sprintf("  fc2: %d weights + %d bias\n", length(fc2_w), length(fc2_b)))
cat(sprintf("  fc3: %d weights + %d bias\n", length(fc3_w), length(fc3_b)))
cat(sprintf("  fc4: %d weights + %d bias\n", length(fc4_w), length(fc4_b)))
cat(sprintf("  style: %d weights\n", length(style_w)))
cat(sprintf("  norm: %d means + %d sds + 2 eval params\n",
            length(params$means), length(params$sds)))

# Write as float32 binary
out <- file("training/nnue_weights.bin", "wb")

# Header: magic + dimensions
writeBin(as.integer(0x4E4E5545), out, size = 4)  # "NNUE" magic
writeBin(as.integer(length(params$feature_cols)), out, size = 4)  # n_features
writeBin(as.integer(params$n_players), out, size = 4)  # n_players
writeBin(as.integer(params$style_dim), out, size = 4)  # style_dim

# Layer weights and biases (as float32)
writeBin(as.numeric(fc1_w), out, size = 4)
writeBin(as.numeric(fc1_b), out, size = 4)
writeBin(as.numeric(fc2_w), out, size = 4)
writeBin(as.numeric(fc2_b), out, size = 4)
writeBin(as.numeric(fc3_w), out, size = 4)
writeBin(as.numeric(fc3_b), out, size = 4)
writeBin(as.numeric(fc4_w), out, size = 4)
writeBin(as.numeric(fc4_b), out, size = 4)

# Style embeddings
writeBin(as.numeric(style_w), out, size = 4)

# Normalization
writeBin(as.numeric(params$means), out, size = 4)
writeBin(as.numeric(params$sds), out, size = 4)
writeBin(as.numeric(params$eval_mean), out, size = 4)
writeBin(as.numeric(params$eval_sd), out, size = 4)

close(out)

fsize <- file.size("training/nnue_weights.bin")
cat(sprintf("\n  Exported: training/nnue_weights.bin (%.1f KB)\n", fsize / 1024))
cat(sprintf("  Total params: %d\n",
            length(fc1_w) + length(fc1_b) + length(fc2_w) + length(fc2_b) +
            length(fc3_w) + length(fc3_b) + length(fc4_w) + length(fc4_b) +
            length(style_w)))
