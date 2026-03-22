# 03_correlation.R — Feature correlation analysis
#
# Step 3 of incremental analysis. Goal: which features are redundant?
# Are we wasting model capacity on features that say the same thing?
#
# Run from project root:
#   Rscript analysis/03_correlation.R

source("analysis/00_load_data.R")

cat("============================================================\n")
cat("  Tal Case Study — Step 3: Feature Correlation Analysis\n")
cat("============================================================\n\n")

# =========================================================================
# 1. Select numeric features for correlation
# =========================================================================

features <- c(
  "num_pieces", "material_bal", "legal_move_count", "ply",
  "num_captures_avail", "num_checks_avail",
  "max_capture_value", "num_sacrifices_avail", "num_winning_caps_avail",
  "max_sacrifice_gap", "in_material_deficit",
  "king_safety_own", "king_safety_opp",
  "passed_pawns", "isolated_pawns", "pin_count",
  "mob_knight", "mob_queen",
  "best_capture_net", "num_safe_sacrifices", "opp_recapture_after_move",
  "undeveloped_minors", "center_occupied", "center_attacked",
  "space_advantage", "pieces_defended", "pawn_chain_length", "pawn_islands",
  "position_eval", "chosen_move_eval", "best_move_eval", "eval_gap",
  "best_quiet_eval", "best_capture_eval", "worst_capture_eval",
  "capture_eval_spread", "best_sacrifice_eval", "num_moves_better_than_chosen"
)

mat <- as.matrix(df[, features])
cor_mat <- cor(mat, use = "complete.obs")

# =========================================================================
# 2. Highly correlated pairs (|r| > 0.7)
# =========================================================================

cat("============================================================\n")
cat("  2. HIGHLY CORRELATED PAIRS (|r| > 0.7)\n")
cat("============================================================\n\n")

pairs <- data.frame(feat1 = character(), feat2 = character(), r = numeric(),
                    stringsAsFactors = FALSE)
for (i in 1:(ncol(cor_mat) - 1)) {
  for (j in (i + 1):ncol(cor_mat)) {
    r <- cor_mat[i, j]
    if (abs(r) > 0.7) {
      pairs <- rbind(pairs, data.frame(
        feat1 = colnames(cor_mat)[i],
        feat2 = colnames(cor_mat)[j],
        r = round(r, 3),
        stringsAsFactors = FALSE
      ))
    }
  }
}

pairs <- pairs[order(-abs(pairs$r)), ]
cat(sprintf("  Found %d pairs with |r| > 0.7:\n\n", nrow(pairs)))
cat(sprintf("  %-30s %-30s %8s\n", "Feature 1", "Feature 2", "r"))
cat(paste0("  ", paste(rep("-", 72), collapse = "")), "\n")
for (i in seq_len(nrow(pairs))) {
  cat(sprintf("  %-30s %-30s %+7.3f\n", pairs$feat1[i], pairs$feat2[i], pairs$r[i]))
}

# =========================================================================
# 3. Moderate correlations (0.5 < |r| < 0.7)
# =========================================================================

cat("\n============================================================\n")
cat("  3. MODERATE CORRELATIONS (0.5 < |r| < 0.7)\n")
cat("============================================================\n\n")

mod_pairs <- data.frame(feat1 = character(), feat2 = character(), r = numeric(),
                        stringsAsFactors = FALSE)
for (i in 1:(ncol(cor_mat) - 1)) {
  for (j in (i + 1):ncol(cor_mat)) {
    r <- cor_mat[i, j]
    if (abs(r) > 0.5 && abs(r) <= 0.7) {
      mod_pairs <- rbind(mod_pairs, data.frame(
        feat1 = colnames(cor_mat)[i],
        feat2 = colnames(cor_mat)[j],
        r = round(r, 3),
        stringsAsFactors = FALSE
      ))
    }
  }
}

mod_pairs <- mod_pairs[order(-abs(mod_pairs$r)), ]
cat(sprintf("  Found %d pairs with 0.5 < |r| < 0.7:\n\n", nrow(mod_pairs)))
cat(sprintf("  %-30s %-30s %8s\n", "Feature 1", "Feature 2", "r"))
cat(paste0("  ", paste(rep("-", 72), collapse = "")), "\n")
for (i in seq_len(nrow(mod_pairs))) {
  cat(sprintf("  %-30s %-30s %+7.3f\n",
              mod_pairs$feat1[i], mod_pairs$feat2[i], mod_pairs$r[i]))
}

# =========================================================================
# 4. Features most correlated with move type
# =========================================================================

cat("\n============================================================\n")
cat("  4. CORRELATION WITH MOVE TYPE (point-biserial per class)\n")
cat("============================================================\n\n")

for (cls in levels(df$move_type)) {
  is_cls <- as.integer(df$move_type == cls)
  cors <- sapply(features, function(f) cor(df[[f]], is_cls, use = "complete.obs"))
  cors <- sort(cors, decreasing = TRUE)

  cat(sprintf("  --- %s (n=%s) ---\n", cls, format(sum(is_cls), big.mark = ",")))
  # Top 5 positive and top 5 negative
  top_pos <- head(cors[cors > 0], 5)
  top_neg <- tail(cors[cors < 0], 5)
  for (nm in names(top_pos)) {
    cat(sprintf("    %+.3f  %s\n", top_pos[nm], nm))
  }
  for (nm in rev(names(top_neg))) {
    cat(sprintf("    %+.3f  %s\n", top_neg[nm], nm))
  }
  cat("\n")
}

# =========================================================================
# 5. Redundancy clusters — groups of features saying the same thing
# =========================================================================

cat("============================================================\n")
cat("  5. REDUNDANCY CLUSTERS\n")
cat("============================================================\n\n")

# Simple clustering: if A-B corr > 0.7 and B-C corr > 0.7, they're in a cluster
visited <- rep(FALSE, length(features))
names(visited) <- features
clusters <- list()

for (f in features) {
  if (visited[f]) next
  cluster <- f
  visited[f] <- TRUE
  # Find all features correlated > 0.7 with any member
  repeat {
    new_members <- character()
    for (member in cluster) {
      for (other in features) {
        if (visited[other]) next
        if (abs(cor_mat[member, other]) > 0.7) {
          new_members <- c(new_members, other)
          visited[other] <- TRUE
        }
      }
    }
    if (length(new_members) == 0) break
    cluster <- c(cluster, new_members)
  }
  if (length(cluster) > 1) {
    clusters[[length(clusters) + 1]] <- cluster
  }
}

if (length(clusters) == 0) {
  cat("  No clusters found (no features correlated > 0.7 transitively)\n")
} else {
  for (i in seq_along(clusters)) {
    cat(sprintf("  Cluster %d: %s\n", i, paste(clusters[[i]], collapse = ", ")))
    # Show within-cluster correlations
    if (length(clusters[[i]]) <= 6) {
      sub_cor <- cor_mat[clusters[[i]], clusters[[i]]]
      for (a in 1:(nrow(sub_cor) - 1)) {
        for (b in (a + 1):ncol(sub_cor)) {
          cat(sprintf("    %s ~ %s: r=%+.3f\n",
                      rownames(sub_cor)[a], colnames(sub_cor)[b], sub_cor[a, b]))
        }
      }
    }
    cat("\n")
  }
}

# =========================================================================
# 6. Recommendation: features to consider dropping
# =========================================================================

cat("============================================================\n")
cat("  6. RECOMMENDATIONS\n")
cat("============================================================\n\n")

cat("  Features to consider dropping (redundant with a stronger feature):\n\n")

# For each high-correlation pair, suggest dropping the less important one
# (using feature importance from step 2 as a guide, or correlation with target)
for (i in seq_len(nrow(pairs))) {
  f1 <- pairs$feat1[i]
  f2 <- pairs$feat2[i]
  # Compute mean absolute correlation with all move type classes
  cors1 <- mean(abs(sapply(levels(df$move_type), function(cls)
    cor(df[[f1]], as.integer(df$move_type == cls), use = "complete.obs"))))
  cors2 <- mean(abs(sapply(levels(df$move_type), function(cls)
    cor(df[[f2]], as.integer(df$move_type == cls), use = "complete.obs"))))

  keep <- if (cors1 >= cors2) f1 else f2
  drop <- if (cors1 >= cors2) f2 else f1

  cat(sprintf("  r=%+.3f  KEEP %-28s  DROP %-28s  (target corr: %.3f vs %.3f)\n",
              pairs$r[i], keep, drop, max(cors1, cors2), min(cors1, cors2)))
}

# =========================================================================
# 7. Summary stats
# =========================================================================

cat("\n============================================================\n")
cat("  7. SUMMARY\n")
cat("============================================================\n\n")

cat(sprintf("  Total features analyzed:       %d\n", length(features)))
cat(sprintf("  Highly correlated pairs:       %d  (|r| > 0.7)\n", nrow(pairs)))
cat(sprintf("  Moderately correlated pairs:   %d  (0.5 < |r| < 0.7)\n", nrow(mod_pairs)))
cat(sprintf("  Redundancy clusters:           %d\n", length(clusters)))
n_in_clusters <- sum(sapply(clusters, length))
cat(sprintf("  Features in clusters:          %d / %d\n", n_in_clusters, length(features)))
cat(sprintf("  Independent features:          %d\n", length(features) - n_in_clusters))

cat("\n============================================================\n")
cat("  Correlation analysis complete. Next step: 04_misclassification.R\n")
cat("============================================================\n")
