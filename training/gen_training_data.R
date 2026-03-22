# gen_training_data.R — Generate training data for NNUE using existing pipeline
#
# Uses our proven PGN replay + enrichment pipeline, adds Stockfish evals.
#
# Run from project root:
#   Rscript training/gen_training_data.R --player Tal --pgn Tal.pgn --depth 10 --max-games 100
#
# Output: training/data/<player>_d<depth>.rds

library(ply)
ns <- getNamespace("ply")
for (fn in ls(ns, pattern = "^cpp_")) assign(fn, get(fn, envir = ns))

# ---- Parse args ----
args <- commandArgs(trailingOnly = TRUE)
player_name <- "Tal"
pgn_path    <- "Tal.pgn"
sf_depth    <- 10
max_games   <- 0  # 0 = all
sf_path     <- "/opt/homebrew/bin/stockfish"
out_dir     <- "training/data"

i <- 1
while (i <= length(args)) {
  if (args[i] == "--player")    { player_name <- args[i+1]; i <- i+2 }
  else if (args[i] == "--pgn")  { pgn_path <- args[i+1]; i <- i+2 }
  else if (args[i] == "--depth") { sf_depth <- as.integer(args[i+1]); i <- i+2 }
  else if (args[i] == "--max-games") { max_games <- as.integer(args[i+1]); i <- i+2 }
  else if (args[i] == "--stockfish") { sf_path <- args[i+1]; i <- i+2 }
  else if (args[i] == "--out")  { out_dir <- args[i+1]; i <- i+2 }
  else i <- i+1
}

cat("=== Training Data Generator ===\n")
cat(sprintf("  Player:     %s\n", player_name))
cat(sprintf("  PGN:        %s\n", pgn_path))
cat(sprintf("  SF depth:   %d\n", sf_depth))
cat(sprintf("  Output:     %s/\n\n", out_dir))

# ---- Replay games using our proven pipeline ----
cat("Replaying games...\n")
replayed <- replay_all_games(pgn_path)
cat(sprintf("  Total plies: %d\n", nrow(replayed)))

# Filter to player's turns only
games_meta <- ply_pgn_load_games(pgn_path)

# Safe matching: game_id may not align with row number if empty games were skipped
replayed <- replayed[replayed$ok, ]
valid_ids <- replayed$game_id <= nrow(games_meta)
replayed <- replayed[valid_ids, ]

is_white <- grepl(player_name, games_meta$White[replayed$game_id], ignore.case = TRUE)
is_black <- grepl(player_name, games_meta$Black[replayed$game_id], ignore.case = TRUE)
replayed$player_color <- ifelse(is_white, "White", ifelse(is_black, "Black", NA))

# Drop positions where player wasn't found
replayed <- replayed[!is.na(replayed$player_color), ]

player_turns <- replayed[
  (replayed$player_color == "White" & replayed$ply_num %% 2 == 1) |
  (replayed$player_color == "Black" & replayed$ply_num %% 2 == 0), ]

if (max_games > 0) {
  keep_games <- unique(player_turns$game_id)[seq_len(min(max_games, length(unique(player_turns$game_id))))]
  player_turns <- player_turns[player_turns$game_id %in% keep_games, ]
}

cat(sprintf("  Player positions: %d across %d games\n\n",
            nrow(player_turns), length(unique(player_turns$game_id))))

# ---- Enrich with our C++ features ----
cat("Enriching with C++ features...\n")
features <- cpp_enrich_batch(player_turns$fen, player_turns$uci_move)
df <- cbind(player_turns, features)
cat(sprintf("  Enriched: %d positions x %d features\n\n", nrow(df), ncol(features)))

# ---- Get Stockfish evaluations ----
cat("Getting Stockfish evaluations...\n")
cat(sprintf("  This will evaluate %d positions at depth %d\n", nrow(df), sf_depth))

library(processx)

# Start Stockfish as interactive process
sf <- process$new(sf_path, stdin = "|", stdout = "|", stderr = "|")

sf_write <- function(cmd) { sf$write_input(paste0(cmd, "\n")) }
sf_read_until <- function(token) {
  lines <- character()
  repeat {
    line <- sf$read_output_lines(n = 1)
    if (length(line) == 0) { Sys.sleep(0.001); next }
    lines <- c(lines, line)
    if (any(grepl(token, line))) break
  }
  lines
}

sf_write("uci")
sf_read_until("uciok")
sf_write(sprintf("setoption name Threads value %d", min(4L, parallel::detectCores())))
sf_write("setoption name Hash value 256")
sf_write("isready")
sf_read_until("readyok")

sf_evals <- integer(nrow(df))
t0 <- proc.time()

for (i in seq_len(nrow(df))) {
  fen <- df$fen[i]
  is_white <- grepl(" w ", fen)

  sf_write(paste("position fen", fen))
  sf_write(paste("go depth", sf_depth))

  eval_cp <- 0L
  lines <- sf_read_until("^bestmove")
  for (line in lines) {
    if (grepl("score cp", line)) {
      m <- regmatches(line, regexpr("score cp -?[0-9]+", line))
      if (length(m) > 0) eval_cp <- as.integer(sub("score cp ", "", m))
    } else if (grepl("score mate", line)) {
      m <- regmatches(line, regexpr("score mate -?[0-9]+", line))
      if (length(m) > 0) {
        mate_in <- as.integer(sub("score mate ", "", m))
        eval_cp <- if (mate_in > 0) 30000L else -30000L
      }
    }
  }

  # Normalize to white's perspective
  if (!is_white) eval_cp <- -eval_cp
  sf_evals[i] <- eval_cp

  if (i %% 100 == 0 || i == nrow(df)) {
    elapsed <- (proc.time() - t0)[3]
    rate <- i / elapsed
    eta <- (nrow(df) - i) / rate
    cat(sprintf("\r  %d / %d  (%.0f pos/sec, ETA %.0fs)    ", i, nrow(df), rate, eta))
  }
}

sf_write("quit")
sf$wait(timeout = 5000)
sf$kill()

df$sf_eval <- sf_evals
elapsed <- (proc.time() - t0)[3]
cat(sprintf("\n  Done: %d evals in %.0fs (%.0f pos/sec)\n\n", nrow(df), elapsed, nrow(df)/elapsed))

# ---- Add metadata ----
df$player <- player_name

# ---- Save ----
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
out_path <- file.path(out_dir, sprintf("%s_d%d.rds", player_name, sf_depth))
saveRDS(df, out_path)
cat(sprintf("  Saved: %s (%.1f MB)\n", out_path, file.size(out_path) / 1e6))

# ---- Summary stats ----
cat(sprintf("\n  Summary:\n"))
cat(sprintf("    Positions:     %d\n", nrow(df)))
cat(sprintf("    SF eval range: [%d, %d] cp\n", min(df$sf_eval), max(df$sf_eval)))
cat(sprintf("    SF eval mean:  %+.0f cp\n", mean(df$sf_eval)))
cat(sprintf("    Classic vs SF correlation: %.3f\n",
            cor(df$position_eval, df$sf_eval)))
cat(sprintf("    Features:      %d columns\n", ncol(df)))
