# test_stress_90k.R — Stress test: replay games from pgnmentor
#
# Uses PGN files from the Midterm_Project/pgnmentor folder.
# Reports R-side parsing and C++ engine speed separately.

context("Stress Test — pgnmentor games")

skip_on_ci()
skip_on_cran()

pgn_dir <- normalizePath(
  file.path(Sys.getenv("TAL_PROJECT_ROOT", unset = "."), "..", "Midterm_Project", "pgnmentor"),
  mustWork = FALSE
)

skip_if_not(dir.exists(pgn_dir), paste("pgnmentor directory not found:", pgn_dir))

pgn_files <- sort(list.files(pgn_dir, pattern = "\\.pgn$", full.names = TRUE))
skip_if(length(pgn_files) == 0L, "No PGN files found in pgnmentor directory")

# ---------------------------------------------------------------------------
# Stress test with split timing: R parsing vs C++ engine
# ---------------------------------------------------------------------------

test_that("pgnmentor files replay successfully", {
  use_files <- head(pgn_files, 50)
  cat(sprintf("\n  Using %d PGN files from %s\n", length(use_files), pgn_dir))

  # --- Phase 1: Read + concatenate (I/O) ---
  t_io <- proc.time()
  tmp_pgn <- tempfile(fileext = ".pgn")
  on.exit(unlink(tmp_pgn), add = TRUE)
  con <- file(tmp_pgn, open = "wt")
  for (f in use_files) {
    writeLines(readLines(f, warn = FALSE, encoding = "UTF-8"), con)
    writeLines("", con)
  }
  close(con)
  elapsed_io <- (proc.time() - t_io)["elapsed"]

  file_mb <- file.size(tmp_pgn) / 1024^2
  cat(sprintf("  Combined PGN: %.1f MB\n", file_mb))

  # --- Phase 2: R-side PGN parsing (split + extract movetext) ---
  t_parse <- proc.time()
  lines     <- readLines(tmp_pgn, warn = FALSE, encoding = "UTF-8")
  raw       <- paste(lines, collapse = "\n")
  games_raw <- strsplit(raw, "\n(?=\\[Event )", perl = TRUE)[[1]]

  movetexts  <- vapply(games_raw, ply_pgn_extract_movetext, character(1),
                        USE.NAMES = FALSE)
  move_lists <- strsplit(movetexts, " ")
  move_lists <- lapply(move_lists, function(m) m[nzchar(m)])
  lens       <- lengths(move_lists)
  keep       <- lens > 0L
  move_lists <- move_lists[keep]
  lens       <- lens[keep]
  ids        <- which(keep)

  all_moves   <- unlist(move_lists, use.names = FALSE)
  game_starts <- cumsum(c(1L, lens[-length(lens)]))
  elapsed_parse <- (proc.time() - t_parse)["elapsed"]

  n_games <- length(ids)
  n_plies <- length(all_moves)
  cat(sprintf("  Games:       %s\n", format(n_games, big.mark = ",")))
  cat(sprintf("  Total plies: %s\n", format(n_plies, big.mark = ",")))

  # --- Phase 3: C++ engine replay (the actual benchmark) ---
  t_engine <- proc.time()
  positions <- cpp_replay_games_batch(ids, all_moves, game_starts)
  elapsed_engine <- (proc.time() - t_engine)["elapsed"]

  n_ok    <- sum(positions$ok)
  n_err   <- sum(!positions$ok)
  err_pct <- 100 * n_err / n_plies

  engine_rate <- n_plies / elapsed_engine
  total_rate  <- n_plies / (elapsed_io + elapsed_parse + elapsed_engine)

  cat(sprintf("\n  --- Timing Breakdown ---\n"))
  cat(sprintf("  File I/O:      %6.2f s\n", elapsed_io))
  cat(sprintf("  R PGN parsing: %6.2f s\n", elapsed_parse))
  cat(sprintf("  C++ engine:    %6.2f s\n", elapsed_engine))
  cat(sprintf("  Total:         %6.2f s\n", elapsed_io + elapsed_parse + elapsed_engine))
  cat(sprintf("\n  --- Results ---\n"))
  cat(sprintf("  OK plies:      %s\n", format(n_ok, big.mark = ",")))
  cat(sprintf("  Errors:        %s (%.2f%%)\n", format(n_err, big.mark = ","), err_pct))
  cat(sprintf("  Engine speed:  %s plies/sec\n", format(round(engine_rate), big.mark = ",")))
  cat(sprintf("  End-to-end:    %s plies/sec\n", format(round(total_rate), big.mark = ",")))

  expect_gt(n_games, 0)
  expect_lt(err_pct, 2.0)
})
