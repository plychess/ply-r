library(ply)
pgn_dir <- file.path("../Midterm_Project/pgnmentor")
pgn_files <- sort(list.files(pgn_dir, pattern="[.]pgn$", full.names=TRUE))
use_files <- head(pgn_files, 50)
tmp <- tempfile(fileext=".pgn")
con <- file(tmp, open="wt")
for (f in use_files) {
  writeLines(readLines(f, warn=FALSE, encoding="UTF-8"), con)
  writeLines("", con)
}
close(con)

pos <- replay_all_games(tmp)
errs <- pos[!pos$ok, ]
err_games <- unique(errs$game_id)
cat(sprintf("Error games: %d\n", length(err_games)))

lines <- readLines(tmp, warn=FALSE, encoding="UTF-8")
raw <- paste(lines, collapse="\n")
games_raw <- strsplit(raw, "\n(?=\\[Event )", perl=TRUE)[[1]]

for (gid in err_games[1:3]) {
  g <- games_raw[[gid]]
  tags <- ply_pgn_parse_tags(g)
  mt <- ply_pgn_extract_movetext(g)
  moves <- strsplit(mt, " ")[[1]]
  moves <- moves[nzchar(moves)]

  first_err_ply <- min(errs$ply_num[errs$game_id == gid])
  first_err_fen <- errs$fen[errs$game_id == gid][1]

  cat(sprintf("\n=== Game %d: %s vs %s ===\n", gid, tags$White, tags$Black))
  cat(sprintf("  Result: %s\n", tags$Result))
  cat(sprintf("  Extracted moves: %d\n", length(moves)))
  cat(sprintf("  First error ply: %d\n", first_err_ply))
  cat(sprintf("  FEN at error: %s\n", first_err_fen))
  cat(sprintf("  SAN at error ply: '%s'\n", if (first_err_ply <= length(moves)) moves[first_err_ply] else "(beyond move list)"))
  cat(sprintf("  Last 5 raw chars of game block: '%s'\n", substring(g, nchar(g)-100, nchar(g))))
}
