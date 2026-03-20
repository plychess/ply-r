library(ply)
pgn_dir <- file.path("../Midterm_Project/pgnmentor")
pgn_files <- sort(list.files(pgn_dir, pattern="[.]pgn$", full.names=TRUE))
use_files <- head(pgn_files, 100)
tmp <- tempfile(fileext=".pgn")
con <- file(tmp, open="wt")
for (f in use_files) {
  writeLines(readLines(f, warn=FALSE, encoding="UTF-8"), con)
  writeLines("", con)
}
close(con)

pos <- replay_all_games(tmp)
errs <- pos[!pos$ok, ]

# Show unique game_ids and first error per game
err_games <- unique(errs$game_id)
cat(sprintf("Error games: %d\n", length(err_games)))

lines <- readLines(tmp, warn=FALSE, encoding="UTF-8")
raw <- paste(lines, collapse="\n")
games_raw <- strsplit(raw, "\n(?=\\[Event )", perl=TRUE)[[1]]

parse_fen <- ply:::cpp_parse_fen
state_to_fen <- ply:::cpp_state_to_fen
san_to_uci <- ply:::cpp_san_to_uci
uci_to_ply <- ply:::cpp_uci_to_ply
apply_ply_fn <- ply:::cpp_apply_ply

for (gid in err_games) {
  g <- games_raw[[gid]]
  tags <- ply_pgn_parse_tags(g)
  mt <- ply_pgn_extract_movetext(g)
  moves <- strsplit(mt, " ")[[1]]
  moves <- moves[nzchar(moves)]
  cat(sprintf("\n=== Game %d: %s vs %s (%s) ===\n", gid,
              tags$White, tags$Black, tags$Event))
  cat(sprintf("  Moves: %d  Last 5: [%s]\n", length(moves),
              paste(tail(moves, 5), collapse=", ")))

  state <- parse_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
  for (i in seq_along(moves)) {
    uci <- tryCatch(san_to_uci(state, moves[i]), error=function(e) NA_character_)
    if (is.na(uci) || uci == "") {
      cat(sprintf("  FAIL ply %d: SAN='%s'  FEN='%s'\n", i, moves[i], state_to_fen(state)))
      break
    }
    ply_obj <- uci_to_ply(state, uci)
    state <- apply_ply_fn(state, ply_obj)
  }
}
