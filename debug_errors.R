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

lines <- readLines(tmp, warn=FALSE, encoding="UTF-8")
raw <- paste(lines, collapse="\n")
games_raw <- strsplit(raw, "\n(?=\\[Event )", perl=TRUE)[[1]]

# Use the internal cpp functions via :::
parse_fen <- ply:::cpp_parse_fen
state_to_fen <- ply:::cpp_state_to_fen
san_to_uci <- ply:::cpp_san_to_uci
uci_to_ply <- ply:::cpp_uci_to_ply
apply_ply <- ply:::cpp_apply_ply

for (gid in c(4659, 4674, 4901)) {
  g <- games_raw[[gid]]
  tags <- ply_pgn_parse_tags(g)
  mt <- ply_pgn_extract_movetext(g)
  moves <- strsplit(mt, " ")[[1]]
  moves <- moves[nzchar(moves)]
  cat(sprintf("\n=== Game %d: %s vs %s ===\n", gid, tags$White, tags$Black))
  cat(sprintf("Event: %s\n", tags$Event))
  cat(sprintf("Total moves: %d\n", length(moves)))

  state <- parse_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
  for (i in seq_along(moves)) {
    uci <- tryCatch(san_to_uci(state, moves[i]), error=function(e) NA_character_)
    if (is.na(uci) || uci == "") {
      fen <- state_to_fen(state)
      cat(sprintf("  FAIL at ply %d: SAN='%s'\n", i, moves[i]))
      cat(sprintf("  FEN: %s\n", fen))
      if (i > 1) cat(sprintf("  Previous moves: %s\n", paste(moves[max(1,i-3):(i-1)], collapse=" ")))
      if (i < length(moves)) cat(sprintf("  Next moves: %s\n", paste(moves[(i+1):min(length(moves),i+3)], collapse=" ")))
      break
    }
    ply_obj <- uci_to_ply(state, uci)
    state <- apply_ply(state, ply_obj)
  }
}
