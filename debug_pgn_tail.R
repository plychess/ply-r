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

for (gid in c(4659, 4674, 4901)) {
  cat(sprintf("\n=== Game %d (last 400 chars) ===\n", gid))
  g <- games_raw[[gid]]
  start <- max(1, nchar(g) - 400)
  cat(substring(g, start, nchar(g)))
  cat("\n")
}
