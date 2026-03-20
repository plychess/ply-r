library(ply)

parse_fen <- ply:::cpp_parse_fen
gen_moves <- ply:::cpp_generate_legal_moves
apply_ply <- ply:::cpp_apply_ply

state <- parse_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")

# Perft: recursively generate and apply all legal moves to depth d
perft <- function(state, depth) {
  if (depth == 0L) return(1L)
  moves <- gen_moves(state)
  if (depth == 1L) return(length(moves))
  total <- 0L
  for (m in moves) {
    s2 <- apply_ply(state, m)
    total <- total + perft(s2, depth - 1L)
  }
  total
}

# Known values: depth 1=20, 2=400, 3=8902, 4=197281, 5=4865609
for (d in 1:5) {
  t0 <- proc.time()
  nodes <- perft(state, d)
  elapsed <- (proc.time() - t0)["elapsed"]
  nps <- if (elapsed > 0) nodes / elapsed else Inf
  cat(sprintf("  Perft(%d): %10s nodes in %7.3fs  =  %s NPS\n",
              d, format(nodes, big.mark=","), elapsed,
              format(round(nps), big.mark=",")))
}
