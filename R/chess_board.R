# chess_board.R — FEN parser and board representation
# Matches the square mapping used by the C++ bitboard engine.
# Each square is encoded as: "." empty, or "PNBRQKpnbrqk" (uppercase=white).

parse_fen <- function(fen) {
  parts <- strsplit(fen, " ")[[1]]
  ranks <- strsplit(parts[1], "/")[[1]]

  board <- matrix(".", nrow = 8, ncol = 8)  # board[rank, file], rank 8 = row 1
  for (i in seq_along(ranks)) {
    col <- 1L
    for (ch in strsplit(ranks[i], "")[[1]]) {
      if (grepl("[1-8]", ch)) {
        col <- col + as.integer(ch)
      } else {
        board[i, col] <- ch
        col <- col + 1L
      }
    }
  }

  list(
    board          = board,
    side_to_move   = parts[2],
    castling       = if (length(parts) >= 3) parts[3] else "-",
    en_passant     = if (length(parts) >= 4) parts[4] else "-",
    halfmove_clock = if (length(parts) >= 5) as.integer(parts[5]) else 0L,
    fullmove       = if (length(parts) >= 6) as.integer(parts[6]) else 1L
  )
}

# Convert algebraic square (e.g. "e4") to board[row, col] indices
sq_to_rc <- function(sq) {
  file <- match(substr(sq, 1, 1), letters[1:8])
  rank <- as.integer(substr(sq, 2, 2))
  row  <- 9L - rank  # rank 8 = row 1
  c(row = row, col = file)
}

piece_at <- function(board, sq) {
  rc <- sq_to_rc(sq)
  board[rc[1], rc[2]]
}

is_white_piece <- function(p) p != "." && p == toupper(p)
is_black_piece <- function(p) p != "." && p == tolower(p)
is_empty       <- function(p) p == "."

# Count material: returns named list (white_material, black_material, balance, num_pieces)
count_material <- function(board) {
  vals <- c(P=1, N=3, B=3, R=5, Q=9, K=0, p=1, n=3, b=3, r=5, q=9, k=0)
  pieces <- board[board != "."]
  white <- sum(vals[pieces[pieces == toupper(pieces)]])
  black <- sum(vals[pieces[pieces == tolower(pieces)]])
  list(
    white_material = white,
    black_material = black,
    balance        = white - black,
    num_pieces     = length(pieces)
  )
}

# Detect if the side to move is in check
# Uses the same reverse-attack approach as the C++ engine.
is_in_check <- function(parsed_fen) {
  board <- parsed_fen$board
  side  <- parsed_fen$side_to_move
  king_char <- if (side == "w") "K" else "k"

  # Find king
  king_pos <- which(board == king_char, arr.ind = TRUE)
  if (nrow(king_pos) == 0) return(FALSE)
  kr <- king_pos[1, 1]; kc <- king_pos[1, 2]

  is_enemy <- if (side == "w") is_black_piece else is_white_piece
  enemy_pawn   <- if (side == "w") "p" else "P"
  enemy_knight <- if (side == "w") "n" else "N"
  enemy_bishop <- if (side == "w") "b" else "B"
  enemy_rook   <- if (side == "w") "r" else "R"
  enemy_queen  <- if (side == "w") "q" else "Q"
  enemy_king   <- if (side == "w") "k" else "K"

  # Pawn attacks
  if (side == "w") {
    if (kr >= 2) {
      if (kc >= 2 && board[kr - 1, kc - 1] == enemy_pawn) return(TRUE)
      if (kc <= 7 && board[kr - 1, kc + 1] == enemy_pawn) return(TRUE)
    }
  } else {
    if (kr <= 7) {
      if (kc >= 2 && board[kr + 1, kc - 1] == enemy_pawn) return(TRUE)
      if (kc <= 7 && board[kr + 1, kc + 1] == enemy_pawn) return(TRUE)
    }
  }

  # Knight attacks
  knight_offsets <- list(c(-2,-1),c(-2,1),c(-1,-2),c(-1,2),c(1,-2),c(1,2),c(2,-1),c(2,1))
  for (off in knight_offsets) {
    r <- kr + off[1]; c_ <- kc + off[2]
    if (r >= 1 && r <= 8 && c_ >= 1 && c_ <= 8 && board[r, c_] == enemy_knight)
      return(TRUE)
  }

  # King attacks (adjacent)
  for (dr in -1:1) for (dc in -1:1) {
    if (dr == 0 && dc == 0) next
    r <- kr + dr; c_ <- kc + dc
    if (r >= 1 && r <= 8 && c_ >= 1 && c_ <= 8 && board[r, c_] == enemy_king)
      return(TRUE)
  }

  # Sliding attacks: diagonals (bishop/queen)
  diag_dirs <- list(c(-1,-1), c(-1,1), c(1,-1), c(1,1))
  for (d in diag_dirs) {
    r <- kr + d[1]; c_ <- kc + d[2]
    while (r >= 1 && r <= 8 && c_ >= 1 && c_ <= 8) {
      p <- board[r, c_]
      if (p != ".") {
        if (p == enemy_bishop || p == enemy_queen) return(TRUE)
        break
      }
      r <- r + d[1]; c_ <- c_ + d[2]
    }
  }

  # Sliding attacks: orthogonals (rook/queen)
  ortho_dirs <- list(c(-1,0), c(1,0), c(0,-1), c(0,1))
  for (d in ortho_dirs) {
    r <- kr + d[1]; c_ <- kc + d[2]
    while (r >= 1 && r <= 8 && c_ >= 1 && c_ <= 8) {
      p <- board[r, c_]
      if (p != ".") {
        if (p == enemy_rook || p == enemy_queen) return(TRUE)
        break
      }
      r <- r + d[1]; c_ <- c_ + d[2]
    }
  }

  FALSE
}
