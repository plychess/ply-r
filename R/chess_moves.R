# chess_moves.R — Move classification from FEN + UCI move
# Determines capture, castling, en passant, promotion, check-giving moves.

# Detect capture: destination square has an opponent piece
classify_capture <- function(board, uci_move, side_to_move, en_passant_sq) {
  to_sq <- substr(uci_move, 3, 4)
  dest  <- piece_at(board, to_sq)

  # Standard capture
  is_opp <- if (side_to_move == "w") is_black_piece else is_white_piece
  if (is_opp(dest)) return(TRUE)

  # En passant: pawn captures diagonally to empty square == EP square
  from_sq <- substr(uci_move, 1, 2)
  mover   <- piece_at(board, from_sq)
  if (tolower(mover) == "p" && to_sq == en_passant_sq && is_empty(dest))
    return(TRUE)

  FALSE
}

# Detect castling: king moves two squares laterally
classify_castling <- function(uci_move, board) {
  from_sq <- substr(uci_move, 1, 2)
  mover   <- piece_at(board, from_sq)
  if (tolower(mover) != "k") return(FALSE)
  uci_move %in% c("e1g1", "e1c1", "e8g8", "e8c8")
}

# Detect promotion: UCI move has 5th character
classify_promotion <- function(uci_move) {
  nchar(uci_move) >= 5
}

# Detect en passant capture
classify_en_passant <- function(board, uci_move, en_passant_sq) {
  if (en_passant_sq == "-") return(FALSE)
  from_sq <- substr(uci_move, 1, 2)
  to_sq   <- substr(uci_move, 3, 4)
  mover   <- piece_at(board, from_sq)
  tolower(mover) == "p" && to_sq == en_passant_sq
}

# Apply a UCI move to a parsed board and check if opponent is now in check
gives_check <- function(parsed_fen, uci_move) {
  board <- parsed_fen$board
  from_sq <- substr(uci_move, 1, 2)
  to_sq   <- substr(uci_move, 3, 4)
  from_rc <- sq_to_rc(from_sq)
  to_rc   <- sq_to_rc(to_sq)

  # Copy board
  new_board <- board

  # Handle en passant removal
  mover <- board[from_rc[1], from_rc[2]]
  if (tolower(mover) == "p" && parsed_fen$en_passant != "-" &&
      to_sq == parsed_fen$en_passant) {
    cap_row <- from_rc[1]  # captured pawn is on same rank as moving pawn
    cap_col <- to_rc[2]
    new_board[cap_row, cap_col] <- "."
  }

  # Remove from, place on to
  new_board[from_rc[1], from_rc[2]] <- "."

  # Promotion
  if (nchar(uci_move) >= 5) {
    promo_char <- substr(uci_move, 5, 5)
    if (parsed_fen$side_to_move == "w") promo_char <- toupper(promo_char)
    new_board[to_rc[1], to_rc[2]] <- promo_char
  } else {
    new_board[to_rc[1], to_rc[2]] <- mover
  }

  # Handle castling rook movement
  if (tolower(mover) == "k") {
    if (uci_move == "e1g1") { new_board[8, 8] <- "."; new_board[8, 6] <- "R" }
    if (uci_move == "e1c1") { new_board[8, 1] <- "."; new_board[8, 4] <- "R" }
    if (uci_move == "e8g8") { new_board[1, 8] <- "."; new_board[1, 6] <- "r" }
    if (uci_move == "e8c8") { new_board[1, 1] <- "."; new_board[1, 4] <- "r" }
  }

  # Check if the opponent's king is now attacked
  opp_side <- if (parsed_fen$side_to_move == "w") "b" else "w"
  new_parsed <- list(board = new_board, side_to_move = opp_side)
  is_in_check(new_parsed)
}
