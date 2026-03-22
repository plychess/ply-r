# board_render.R — Visual board rendering from FEN
#
# Two renderers:
#   ply_board_text(fen)  — Unicode board in console
#   ply_board_html(fen, highlights, labels)  — HTML/CSS board for reports

# Unicode chess piece map
.piece_unicode <- c(
  K = "\u2654", Q = "\u2655", R = "\u2656", B = "\u2657", N = "\u2658", P = "\u2659",
  k = "\u265A", q = "\u265B", r = "\u265C", b = "\u265D", n = "\u265E", p = "\u265F"
)

#' Parse FEN board string into 8x8 matrix
#' @param fen FEN string
#' @return 8x8 character matrix (row 1 = rank 8, row 8 = rank 1)
ply_fen_to_matrix <- function(fen) {
  board_str <- strsplit(fen, " ")[[1]][1]
  ranks <- strsplit(board_str, "/")[[1]]
  mat <- matrix("", nrow = 8, ncol = 8)
  for (r in 1:8) {
    col <- 1
    for (ch in strsplit(ranks[r], "")[[1]]) {
      if (grepl("[0-9]", ch)) {
        col <- col + as.integer(ch)
      } else {
        mat[r, col] <- ch
        col <- col + 1
      }
    }
  }
  mat
}

#' Render a chess board in the console using Unicode pieces
#'
#' @param fen FEN string
#' @param flip Logical; if TRUE, show from Black's perspective
#' @export
ply_board_text <- function(fen, flip = FALSE) {
  mat <- ply_fen_to_matrix(fen)
  parts <- strsplit(fen, " ")[[1]]
  side <- if (length(parts) >= 2) parts[2] else "w"

  rows <- if (flip) 8:1 else 1:8
  cols <- if (flip) 8:1 else 1:8
  rank_labels <- if (flip) 1:8 else 8:1

  cat("\n")
  for (i in seq_along(rows)) {
    r <- rows[i]
    cat(sprintf("  %d  ", rank_labels[i]))
    for (c in cols) {
      piece <- mat[r, c]
      is_dark <- ((r + c) %% 2 == 0)
      bg <- if (is_dark) "\u2591" else " "
      if (piece == "") {
        cat(sprintf(" %s ", bg))
      } else {
        symbol <- .piece_unicode[piece]
        if (is.na(symbol)) symbol <- piece
        cat(sprintf(" %s ", symbol))
      }
    }
    cat("\n")
  }
  file_labels <- if (flip) rev(letters[1:8]) else letters[1:8]
  cat("     ", paste(sprintf(" %s ", file_labels), collapse = ""), "\n")
  cat(sprintf("\n  %s to move\n\n", if (side == "w") "White" else "Black"))
  invisible(mat)
}

#' Render a chess board as HTML (for Rmd reports and viewer)
#'
#' @param fen FEN string
#' @param highlights Named list of square -> color, e.g. list(e4 = "#ff0", d5 = "#f00")
#' @param labels Named list of square -> label text, e.g. list(e4 = "Q=+0.3")
#' @param title Optional title above the board
#' @param size Square size in pixels (default 50)
#' @param flip Show from Black's perspective
#' @return HTML string (also prints via htmltools if available)
#' @export
ply_board_html <- function(fen, highlights = NULL, labels = NULL,
                            title = NULL, size = 50, flip = FALSE) {
  mat <- ply_fen_to_matrix(fen)
  parts <- strsplit(fen, " ")[[1]]
  side <- if (length(parts) >= 2) parts[2] else "w"

  # Piece to HTML entity (larger, cleaner than Unicode)
  piece_html <- c(
    K = "&#9812;", Q = "&#9813;", R = "&#9814;", B = "&#9815;", N = "&#9816;", P = "&#9817;",
    k = "&#9818;", q = "&#9819;", r = "&#9820;", b = "&#9821;", n = "&#9822;", p = "&#9823;"
  )

  rows <- if (flip) 8:1 else 1:8
  cols <- if (flip) 8:1 else 1:8
  rank_labels <- if (flip) 1:8 else 8:1

  html <- sprintf('<div style="display:inline-block;font-family:sans-serif;">\n')
  if (!is.null(title)) {
    html <- paste0(html, sprintf('<div style="font-weight:bold;margin-bottom:4px;font-size:14px;">%s</div>\n', title))
  }
  html <- paste0(html, '<table style="border-collapse:collapse;border:2px solid #333;">\n')

  for (i in seq_along(rows)) {
    r <- rows[i]
    html <- paste0(html, '<tr>\n')
    # Rank label
    html <- paste0(html, sprintf(
      '<td style="width:20px;text-align:center;font-size:12px;color:#666;">%d</td>\n',
      rank_labels[i]))

    for (c in cols) {
      piece <- mat[r, c]
      is_dark <- ((r + c) %% 2 == 0)
      bg_color <- if (is_dark) "#b58863" else "#f0d9b5"

      # Check for highlight
      file_letter <- letters[c]
      rank_num <- 9 - r
      sq_name <- paste0(file_letter, rank_num)
      if (!is.null(highlights) && sq_name %in% names(highlights)) {
        bg_color <- highlights[[sq_name]]
      }

      # Piece display
      piece_display <- ""
      if (piece != "") {
        p <- piece_html[piece]
        if (!is.na(p)) piece_display <- p else piece_display <- piece
      }

      # Label overlay
      label_html <- ""
      if (!is.null(labels) && sq_name %in% names(labels)) {
        label_html <- sprintf(
          '<div style="position:absolute;bottom:0;right:1px;font-size:8px;color:#000;background:rgba(255,255,255,0.7);padding:0 2px;border-radius:2px;">%s</div>',
          labels[[sq_name]])
      }

      html <- paste0(html, sprintf(
        '<td style="position:relative;width:%dpx;height:%dpx;background:%s;text-align:center;font-size:%dpx;line-height:%dpx;">%s%s</td>\n',
        size, size, bg_color, as.integer(size * 0.7), size,
        piece_display, label_html))
    }
    html <- paste0(html, '</tr>\n')
  }

  # File labels
  html <- paste0(html, '<tr><td></td>')
  file_labels <- if (flip) rev(letters[1:8]) else letters[1:8]
  for (f in file_labels) {
    html <- paste0(html, sprintf(
      '<td style="text-align:center;font-size:12px;color:#666;">%s</td>', f))
  }
  html <- paste0(html, '</tr>\n')
  html <- paste0(html, '</table>\n')
  html <- paste0(html, sprintf(
    '<div style="font-size:12px;color:#666;margin-top:4px;">%s to move</div>\n',
    if (side == "w") "White" else "Black"))
  html <- paste0(html, '</div>\n')

  # Try to display in viewer
  if (interactive() && requireNamespace("htmltools", quietly = TRUE)) {
    htmltools::html_print(htmltools::HTML(html))
  }

  class(html) <- c("ply_board_html", "html", "character")
  invisible(html)
}

#' Print method for ply_board_html (knitr integration)
#' @export
knit_print.ply_board_html <- function(x, ...) {
  knitr::asis_output(x)
}

#' Render two boards side by side (for before/after comparison)
#'
#' @param fen1 First FEN
#' @param fen2 Second FEN
#' @param title1 Title for first board
#' @param title2 Title for second board
#' @param highlights1 Highlights for first board
#' @param highlights2 Highlights for second board
#' @export
ply_board_compare <- function(fen1, fen2, title1 = "Before", title2 = "After",
                               highlights1 = NULL, highlights2 = NULL, size = 45) {
  b1 <- ply_board_html(fen1, highlights = highlights1, title = title1, size = size)
  b2 <- ply_board_html(fen2, highlights = highlights2, title = title2, size = size)
  html <- sprintf(
    '<div style="display:flex;gap:20px;align-items:flex-start;">%s%s</div>',
    as.character(b1), as.character(b2))
  class(html) <- c("ply_board_html", "html", "character")
  if (interactive() && requireNamespace("htmltools", quietly = TRUE)) {
    htmltools::html_print(htmltools::HTML(html))
  }
  invisible(html)
}
