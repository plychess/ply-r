#' ply: A Bitboard Chess Engine for R
#'
#' Provides a fully legal chess move generator and game engine
#' implemented in C++17 via Rcpp. The package exposes two layers:
#'
#' \itemize{
#'   \item A high-level R API (\code{ply_*} functions) for everyday use.
#'   \item An internal Rcpp bridge (\code{cpp_*} functions) used by the exported
#'   R helpers and test infrastructure.
#' }
#'
#' @keywords internal
"_PACKAGE"
