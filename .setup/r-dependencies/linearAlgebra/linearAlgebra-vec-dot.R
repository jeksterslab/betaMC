#' Vectorize a Matrix (Internal)
#'
#' Returns \eqn{\operatorname{vec} \! \left( \boldsymbol{A} \right)},
#' the columns of a matrix stacked into one vector.
#' No input checks: see `Vec()`.
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param x Matrix.
#'
#' @return Returns a vector of length `nrow(x) * ncol(x)`.
#'
#' @examples
#' .Vec(x = matrix(data = 1:4, nrow = 2))
#'
#' @family Vectorization Functions
#' @keywords linearAlgebra vectorization internal
#' @noRd
.Vec <- function(x) {
  dim(x = x) <- NULL
  x
}
