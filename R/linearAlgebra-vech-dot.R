#' Half-Vectorize a Matrix (Internal)
#'
#' Returns \eqn{\operatorname{vech} \! \left( \boldsymbol{A} \right)},
#' the lower triangle of a square matrix, diagonal included,
#' stacked column by column.
#' No input checks: see `Vech()`.
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param x Square matrix.
#'
#' @return Returns a vector of length \eqn{k (k + 1) / 2}.
#'
#' @examples
#' .Vech(x = toeplitz(x = (3:1) / 3))
#'
#' @family Vectorization Functions
#' @keywords linearAlgebra vectorization internal
#' @noRd
.Vech <- function(x) {
  x[
    lower.tri(
      x = x,
      diag = TRUE
    )
  ]
}
