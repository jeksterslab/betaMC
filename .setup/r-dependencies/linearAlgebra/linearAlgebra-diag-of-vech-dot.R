#' Diagonal Elements of a Matrix from Its Half-Vectorization (Internal)
#'
#' Returns the diagonal elements of a \eqn{k \times k} matrix
#' \eqn{\boldsymbol{A}},
#' or their positions,
#' from \eqn{\operatorname{vech} \! \left( \boldsymbol{A} \right)}.
#' No input checks.
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param x Numeric vector of length \eqn{k (k + 1) / 2}.
#'   Half-vectorization of a \eqn{k \times k} matrix,
#'   \eqn{\operatorname{vech} \! \left( \boldsymbol{A} \right)}.
#' @param k Positive integer.
#'   Dimension of the \eqn{k \times k} matrix.
#' @param loc Logical.
#'   If `loc = TRUE`, return the positions of the diagonal elements
#'   in `x`.
#'   If `loc = FALSE`, return the values of the diagonal elements.
#'
#' @return Returns a vector of length \eqn{k}:
#'   the diagonal elements, or their positions in `x`.
#'
#' @examples
#' .DiagofVech(x = c(1, 0.5, 0.25, 2, 0.5, 3), k = 3)
#' .DiagofVech(x = c(1, 0.5, 0.25, 2, 0.5, 3), k = 3, loc = TRUE)
#'
#' @family Vectorization Functions
#' @keywords linearAlgebra vectorization internal
#' @noRd
.DiagofVech <- function(x,
                        k,
                        loc = FALSE) {
  if (length(x = x) == 1) {
    if (loc) {
      out <- 1
    } else {
      out <- x[1]
    }
  } else {
    j <- 0.5 * (
      2 * k * 1:k - 2 * k + 3 * 1:k - (1:k) * (1:k)
    )
    if (loc) {
      out <- j
    } else {
      out <- x[j]
    }
  }
  out
}
