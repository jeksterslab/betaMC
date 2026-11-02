#' Diagonals of A from vech(A)
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param x Vector.
#'   Half-vectorization of a `k` by `k` matrix.
#' @param k Positive integer.
#'   Dimension of the `k` by `k` matrix.
#' @param loc Logical.
#'   If `loc = TRUE`, return the location of the diagonal elements
#'   in the input vector.
#'   If `loc = FALSE`, return the values of the diagonal elements.
#' @return Returns a vector of the diagonals of a matrix
#'   from its half-vectorization.
#'
#' @family Vectorization Functions
#' @keywords linearAlgebra vectorization internal
#' @noRd
.DiagofVech <- function(x,
                        k,
                        loc = FALSE) {
  if (length(x) == 1) {
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
