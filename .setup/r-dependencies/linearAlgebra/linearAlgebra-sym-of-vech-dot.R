#' Symmetric matrix A from vech(A)
#'
#' Symmetric matrix from its half-vectorization.
#'
#' Generates an
#' \eqn{k \times k}
#' symmetric matrix
#' from a
#' \eqn{\frac{1}{2}k(k + 1)}
#' vector.
#'
#' @return Returns a `k` by `k` matrix.
#'
#' @param x Vector of length `0.5 * k(k + 1)`.
#'   Half-vectorization of a `k` by `k` matrix.
#'   \eqn{\mathrm{vech} \left( \mathbf{A}_{k \times k} \right)}.
#' @param k Positive integer.
#'   Dimension of the `k` by `k` matrix.
#'
#' @family Symmetric Functions
#' @keywords linearAlgebra symmetric dot internal
#' @noRd
.SymofVech <- function(x,
                       k) {
  sym <- matrix(
    data = 0,
    nrow = k,
    ncol = k
  )
  sym[lower.tri(sym, diag = TRUE)] <- x
  sym[upper.tri(sym)] <- t(sym)[upper.tri(sym)]
  sym
}
