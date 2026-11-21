#' Symmetric Matrix from Its Half-Vectorization (Internal)
#'
#' Builds the symmetric \eqn{k \times k} matrix
#' from \eqn{\operatorname{vech} \! \left( \boldsymbol{A} \right)},
#' its lower triangle stacked column by column.
#' No input checks: see `SymofVech()`.
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param x Numeric vector of length \eqn{k (k + 1) / 2}.
#'   Half-vectorization of a \eqn{k \times k} matrix,
#'   \eqn{\operatorname{vech} \! \left( \boldsymbol{A} \right)}.
#' @param k Positive integer.
#'   Dimension of the \eqn{k \times k} matrix.
#'
#' @return Returns a numeric \eqn{k \times k} symmetric matrix.
#'
#' @examples
#' .SymofVech(x = c(1, 0.5, 0.25, 2, 0.5, 3), k = 3)
#'
#' @family Symmetric Functions
#' @keywords linearAlgebra symmetric internal
#' @noRd
.SymofVech <- function(x,
                       k) {
  sym <- matrix(
    data = 0,
    nrow = k,
    ncol = k
  )
  sym[lower.tri(x = sym, diag = TRUE)] <- x
  sym[upper.tri(x = sym)] <- t(x = sym)[upper.tri(x = sym)]
  sym
}
