#' The Duplication Matrix (Internal)
#'
#' Returns the duplication matrix \eqn{\boldsymbol{D}_{k}},
#' the \eqn{k^2 \times k (k + 1) / 2} matrix
#' with \eqn{\boldsymbol{D}_{k}
#' \operatorname{vech} \! \left( \boldsymbol{A} \right) =
#' \operatorname{vec} \! \left( \boldsymbol{A} \right)}
#' for every symmetric \eqn{k \times k} matrix \eqn{\boldsymbol{A}}.
#' No input checks: see `DMat()`.
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param k Positive integer.
#'   Dimension of the \eqn{k \times k} matrix.
#'
#' @return Returns a numeric \eqn{k^2 \times k (k + 1) / 2} matrix.
#'
#' @examples
#' .DMat(k = 3L)
#'
#' @family Symmetric Functions
#' @keywords linearAlgebra symmetric internal
#' @noRd
.DMat <- function(k) {
  q <- 0.5 * k * (k + 1)
  sym <- matrix(
    data = 0,
    nrow = k,
    ncol = k
  )
  sym[lower.tri(x = sym, diag = TRUE)] <- seq_len(length.out = q)
  sym[upper.tri(x = sym)] <- t(x = sym)[upper.tri(x = sym)]
  out <- matrix(
    data = 0,
    nrow = k * k,
    ncol = q
  )
  # one 1 per row: row i of vec(A) takes element sym[i] of vech(A)
  out[cbind(seq_len(length.out = k * k), .Vec(x = sym))] <- 1
  out
}
