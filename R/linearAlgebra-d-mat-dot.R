#' The Duplication Matrix
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param k Positive integer.
#'   Dimension of the `k` by `k` matrix.
#'
#' @return Returns a matrix.
#'
#' @family Symmetric Functions
#' @keywords linearAlgebra symmetric internal
#' @noRd
.DMat <- function(k) {
  sym <- matrix(
    0,
    nrow = k,
    ncol = k
  )
  q <- seq_len(
    0.5 * k * (k + 1)
  )
  sym[lower.tri(sym, diag = TRUE)] <- q
  sym[upper.tri(sym)] <- t(sym)[upper.tri(sym)]
  return(
    outer(
      X = .Vec(sym),
      Y = q,
      FUN = function(x, y) {
        ifelse(
          test = x == y,
          yes = 1,
          no = 0
        )
      }
    )
  )
}
