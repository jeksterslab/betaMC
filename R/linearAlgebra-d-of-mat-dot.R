#' Deviation from the Center (Matrix Input, Internal)
#'
#' Subtracts the center of each column from that column of `x`.
#' No input checks: see `Deviation()`.
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param x Numeric matrix.
#'   Data matrix with `n` rows and `k` columns.
#' @param center Numeric vector of length `k`.
#'   Center of each column.
#' @param n Positive integer.
#'   Number of rows of `x`.
#' @param k Positive integer.
#'   Number of columns of `x`.
#'
#' @return Returns a numeric matrix with the dimensions of `x`.
#'
#' @examples
#' .DofMat(
#'   x = matrix(data = 1:6, ncol = 2),
#'   center = c(2, 5),
#'   n = 3,
#'   k = 2
#' )
#'
#' @family Scaling Functions
#' @keywords linearAlgebra scaling internal
#' @noRd
.DofMat <- function(x,
                    center,
                    n,
                    k) {
  x - rep(
    x = center,
    times = rep(
      x = n,
      times = k
    )
  )
}
