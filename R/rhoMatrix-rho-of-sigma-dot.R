#' Correlation Matrix from a Covariance Matrix (Internal)
#'
#' Computes the correlation matrix
#' \eqn{\boldsymbol{P} = \boldsymbol{Q} \boldsymbol{\Sigma} \boldsymbol{Q}},
#' where \eqn{\boldsymbol{Q}} is the diagonal matrix
#' of the inverse standard deviations,
#' as an elementwise product, without forming \eqn{\boldsymbol{Q}}.
#' No input checks: see `RhoofSigma()`.
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param x Numeric matrix.
#'   Covariance matrix (\eqn{\boldsymbol{\Sigma}}).
#' @param q Numeric vector.
#'   Inverse of the standard deviation vector,
#'   `1 / sqrt(diag(x))`.
#'
#' @return Returns a numeric matrix of the dimensions of `x`.
#'
#' @examples
#' sigmacap <- matrix(data = c(4, 1, 1, 9), nrow = 2)
#' .RhoofSigma(x = sigmacap, q = 1 / sqrt(diag(sigmacap)))
#'
#' @family Correlation Functions
#' @keywords rhoMatrix correlation internal
#' @noRd
.RhoofSigma <- function(x,
                        q) {
  q * x * rep(
    x = q,
    each = dim(x = x)[1]
  )
}
