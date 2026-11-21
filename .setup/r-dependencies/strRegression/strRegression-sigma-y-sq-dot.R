#' Variance of Y
#'
#' Calculate the model-implied
#' variance of \eqn{Y}.
#'
#' @section Variance of Y:
#' The variance of \eqn{Y} is given by:
#' \deqn{
#'     \sigma_{Y}^{2}
#'     =
#'     \sigma^{2}
#'     +
#'     \boldsymbol{\beta}^{\prime}
#'     \boldsymbol{\Sigma}_{\boldsymbol{X}, \boldsymbol{X}}
#'     \boldsymbol{\beta}
#' }
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param beta Numeric vector
#'   \eqn{\boldsymbol{\beta}}.
#'   Partial regression slopes.
#' @param sigmasq Numeric vector of length 1
#'   \eqn{\sigma^{2}}.
#'   Error variance.
#' @param sigmacapx Numeric matrix
#'   \eqn{\boldsymbol{\Sigma}_{\boldsymbol{X}, \boldsymbol{X}}}.
#'   Covariance matrix of
#'   \eqn{X_{1}, \dots, X_{j}, \dots, X_{p}}.
#'
#' @return Returns a numeric value, the variance of Y.
#'
#' @examples
#' beta <- c(0.5, 0.3)
#' sigmacapx <- matrix(data = c(1, 0.2, 0.2, 2), nrow = 2)
#' sigmacap <- matrix(
#'   data = c(0.89, 0.56, 0.7, 0.56, 1, 0.2, 0.7, 0.2, 2),
#'   nrow = 3
#' )
#' strRegression:::.SigmaYSq(beta = beta, sigmasq = 0.4, sigmacapx = sigmacapx)
#'
#' @family Moments Functions
#' @keywords strRegression moments internal
#' @noRd
.SigmaYSq <- function(beta,
                      sigmasq,
                      sigmacapx) {
  .Vec(
    x = sigmasq + crossprod(
      x = beta,
      y = sigmacapx
    ) %*% beta
  )
}
