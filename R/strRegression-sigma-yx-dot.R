#' Covariance Vector of Y and X
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param beta Numeric vector
#'   \eqn{\boldsymbol{\beta}}.
#'   Partial regression slopes.
#' @param sigmacapx Numeric matrix
#'   \eqn{\boldsymbol{\Sigma}_{\boldsymbol{X}, \boldsymbol{X}}}.
#'   Covariance matrix of
#'   \eqn{X_{1}, \dots, X_{j}, \dots, X_{p}}.
#'
#' @return Returns a numeric vector of the covariances of Y and the regressors.
#'
#' @examples
#' beta <- c(0.5, 0.3)
#' sigmacapx <- matrix(data = c(1, 0.2, 0.2, 2), nrow = 2)
#' sigmacap <- matrix(
#'   data = c(0.89, 0.56, 0.7, 0.56, 1, 0.2, 0.7, 0.2, 2),
#'   nrow = 3
#' )
#' strRegression:::.SigmaYX(beta = beta, sigmacapx = sigmacapx)
#'
#' @family Moments Functions
#' @keywords strRegression moments internal
#' @noRd
.SigmaYX <- function(beta,
                     sigmacapx) {
  .Vec(
    x = crossprod(
      x = beta,
      y = sigmacapx
    )
  )
}
