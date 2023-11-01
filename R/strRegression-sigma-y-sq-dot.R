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
#'     \boldsymbol{\Sigma}_{\mathbf{X}, \mathbf{X}}
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
#'   \eqn{\boldsymbol{\Sigma}_{\mathbf{X}, \mathbf{X}}}.
#'   Covariance matrix of
#'   \eqn{X_{1}, \dots, X_{j}, \dots, X_{p}}.
#'
#' @family Moments Functions
#' @keywords strRegression moments internal
#' @noRd
.SigmaYSq <- function(beta,
                      sigmasq,
                      sigmacapx) {
  return(
    .Vec(
      sigmasq + crossprod(
        beta,
        sigmacapx
      ) %*% beta
    )
  )
}
