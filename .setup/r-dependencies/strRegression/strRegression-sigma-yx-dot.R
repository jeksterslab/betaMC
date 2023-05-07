#' Covariance Vector of Y and X
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param beta Numeric vector
#'   \eqn{\boldsymbol{\beta}}.
#'   Partial regression slopes.
#' @param sigmacapx Numeric matrix
#'   \eqn{\boldsymbol{\Sigma}_{\mathbf{X}, \mathbf{X}}}.
#'   Covariance matrix of
#'   \eqn{X_{1}, \dots, X_{j}, \dots, X_{p}}.
#'
#' @family Moments Functions
#' @keywords strRegression moments internal
#' @noRd
.SigmaYX <- function(beta,
                     sigmacapx) {
  return(
    .Vec(
      crossprod(
        beta,
        sigmacapx
      )
    )
  )
}
