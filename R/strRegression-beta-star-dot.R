#' Standardized Partial Regression Slopes
#'
#' Calculate standardized partial regression slopes.
#'
#' @details The vector of standardized partial regression slopes
#'   is given by
#'   \deqn{
#'     \boldsymbol{\beta}^{\ast}
#'     =
#'     \sigma_{Y}^{-1}
#'     \boldsymbol{\sigma}_{\mathbf{X}}
#'     \boldsymbol{\beta} .
#'   }
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param beta Numeric vector
#'   \eqn{\boldsymbol{\beta}}.
#'   Partial regression slopes.
#' @param sigmay Numeric.
#'   \eqn{\sigma_{Y}}.
#'   Standard deviation of \eqn{Y}.
#' @param sigmax Numeric vector.
#'   \eqn{\boldsymbol{\sigma}_{\mathbf{X}}}.
#'   Standard deviation of
#'   \eqn{X_{1}, \dots, X_{j}, \dots, X_{p}}.
#'
#' @return Returns a vector.
#' @family Standardized Slopes Functions
#' @keywords strRegression slopesstd internal
#' @noRd
.BetaStar <- function(beta,
                      sigmay,
                      sigmax) {
  .Vec(
    (
      sigmax / sigmay
    ) * beta
  )
}
