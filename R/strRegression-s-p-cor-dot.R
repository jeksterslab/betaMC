#' Semipartial Correlation
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param betastar Numeric vector.
#'   Standardized regression slopes.
#' @param sigmacapx Numeric matrix.
#'   Covariance matrix of
#'   \eqn{\left\{ X_{1}, \dots, X_{p} \right\}^{\prime}}.
#'
#' @family Semipartial Correlation Functions
#' @keywords strRegression spcor internal
#' @noRd
.SPCor <- function(betastar,
                   sigmacapx) {
  return(
    betastar * sqrt(
      1 / diag(
        chol2inv(
          chol(
            .RhoofSigma(
              x = sigmacapx,
              q = 1 / sqrt(diag(sigmacapx))
            )
          )
        )
      )
    )
  )
}
