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
#' @return Returns a numeric vector of the semipartial correlations.
#'
#' @examples
#' beta <- c(0.5, 0.3)
#' sigmacapx <- matrix(data = c(1, 0.2, 0.2, 2), nrow = 2)
#' sigmacap <- matrix(
#'   data = c(0.89, 0.56, 0.7, 0.56, 1, 0.2, 0.7, 0.2, 2),
#'   nrow = 3
#' )
#' strRegression:::.SPCor(betastar = c(0.4, 0.3), sigmacapx = sigmacapx)
#'
#' @family Semipartial Correlation Functions
#' @keywords strRegression spcor internal
#' @noRd
.SPCor <- function(betastar,
                   sigmacapx) {
  betastar * sqrt(
    x = 1 / diag(
      x = chol2inv(
        x = chol(
          x = .RhoofSigma(
            x = sigmacapx,
            q = 1 / sqrt(x = diag(x = sigmacapx))
          )
        )
      )
    )
  )
}
