#' R-Squared as a Function
#' of the Covariance Matrix
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param sigmacap Numeric matrix.
#'   Covariance matrix of
#'   \eqn{\left\{ Y, X_{1}, \dots, X_{p} \right\}^{\prime}}.
#' @param k Positive integer.
#'   `p` regressors plus 1.
#'
#' @return Returns a numeric value, R-squared.
#'
#' @examples
#' beta <- c(0.5, 0.3)
#' sigmacapx <- matrix(data = c(1, 0.2, 0.2, 2), nrow = 2)
#' sigmacap <- matrix(
#'   data = c(0.89, 0.56, 0.7, 0.56, 1, 0.2, 0.7, 0.2, 2),
#'   nrow = 3
#' )
#' strRegression:::.RSqofSigma(sigmacap = sigmacap, k = 3)
#'
#' @family R-squared Functions
#' @keywords strRegression rsq internal
#' @noRd
.RSqofSigma <- function(sigmacap,
                        k) {
  1 - (
    det(x = sigmacap) / det(
      x = sigmacap[
        2:k,
        2:k,
        drop = FALSE
      ]
    )
  ) / sigmacap[1, 1]
}
