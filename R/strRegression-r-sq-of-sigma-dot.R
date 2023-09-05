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
#' @family R-squared Functions
#' @keywords strRegression rsq internal
#' @noRd
.RSqofSigma <- function(sigmacap,
                        k) {
  return(
    1 - (
      det(sigmacap) / det(
        sigmacap[
          2:k,
          2:k,
          drop = FALSE
        ]
      )
    ) / sigmacap[1, 1]
  )
}
