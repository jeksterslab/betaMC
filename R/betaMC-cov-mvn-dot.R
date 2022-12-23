#' Sampling Covariance Matrix of the Parameter Vector
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param acov Numeric matrix.
#'   Asymptotic covariance matrix of the parameter vector.
#' @param n Integer.
#'   Sample size.
#'
#' @family Beta Monte Carlo Functions
#' @keywords betaMC cov internal
#' @noRd
.CovN <- function(acov,
                  n) {
  return(
    (
      1 / n
    ) * chol2inv(
      chol(acov)
    )
  )
}
