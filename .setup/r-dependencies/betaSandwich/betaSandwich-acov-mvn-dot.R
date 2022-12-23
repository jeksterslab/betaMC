#' Asymptotic Covariance Matrix of the Standardized Parameter Vector
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param jcap Numeric matrix.
#'   Jacobian matrix of the half-vectorization
#'   of the model-implied covariance matrix
#'   with respect to the standardized parameter vector.
#' @param gammacap_mvn Numeric matrix.
#'   Asymptotic covariance matrix of the sample covariance matrix
#'   assuming multivariate normal distribution.
#'
#' @family Beta Sandwich Functions
#' @keywords betaSandwich acov internal
#' @noRd
.ACovN <- function(jcap,
                   gammacap_mvn) {
  return(
    t(jcap) %*% chol2inv(
      chol(gammacap_mvn)
    ) %*% jcap
  )
}
