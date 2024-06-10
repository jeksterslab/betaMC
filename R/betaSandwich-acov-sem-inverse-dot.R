#' Inverse of The Asymptotic Covariance Matrix of the
#' Standardized Parameter Vector
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param jcap Numeric matrix.
#'   Jacobian matrix of the half-vectorization
#'   of the model-implied covariance matrix
#'   with respect to the standardized parameter vector.
#' @param acov Numeric matrix.
#'   Asymptotic covariance matrix of the sample covariance matrix.
#'
#' @family Beta Sandwich Functions
#' @keywords betaSandwich acov internal
#' @noRd
.ACovSEMInverse <- function(jcap,
                            acov) {
  return(
    t(jcap) %*% chol2inv(
      chol(acov)
    ) %*% jcap
  )
}
