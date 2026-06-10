#' Asymptotic Covariance Matrix of the
#' Standardized Parameter Vector
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param jcap Numeric matrix.
#'   Jacobian matrix of the half-vectorization
#'   of the model-implied covariance matrix
#'   with respect to the standardized parameter vector.
#' @param gammacap Numeric matrix.
#'   Adjusted asymptotic covariance matrix.
#' @param gammacap_mvn Numeric matrix.
#'   Asymptotic covariance matrix of the sample covariance matrix
#'   assuming multivariate normal distribution.
#'
#' @family Beta Sandwich Functions
#' @keywords betaSandwich acov internal
#' @noRd
.ACovHC <- function(jcap,
                    gammacap,
                    gammacap_mvn) {
  inversemvn <- chol2inv(
    chol(gammacap_mvn)
  )
  tjcapinversemvn <- t(jcap) %*% inversemvn
  bread <- chol2inv(
    chol(tjcapinversemvn %*% jcap)
  )
  meat <- tjcapinversemvn %*% gammacap %*% inversemvn %*% jcap
  bread %*% meat %*% bread
}
