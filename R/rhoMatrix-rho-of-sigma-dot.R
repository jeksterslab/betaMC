#' Correlation Matrix from Covariance Matrix
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param x Numeric matrix.
#'   Covariance matrix.
#' @param q Numeric vector.
#'   Inverse of the standard deviation vector.
#' @return Returns a matrix.
#'
#' @family Correlation Functions
#' @keywords rhoMatrix correlation internal
#' @noRd
.RhoofSigma <- function(x,
                        q) {
  return(
    q * x * rep(
      x = q,
      each = dim(x)[1]
    )
  )
}
