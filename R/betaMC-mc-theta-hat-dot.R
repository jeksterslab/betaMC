#' List of Parameter Estimates and Model-Implied Covariance Matrix
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param thetahat Numeric vector.
#'   Parameter estimates.
#' @param p Positive integer.
#'   `p` regressors.
#' @param k Positive integer.
#'   `p` regressors plus 1.
#' @param q Positive integer.
#'   Length of `thetahat`.
#'
#' @return Returns a list of parameter estimates
#'   and model-implied covariance matrix.
#'
#' @family Beta Monte Carlo Functions
#' @keywords betaMC mc internal
#' @noRd
.MCThetaHat <- function(thetahat,
                        p,
                        k,
                        q) {
  beta <- thetahat[1:p]
  sigmasq <- thetahat[k]
  vechsigmacapx <- thetahat[
    (k + 1):q
  ]
  sigmacapx <- .SymofVech(
    x = vechsigmacapx,
    k = p
  )
  sigmaysq <- NA
  sigmayx <- NA
  sigmacap <- NA
  pd <- FALSE
  if (sigmasq > 0 & all(diag(sigmacapx) > 0)) {
    sigmaysq <- .SigmaYSq(
      beta = beta,
      sigmasq = sigmasq,
      sigmacapx = sigmacapx
    )
    if (sigmaysq > 0) {
      sigmayx <- .SigmaYX(
        beta = beta,
        sigmacapx = sigmacapx
      )
      sigmacap <- matrix(
        data = 0.0,
        nrow = k,
        ncol = k
      )
      sigmacap[1, 1] <- sigmaysq
      sigmacap[
        1,
        2:k
      ] <- sigmacap[
        2:k,
        1
      ] <- sigmayx
      sigmacap[
        2:k,
        2:k
      ] <- sigmacapx
      pd <- .TestPositiveDefinite2(sigmacap)
    }
  }
  return(
    list(
      coef = beta,
      sigmasq = sigmasq,
      vechsigmacapx = vechsigmacapx,
      sigmacapx = sigmacapx,
      sigmaysq = sigmaysq,
      sigmayx = sigmayx,
      sigmacap = sigmacap,
      pd = pd
    )
  )
}
