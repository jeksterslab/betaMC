#' Jacobian Matrix of the Half-Vectorization
#' of the Model-Implied Covariance Matrix
#' with Respect to the Parameter Vector
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param beta Numeric vector.
#'   Partial regression slopes.
#' @param sigmacapx Numeric matrix.
#'   Covariance matrix of the regressor variables.
#' @param q Positive integer.
#'   Length of the parameter vector.
#' @param p Positive integer.
#'   `p` regressors.
#'
#' @return Returns a matrix.
#' @family Derivatives Functions
#' @keywords strRegression derivatives internal
#' @noRd
.JacobianVechSigmaWRTTheta <- function(beta,
                                       sigmacapx,
                                       q,
                                       p) {
  theta <- .ThetaIndex(
    p = p
  )
  moments <- .MomentsIndex(
    p = p
  )
  u <- 0.5 * p * (p + 1)
  dp <- .DMat(p)
  iden <- diag(p)
  jcap <- matrix(
    data = 0,
    nrow = q,
    ncol = q
  )
  rownames(jcap) <- c(
    moments$sigmaysq,
    moments$sigmayx,
    moments$vechsigmacapx
  )
  colnames(jcap) <- c(
    theta$beta,
    theta$sigmasq,
    theta$vechsigmacapx
  )
  jcap[
    moments$sigmaysq,
    theta$beta
  ] <- .Vec(
    2 * crossprod(
      beta,
      sigmacapx
    )
  )
  jcap[
    moments$sigmaysq,
    theta$sigmasq
  ] <- 1
  jcap[
    moments$sigmaysq,
    theta$vechsigmacapx
  ] <- .Vec(tcrossprod(beta)) %*% dp
  jcap[
    moments$sigmayx,
    theta$beta
  ] <- sigmacapx
  jcap[
    moments$sigmayx,
    theta$vechsigmacapx
  ] <- kronecker(
    t(beta),
    iden
  ) %*% dp
  jcap[
    moments$vechsigmacapx,
    theta$vechsigmacapx
  ] <- diag(u)
  return(
    jcap
  )
}
