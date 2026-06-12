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
#' @param rsq Numeric.
#'   R-squared.
#'   If `rsq = NULL`, the kth element in `theta` is \eqn{R^{2}}.
#'   If `rsq = Numeric`, the kth element in `theta` is \eqn{\sigma^{2}}.
#' @param fixed_x Logical.
#'   If `fixed_x = TRUE`, treat the regressors as fixed.
#'   If `fixed_x = FALSE`, treat the regressors as random.
#'
#' @return Returns a matrix.
#' @family Derivatives Functions
#' @keywords strRegression derivatives internal
#' @noRd
.JacobianVechSigmaWRTTheta <- function(beta,
                                       sigmacapx,
                                       q,
                                       p,
                                       rsq = NULL,
                                       fixed_x = FALSE) {
  theta <- .ThetaIndex(
    p = p
  )
  moments <- .MomentsIndex(
    p = p
  )
  u <- 0.5 * p * (p + 1)
  dp <- .DMat(p)
  iden <- diag(p)
  if (fixed_x) {
    jcap <- matrix(
      data = 0.0,
      nrow = q,
      ncol = p + 1
    )
  } else {
    jcap <- matrix(
      data = 0.0,
      nrow = q,
      ncol = q
    )
  }
  rownames(jcap) <- c(
    moments$sigmaysq,
    moments$sigmayx,
    moments$vechsigmacapx
  )
  if (is.null(rsq)) {
    if (fixed_x) {
      colnames(jcap) <- c(
        theta$beta,
        theta$sigmasq
      )
    } else {
      colnames(jcap) <- c(
        theta$beta,
        theta$sigmasq,
        theta$vechsigmacapx
      )
    }
  } else {
    if (fixed_x) {
      colnames(jcap) <- c(
        theta$beta,
        "rsq"
      )
    } else {
      colnames(jcap) <- c(
        theta$beta,
        "rsq",
        theta$vechsigmacapx
      )
    }
  }
  jcap[
    moments$sigmaysq,
    theta$beta
  ] <- .Vec(
    2 * crossprod(
      beta,
      sigmacapx
    )
  )
  if (is.null(rsq)) {
    jcap[
      moments$sigmaysq,
      theta$sigmasq
    ] <- 1
  } else {
    jcap[
      moments$sigmaysq,
      "rsq"
    ] <- -(
      t(beta) %*% sigmacapx %*% beta
    ) / rsq^2
  }
  if (!fixed_x) {
    jcap[
      moments$sigmaysq,
      theta$vechsigmacapx
    ] <- .Vec(tcrossprod(beta)) %*% dp
  }
  jcap[
    moments$sigmayx,
    theta$beta
  ] <- sigmacapx
  if (!fixed_x) {
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
  }
  jcap
}
