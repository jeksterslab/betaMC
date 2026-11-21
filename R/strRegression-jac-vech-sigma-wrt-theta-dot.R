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
#' @examples
#' beta <- c(0.5, 0.3)
#' sigmacapx <- matrix(data = c(1, 0.2, 0.2, 2), nrow = 2)
#' sigmacap <- matrix(
#'   data = c(0.89, 0.56, 0.7, 0.56, 1, 0.2, 0.7, 0.2, 2),
#'   nrow = 3
#' )
#' strRegression:::.JacVechSigmaWRTTheta(
#'   beta = beta,
#'   sigmacapx = sigmacapx,
#'   q = 6,
#'   p = 2
#' )
#'
#' @family Derivatives Functions
#' @keywords strRegression derivatives internal
#' @noRd
.JacVechSigmaWRTTheta <- function(beta,
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
  dp <- .DMat(k = p)
  iden <- diag(x = p)
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
  rownames(x = jcap) <- c(
    moments$sigmaysq,
    moments$sigmayx,
    moments$vechsigmacapx
  )
  if (is.null(x = rsq)) {
    if (fixed_x) {
      colnames(x = jcap) <- c(
        theta$beta,
        theta$sigmasq
      )
    } else {
      colnames(x = jcap) <- c(
        theta$beta,
        theta$sigmasq,
        theta$vechsigmacapx
      )
    }
  } else {
    if (fixed_x) {
      colnames(x = jcap) <- c(
        theta$beta,
        "rsq"
      )
    } else {
      colnames(x = jcap) <- c(
        theta$beta,
        "rsq",
        theta$vechsigmacapx
      )
    }
  }
  # with theta = (beta, rsq, vech(sigmacapx)),
  # sigmaysq = beta' sigmacapx beta / rsq,
  # so its derivatives with respect to beta and vech(sigmacapx)
  # are divided by rsq
  if (is.null(x = rsq)) {
    scale <- 1
  } else {
    scale <- 1 / as.vector(x = rsq)[1]
  }
  jcap[
    moments$sigmaysq,
    theta$beta
  ] <- scale * .Vec(
    x = 2 * crossprod(
      x = beta,
      y = sigmacapx
    )
  )
  if (is.null(x = rsq)) {
    jcap[
      moments$sigmaysq,
      theta$sigmasq
    ] <- 1
  } else {
    jcap[
      moments$sigmaysq,
      "rsq"
    ] <- -(
      t(x = beta) %*% sigmacapx %*% beta
    ) / rsq^2
  }
  if (!fixed_x) {
    jcap[
      moments$sigmaysq,
      theta$vechsigmacapx
    ] <- scale * .Vec(x = tcrossprod(x = beta)) %*% dp
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
      X = t(x = beta),
      Y = iden
    ) %*% dp
    jcap[
      moments$vechsigmacapx,
      theta$vechsigmacapx
    ] <- diag(x = u)
  }
  jcap
}
