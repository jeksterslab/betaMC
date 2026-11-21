#' Asymptotic Covariance Matrix of the Sample Covariance Matrix
#' (Multivariate Normal Distribution) (Internal)
#'
#' Returns
#' \eqn{\boldsymbol{\Gamma} = 2 \boldsymbol{D}_{k}^{+}
#' \left( \boldsymbol{\Sigma} \otimes \boldsymbol{\Sigma} \right)
#' \boldsymbol{D}_{k}^{+ \prime}},
#' the asymptotic covariance matrix of
#' \eqn{\sqrt{n} \operatorname{vech} \! \left( \boldsymbol{S} \right)}
#' under the multivariate normal distribution.
#' No input checks.
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param sigmacap Numeric matrix.
#'   Covariance matrix \eqn{\boldsymbol{\Sigma}}.
#' @param pinv_of_dcap Numeric matrix.
#'   Moore-Penrose inverse of the duplication matrix
#'   \eqn{\boldsymbol{D}_{k}^{+}}.
#'
#' @return Returns a numeric \eqn{k (k + 1) / 2 \times k (k + 1) / 2} matrix.
#'
#' @family gammaN Functions
#' @keywords gammaN gamma internal
#' @noRd
.GammaN <- function(sigmacap,
                    pinv_of_dcap) {
  2 * pinv_of_dcap %*% (
    tcrossprod(
      x = kronecker(
        X = sigmacap,
        Y = sigmacap
      ),
      y = pinv_of_dcap
    )
  )
}
