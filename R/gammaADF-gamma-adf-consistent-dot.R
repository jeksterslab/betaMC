#' Asymptotic Covariance Matrix of the Sample Covariance Matrix
#' (Asymptotic Distribution Free - Consistent)
#'
#' Returns
#' \deqn{
#'   \hat{\boldsymbol{\Gamma}}_{\mathrm{ADF}} =
#'   \frac{1}{n}
#'   \sum_{i = 1}^{n}
#'   \operatorname{vech} \! \left(
#'     \boldsymbol{d}_i \boldsymbol{d}_i^{\prime}
#'   \right)
#'   \operatorname{vech}^{\prime} \! \left(
#'     \boldsymbol{d}_i \boldsymbol{d}_i^{\prime}
#'   \right)
#'   -
#'   \operatorname{vech} \! \left( \hat{\boldsymbol{\Sigma}} \right)
#'   \operatorname{vech}^{\prime} \! \left( \hat{\boldsymbol{\Sigma}} \right) ,
#' }
#' where \eqn{\boldsymbol{d}_i} is row \eqn{i} of the deviation scores
#' and \eqn{\hat{\boldsymbol{\Sigma}}} is the consistent (divisor \eqn{n})
#' sample covariance matrix.
#' No input checks.
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param d Numeric matrix.
#'   Deviation scores, \eqn{n \times k}.
#' @param vechsigmacap_consistent Numeric vector.
#'   Half-vectorization of the consistent covariance matrix,
#'   length \eqn{k (k + 1) / 2}.
#' @param n Integer.
#'   Sample size.
#'
#' @return Returns a \eqn{k (k + 1) / 2 \times k (k + 1) / 2} numeric matrix.
#'
#' @examples
#' set.seed(42)
#' x <- matrix(data = stats::rnorm(n = 60), nrow = 20)
#' d <- scale(x = x, center = TRUE, scale = FALSE)
#' sigma <- crossprod(x = d) / 20
#' .GammaADFConsistent(
#'   d = d,
#'   vechsigmacap_consistent = .Vech(x = sigma),
#'   n = 20
#' )
#'
#' @family gammaADF Functions
#' @keywords gammaADF gamma internal
#' @noRd
.GammaADFConsistent <- function(d,
                                vechsigmacap_consistent,
                                n) {
  # rows of v are vech(d_i d_i'), built for all observations at once
  idx <- which(
    x = lower.tri(
      x = diag(
        nrow = ncol(x = d)
      ),
      diag = TRUE
    ),
    arr.ind = TRUE
  )
  v <- d[
    ,
    idx[, 1],
    drop = FALSE
  ] * d[
    ,
    idx[, 2],
    drop = FALSE
  ]
  (
    (1 / n) * crossprod(x = v)
  ) - tcrossprod(
    x = vechsigmacap_consistent
  )
}
