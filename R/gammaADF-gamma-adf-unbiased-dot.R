#' Asymptotic Covariance Matrix of the Sample Covariance Matrix
#' (Asymptotic Distribution Free - Unbiased)
#'
#' Returns
#' \deqn{
#'   \hat{\boldsymbol{\Gamma}}_{\mathrm{ADF}}^{\mathrm{unbiased}} =
#'   \frac{n (n - 1)}{(n - 2) (n - 3)}
#'   \hat{\boldsymbol{\Gamma}}_{\mathrm{ADF}}
#'   -
#'   \frac{n}{(n - 2) (n - 3)}
#'   \left(
#'     \hat{\boldsymbol{\Gamma}}_{\mathrm{MVN}}
#'     -
#'     \frac{2}{n - 1}
#'     \operatorname{vech} \! \left( \hat{\boldsymbol{\Sigma}} \right)
#'     \operatorname{vech}^{\prime} \! \left( \hat{\boldsymbol{\Sigma}} \right)
#'   \right) .
#' }
#' No input checks. The result needs \eqn{n > 3}.
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param gammacapadf_consistent Numeric matrix.
#'   Consistent estimate of the asymptotic distribution-free covariance matrix.
#' @param gammacapmvn_consistent Numeric matrix.
#'   Asymptotic covariance matrix
#'   of the consistent estimator of the sample covariance
#'   assuming multivariate normal distribution.
#' @param vechsigmacap_consistent Numeric vector.
#'   Half-vectorization of the consistent covariance matrix.
#' @param n Integer.
#'   Sample size.
#'
#' @return Returns a numeric matrix.
#'   Same dimensions as `gammacapadf_consistent`.
#'
#' @examples
#' k <- 3
#' q <- k * (k + 1) / 2
#' .GammaADFUnbiased(
#'   gammacapadf_consistent = diag(x = q),
#'   gammacapmvn_consistent = diag(x = q),
#'   vechsigmacap_consistent = rep(x = 0.5, times = q),
#'   n = 20
#' )
#'
#' @family gammaADF Functions
#' @keywords gammaADF gamma internal
#' @noRd
.GammaADFUnbiased <- function(gammacapadf_consistent,
                              gammacapmvn_consistent,
                              vechsigmacap_consistent,
                              n) {
  (
    (
      (
        n * (n - 1)
      ) / (
        (n - 2) * (n - 3)
      )
    ) * gammacapadf_consistent
  ) - (
    (
      n / (
        (n - 2) * (n - 3)
      )
    ) * (
      gammacapmvn_consistent - (
        (
          2 / (n - 1)
        ) * tcrossprod(
          x = vechsigmacap_consistent
        )
      )
    )
  )
}
