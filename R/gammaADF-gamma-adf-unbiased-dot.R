################################################################################
#' Asymptotic Covariance Matrix of the Sample Covariance Matrix
#' (Asymptotic Distribution Free - Unbiased)
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
          vechsigmacap_consistent
        )
      )
    )
  )
}
