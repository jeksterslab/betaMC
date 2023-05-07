#' Asymptotic Covariance Matrix of the Sample Covariance Matrix
#' (Asymptotic Distribution Free - Consistent)
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param d Numeric matrix.
#'   Deviation scores.
#' @param vechsigmacap_consistent Numeric vector.
#'   Half-vectorization of the consistent covariance matrix.
#' @param n Integer.
#'   Sample size.
#'
#' @family gammaADF Functions
#' @keywords gammaADF gamma internal
#' @noRd
.GammaADFConsistent <- function(d,
                                vechsigmacap_consistent,
                                n) {
  return(
    (
      (1 / n) * (
        Reduce(
          f = `+`,
          x = lapply(
            X = 1:n,
            FUN = function(i,
                           d) {
              tcrossprod(
                .Vech(
                  tcrossprod(d[i, ])
                )
              )
            },
            d = d
          )
        )
      )
    ) - tcrossprod(
      vechsigmacap_consistent
    )
  )
}
