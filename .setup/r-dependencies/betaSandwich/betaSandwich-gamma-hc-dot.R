#' Asymptotic Covariance Matrix of the Sample Covariance Matrix
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param d Numeric matrix.
#'   Centered data.
#' @param sigmacap Numeric matrix.
#'   Covariance matrix of \eqn{Y, X_1, \dots, X_p}.
#' @param qcap Numeric vector
#'   Leverage adjustment.
#' @param n Integer.
#'   Sample size.
#'
#' @family BetaSandwich Functions
#' @keywords betaSandwich gamma internal
#' @noRd
.GammaHC <- function(d,
                     sigmacap,
                     qcap,
                     n) {
  return(
    (
      1 / n
    ) * Reduce(
      f = "+",
      x = lapply(
        X = seq_len(n),
        FUN = function(i) {
          qcap[i] * tcrossprod(
            .Vech(
              tcrossprod(
                d[i, ]
              ) - sigmacap
            )
          )
        }
      )
    )
  )
}
