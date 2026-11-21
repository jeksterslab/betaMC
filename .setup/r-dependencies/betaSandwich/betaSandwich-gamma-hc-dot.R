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
  # rows of v are vech(d_i d_i' - sigmacap), built for all observations at once
  idx <- which(
    x = lower.tri(
      x = sigmacap,
      diag = TRUE
    ),
    arr.ind = TRUE
  )
  v <- (
    d[
      ,
      idx[, 1],
      drop = FALSE
    ] * d[
      ,
      idx[, 2],
      drop = FALSE
    ]
  ) - rep(
    x = .Vech(
      x = sigmacap
    ),
    each = n
  )
  (
    1 / n
  ) * crossprod(
    x = v * qcap,
    y = v
  )
}
