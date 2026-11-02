#' Selectively Standardized Partial Regression Slopes of Draws
#'
#' Applies `.BetaStarSelect()` to draws, taking the standard deviations from
#' the draws of
#' \eqn{\mathrm{vech} \left( \boldsymbol{\Sigma} \right)}.
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @inheritParams .BetaStarSelect
#' @param betastar Numeric matrix.
#'   Draws of the standardized slopes (rows).
#' @param vechsigmacap Numeric matrix.
#'   Draws of the half-vectorization of the covariance matrix of
#'   \eqn{\left( y, \mathbf{x} \right)} (rows).
#' @param k Positive integer.
#'   Number of variables, \eqn{p + 1}.
#' @return Returns a numeric matrix with the shape of `betastar`.
#'
#' @family Structure of Regression Functions
#' @keywords strRegression internal
#' @noRd
.BetaStarSelectofVechSigmaMat <- function(betastar,
                                          vechsigmacap,
                                          k,
                                          std_y = TRUE,
                                          std_x) {
  diags <- .DiagofVech(
    x = seq_len(ncol(vechsigmacap)),
    k = k,
    loc = FALSE
  )
  out <- .BetaStarSelect(
    betastar = betastar,
    sigmay = sqrt(vechsigmacap[, diags[1]]),
    sigmax = sqrt(vechsigmacap[, diags[-1], drop = FALSE]),
    std_y = std_y,
    std_x = std_x
  )
  colnames(out) <- colnames(betastar)
  out
}
