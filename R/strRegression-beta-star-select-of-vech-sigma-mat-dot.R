#' Selectively Standardized Partial Regression Slopes of Draws
#'
#' Applies `.BetaStarSelect()` to draws, taking the standard deviations from
#' the draws of
#' \eqn{\operatorname{vech} \left( \boldsymbol{\Sigma} \right)}.
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @inheritParams .BetaStarSelect
#' @param betastar Numeric matrix.
#'   Draws of the standardized slopes (rows).
#' @param vechsigmacap Numeric matrix.
#'   Draws of the half-vectorization of the covariance matrix of
#'   \eqn{\left( y, \boldsymbol{x} \right)} (rows).
#' @param k Positive integer.
#'   Number of variables, \eqn{p + 1}.
#' @return Returns a numeric matrix with the shape of `betastar`.
#'
#' @examples
#' beta <- c(0.5, 0.3)
#' sigmacapx <- matrix(data = c(1, 0.2, 0.2, 2), nrow = 2)
#' sigmacap <- matrix(
#'   data = c(0.89, 0.56, 0.7, 0.56, 1, 0.2, 0.7, 0.2, 2),
#'   nrow = 3
#' )
#' vechsigmacap <- rbind(sigmacap[lower.tri(sigmacap, diag = TRUE)])
#' betastar <- rbind(c(x1 = 0.4, x2 = 0.3))
#' strRegression:::.BetaStarSelectofVechSigmaMat(
#'   betastar = betastar,
#'   vechsigmacap = vechsigmacap,
#'   k = 3,
#'   std_y = FALSE,
#'   std_x = c(TRUE, FALSE)
#' )
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
    x = seq_len(length.out = ncol(x = vechsigmacap)),
    k = k,
    loc = FALSE
  )
  out <- .BetaStarSelect(
    betastar = betastar,
    sigmay = sqrt(x = vechsigmacap[, diags[1]]),
    sigmax = sqrt(x = vechsigmacap[, diags[-1], drop = FALSE]),
    std_y = std_y,
    std_x = std_x
  )
  colnames(x = out) <- colnames(x = betastar)
  out
}
