#' Confidence Intervals for
#' Differences of Standardized Regression Coefficients
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @return Returns a matrix of
#'   differences of standardized regression slopes,
#'   standard errors,
#'   and
#'   confidence intervals.
#'
#' @param object Object of class `difbetamc`.
#' @param alpha Numeric vector.
#'   Significance level.
#'
#' @family Beta Monte Carlo Functions
#' @keywords betaMC dif ci internal
#' @noRd
.DiffBetaCI <- function(object,
                        alpha = c(0.05, 0.01, 0.001)) {
  stopifnot(
    methods::is(
      object,
      "diffbetamc"
    )
  )
  thetahatstar <- object$thetahatstar
  thetahat <- object$est
  ci <- vector(
    mode = "list",
    length = dim(thetahatstar)[2]
  )
  for (i in seq_len(dim(thetahatstar)[2])) {
    ci[[i]] <- .PCCI(
      thetahatstar = thetahatstar[, i],
      thetahat = thetahat[[i]],
      alpha = alpha
    )
  }
  ci <- do.call(
    what = "rbind",
    args = ci
  )
  rownames(ci) <- colnames(thetahatstar)
  return(
    ci
  )
}
