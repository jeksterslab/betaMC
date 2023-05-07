#' Confidence Intervals
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @return Returns a matrix of
#'   estimates,
#'   standard errors,
#'   and
#'   confidence intervals.
#'
#' @param object Object of class `betamc`.
#' @param alpha Numeric vector.
#'   Significance level.
#'
#' @family Beta Monte Carlo Functions
#' @keywords betaMC ci internal
#' @noRd
.CI <- function(object,
                alpha = c(0.05, 0.01, 0.001)) {
  stopifnot(
    inherits(
      object,
      "betamc"
    )
  )
  probs <- .PCProbs(alpha = alpha)
  thetahatstar <- do.call(
    what = "rbind",
    args = object$thetahatstar
  )
  thetahatstar <- unname(thetahatstar)
  colnames(thetahatstar) <- names(object$est)
  thetahat <- object$est
  ci <- vector(
    mode = "list",
    length = dim(thetahatstar)[2]
  )
  for (i in seq_len(dim(thetahatstar)[2])) {
    ci[[i]] <- .PCCI(
      thetahatstar = thetahatstar[, i],
      thetahat = thetahat[[i]],
      probs = probs
    )
  }
  ci <- do.call(
    what = "rbind",
    args = ci
  )
  rownames(ci) <- names(thetahat)
  return(
    ci
  )
}
