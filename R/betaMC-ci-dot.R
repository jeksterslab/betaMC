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
                alpha = NULL) {
  if (is.null(x = alpha)) {
    alpha <- object$args$alpha
  }
  probs <- .PCProbs(alpha = alpha)
  thetahatstar <- unname(
    obj = as.matrix(x = object$thetahatstar)
  )
  colnames(x = thetahatstar) <- names(x = object$est)
  thetahat <- object$est
  ci <- vector(
    mode = "list",
    length = dim(x = thetahatstar)[2]
  )
  for (i in seq_len(length.out = dim(x = thetahatstar)[2])) {
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
  rownames(x = ci) <- names(x = thetahat)
  ci
}
