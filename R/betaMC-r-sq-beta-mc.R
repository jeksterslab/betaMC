#' Estimate Multiple Correlation Coefficients
#' (R-Squared and Adjusted R-Squared)
#' and Generate the Corresponding Sampling Distribution
#' Using the Monte Carlo Method
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @return Returns an object of class `rsqbetamc`
#'   which is a list with the following elements:
#'   \describe{
#'     \item{fit}{The argument `object`.}
#'     \item{thetahatstar}{Sampling distribution of
#'       multiple correlation coefficients
#'       (R-squared and adjusted R-squared).}
#'     \item{vcov}{Sampling covariance matrix of
#'       multiple correlation coefficients
#'       (R-squared and adjusted R-squared).}
#'     \item{est}{Vector of
#'       multiple correlation coefficients
#'       (R-squared and adjusted R-squared).}
#'   }
#'
#' @param object Object of class `betamc`,
#'   that is,
#'   the output of the `BetaMC()` function.
#'
#' @examples
#' object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
#' std <- BetaMC(object, R = 100)
#' # use a large R, for example, R = 20000 for actual research
#' rsq <- RSqBetaMC(std)
#' # Methods -------------------------------------------------------
#' print(rsq)
#' summary(rsq)
#' coef(rsq)
#' vcov(rsq)
#' confint(rsq, level = 0.95)
#' @export
#' @family Beta Monte Carlo Functions
#' @keywords betaMC rsq
RSqBetaMC <- function(object) {
  stopifnot(
    methods::is(
      object,
      "betamc"
    )
  )
  rsq <- lapply(
    X = object$thetahatstar,
    FUN = function(x) {
      sigmacap <- matrix(
        data = 0.0,
        nrow = object$lm_process$k,
        ncol = object$lm_process$k
      )
      sigmacap[1, 1] <- x$sigmaysq
      sigmacap[
        1,
        2:object$lm_process$k
      ] <- sigmacap[
        2:object$lm_process$k,
        1
      ] <- .SigmaYX(
        beta = x$beta,
        sigmacapx = x$sigmacapx
      )
      sigmacap[
        2:object$lm_process$k,
        2:object$lm_process$k
      ] <- x$sigmacapx
      return(
        .RSqofSigma(
          sigmacap = sigmacap,
          k = object$lm_process$k
        )
      )
    }
  )
  rsq <- .Vec(
    do.call(
      what = "rbind",
      args = rsq
    )
  )
  adj <- (
    1 - (
      1 - rsq
    ) * (
      (
        object$lm_process$n - 1
      ) / object$lm_process$df
    )
  )
  thetahatstar <- cbind(
    rsq = rsq,
    adj = adj
  )
  est <- c(
    rsq = object$lm_process$summary_lm$r.squared,
    adj = object$lm_process$summary_lm$adj.r.squared
  )
  out <- list(
    fit = object,
    thetahatstar = thetahatstar,
    vcov = stats::var(thetahatstar),
    est = est
  )
  class(out) <- c(
    "rsqbetamc",
    class(out)
  )
  return(
    out
  )
}
