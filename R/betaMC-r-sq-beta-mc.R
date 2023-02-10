#' Estimate Differences of Standardized Slopes
#' and Sampling Covariance Matrix
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @return Returns an object of class `rsqbetamc`
#'   which is a list with the following elements:
#'   \describe{
#'     \item{fit}{The argument `object`.}
#'     \item{thetahatstar}{Sampling distribution.}
#'     \item{vcov}{Sampling covariance matrix of
#'       differences of standardized slopes.}
#'     \item{est}{Vector of
#'       differences of standardized slopes.}
#'   }
#'
#' @param object Object of class `betamc`,
#'   that is,
#'   the output of the `BetaMC()` function.
#'
#' @examples
#' object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
#' std <- BetaMC(object)
#' rsq <- RSqBetaMC(std)
#' # Methods -------------------------------------------------------
#' print(rsq)
#' summary(rsq)
#' coef(rsq)
#' vcov(rsq)
#' confint(rsq, level = 0.95)
#' @family Beta Monte Carlo Functions
#' @keywords betaMC rsq
#' @export
RSqBetaMC <- function(object) {
  stopifnot(
    methods::is(
      object,
      "betamc"
    )
  )
  thetahatstar <- object$thetahatstar[
    ,
    (object$lm_process$k):(object$lm_process$k + 1)
  ]
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
