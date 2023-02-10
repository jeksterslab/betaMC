#' Estimate Differences of Standardized Slopes
#' and Sampling Covariance Matrix
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @return Returns an object of class `diffbetamc`
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
#' diff <- DiffBetaMC(std)
#' # Methods -------------------------------------------------------
#' print(diff)
#' summary(diff)
#' coef(diff)
#' vcov(diff)
#' confint(diff, level = 0.95)
#' @family Beta Monte Carlo Functions
#' @keywords betaMC diff
#' @export
DiffBetaMC <- function(object) {
  stopifnot(
    methods::is(
      object,
      "betamc"
    )
  )
  if (object$lm_process$p < 2) {
    stop("Two or more regressors is required.")
  }
  est <- object$lm_process$dif_betastar
  p_dif <- dim(object$lm_process$dif_idx)[2]
  dif_betastar <- matrix(
    data = 0.0,
    ncol = p_dif,
    nrow = dim(object$thetahatstar)[1]
  )
  for (i in seq_len(p_dif)) {
    dif_betastar[, i] <- object$thetahatstar[
      ,
      object$lm_process$dif_idx[1, i]
    ] - object$thetahatstar[
      ,
      object$lm_process$dif_idx[2, i]
    ]
  }
  colnames(dif_betastar) <- names(object$lm_process$dif_betastar)
  out <- list(
    fit = object,
    thetahatstar = dif_betastar,
    vcov = stats::var(dif_betastar),
    est = est
  )
  out <- list(
    fit = object,
    thetahatstar = dif_betastar,
    vcov = stats::var(dif_betastar),
    est = est
  )
  class(out) <- c(
    "diffbetamc",
    class(out)
  )
  return(
    out
  )
}
