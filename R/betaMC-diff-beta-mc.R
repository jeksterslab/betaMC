#' Estimate Differences of Standardized Slopes
#' and Generate the Corresponding Sampling Distribution
#' Using the Monte Carlo Method
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @return Returns an object
#' of class `diffbetamc` which is a list with the following elements:
#'   \describe{
#'     \item{fit}{The argument `object`.}
#'     \item{thetahatstar}{Sampling distribution of
#'       differences of standardized slopes.}
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
#' std <- BetaMC(object, R = 100)
#' # use a large R, for example, R = 20000 for actual research
#' diff <- DiffBetaMC(std)
#' # Methods -------------------------------------------------------
#' print(diff)
#' summary(diff)
#' coef(diff)
#' vcov(diff)
#' confint(diff, level = 0.95)
#' @export
#' @family Beta Monte Carlo Functions
#' @keywords betaMC diff
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
    nrow = dim(object$thetahatstar_std)[1]
  )
  for (i in seq_len(p_dif)) {
    dif_betastar[, i] <- object$thetahatstar_std[
      ,
      object$lm_process$dif_idx[1, i]
    ] - object$thetahatstar_std[
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
