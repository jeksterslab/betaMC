#' Estimate Squared Semipartial Correlation Coefficients
#' and the Corresponding Confidence Intervals
#' From the Signed Intervals
#'
#' Square the signed interval as a set, with lower limit zero if it
#' contains zero.
#' This differs from an interval of squared draws.
#' No draws are generated and no endpoints are clipped to one.
#' Failed signed endpoints remain missing.
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @details
#' The semipartial correlation \eqn{sr_{j}} (see [SCorMC()]) is signed,
#' and its confidence interval \eqn{\left[ l, u \right]} may contain zero.
#' The interval for \eqn{sr^{2}_{j}} is the image of that interval under
#' squaring.
#' The squared interval is
#' \eqn{\left[ l^{2}, u^{2} \right]} if \eqn{0 < l},
#' \eqn{\left[ u^{2}, l^{2} \right]} if \eqn{u < 0},
#' and \eqn{\left[ 0, \max \left( l^{2}, u^{2} \right) \right]}
#' if the signed interval contains zero.
#' This is not the interval of squared draws, and it is not an interval for
#' [DeltaRSqMC()].
#'
#' @return Data frame with est, lower and upper columns and predictor
#'   row names.
#'   The point estimate is the squared signed estimate.
#'   No standard error is implied by this interval transformation.
#'
#' @param object Context accepted by [SCorMC()].
#' @param level Single confidence level in (0, 1).
#'
#' @examples
#' # Data ---------------------------------------------------------------------
#' data("nas1982", package = "betaMC")
#'
#' # Fit Model in lm ----------------------------------------------------------
#' object <- stats::lm(
#'   formula = QUALITY ~ NARTIC + PCTGRT + PCTSUPP,
#'   data = nas1982
#' )
#'
#' # MC -----------------------------------------------------------------------
#' mc <- MC(
#'   object = object,
#'   R = 100, # use a large value e.g., 20000L for actual research
#'   seed = 0508
#' )
#'
#' # SCorSqMC -----------------------------------------------------------------
#' SCorSqMC(object = mc, level = 0.95)
#'
#' @family Beta Monte Carlo Functions
#' @keywords betaMC scor
#' @export
SCorSqMC <- function(object, level = 0.95) {
  bad_level <- !is.numeric(x = level) || length(x = level) != 1L ||
    !is.finite(x = level) || level <= 0 || level >= 1
  if (bad_level) {
    stop("level must be a single confidence level in (0, 1).")
  }
  signed <- SCorMC(object = object, alpha = 1 - level)
  ci <- stats::confint(object = signed, level = level)
  .SCorSqCI(est = stats::coef(object = signed), ci = ci)
}
