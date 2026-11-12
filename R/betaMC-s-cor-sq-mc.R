#' Squared Signed-Semipartial Confidence Intervals
#'
#' Square the signed interval as a set, with lower limit zero if it
#' contains zero. This differs from an interval of squared draws.
#' No draws are generated and no endpoints are clipped to one.
#' Failed signed endpoints remain missing.
#' @author Ivan Jacob Agaloos Pesigan
#' @param object Context accepted by [SCorMC()].
#' @param level Single confidence level in (0, 1).
#' @return Data frame with est, lower and upper columns and predictor
#'   row names. The point estimate is the squared signed estimate.
#'   No standard error is implied by this interval transformation.
#' @examples
#' fit <- stats::lm(formula = mpg ~ wt + hp, data = mtcars)
#' out <- MC(object = fit, R = 199)
#' SCorSqMC(object = out)
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
