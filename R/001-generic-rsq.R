#' Estimate Coefficient of Determination
#' (R-Squared and Adjusted R-Squared)
#' and Sampling Covariance Matrix
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param object Object used to select a method.
#' @param ... additional arguments.
#' @export
#' @keywords methods
rsq <- function(object,
                ...) {
  UseMethod("rsq")
}
