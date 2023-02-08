#' Differences of Regression Slopes
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param object Object used to select a method.
#' @param ... additional arguments.
#' @export
#' @keywords methods
dif <- function(object,
                ...) {
  UseMethod("dif")
}
