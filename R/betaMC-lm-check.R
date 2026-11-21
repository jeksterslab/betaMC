#' Check the Fitted Linear Model
#'
#' Stop if the `lm` object cannot be handled:
#' a model without an intercept, with case weights, or with an offset.
#' These models are not supported, and the results would be wrong.
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param object Object of class `lm`.
#'
#' @family Beta Monte Carlo Functions
#' @keywords betaMC check internal
#' @noRd
.CheckLM <- function(object) {
  stopifnot(
    inherits(
      x = object,
      what = "lm"
    )
  )
  if (attr(x = stats::terms(x = object), which = "intercept") == 0L) {
    stop("The model must include an intercept.")
  }
  if (!is.null(x = object$weights)) {
    stop("Models fitted with weights are not supported.")
  }
  if (!is.null(x = object$offset)) {
    stop("Models fitted with an offset are not supported.")
  }
  invisible(x = TRUE)
}
