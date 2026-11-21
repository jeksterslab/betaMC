#' Selectively Standardized Partial Regression Slopes of a Fitted Model
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param lm_process List.
#'   Output of `.ProcessLM()`.
#' @param std_y Logical.
#'   If `std_y = TRUE`, standardize the dependent variable.
#' @param std_x Logical vector.
#'   `TRUE` for each regressor to standardize.
#' @return Returns a named numeric vector.
#'
#' @family Process lm Functions
#' @keywords processLM internal
#' @noRd
.BetaStarSelectLM <- function(lm_process,
                              std_y = TRUE,
                              std_x) {
  out <- .BetaStarSelect(
    betastar = lm_process$betastar,
    sigmay = lm_process$sigma[1],
    sigmax = lm_process$sigma[-1],
    std_y = std_y,
    std_x = std_x
  )
  names(x = out) <- lm_process$xnames
  out
}
