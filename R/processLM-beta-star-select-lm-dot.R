#' Selectively Standardized Partial Regression Slopes of a Fitted Model
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @inheritParams .BetaStarSelect
#' @param lm_process List.
#'   Output of `.ProcessLM()`.
#' @return Returns a named numeric vector.
#'
#' @family Process LM Functions
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
  names(out) <- lm_process$xnames
  out
}
