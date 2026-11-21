#' Cohen's f-Squared and Local f-Squared of a Fitted Model
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param lm_process List.
#'   Output of `.ProcessLM()`.
#' @return Returns a named numeric vector:
#'   `fsq` followed by the local \eqn{f^2} of each regressor.
#'
#' @family Process lm Functions
#' @keywords processLM internal
#' @noRd
.FSqLM <- function(lm_process) {
  es <- .StrRegressionofVechSigmaMat(
    vechsigmacap = matrix(
      data = .Vech(x = lm_process$sigmacap),
      nrow = 1
    ),
    k = lm_process$k
  )
  colnames(x = es$spcor) <- lm_process$xnames
  out <- .FSq(
    rsq = es$rsq,
    spcor = es$spcor
  )
  stats::setNames(
    object = drop(x = out),
    nm = colnames(x = out)
  )
}
