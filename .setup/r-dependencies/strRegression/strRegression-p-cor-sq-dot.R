#' Squared Partial Correlation
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param srsq Numeric vector.
#'   Squared semipartial correlation.
#' @param rsq Numeric.
#'   R-squared.
#'
#' @family Partial Correlation Functions
#' @keywords strRegression pcor internal
#' @noRd
.PCorSq <- function(srsq,
                    rsq) {
  return(
    srsq / (
      1 - (
        rsq - srsq
      )
    )
  )
}
