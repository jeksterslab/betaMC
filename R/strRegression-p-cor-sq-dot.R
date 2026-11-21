#' Squared Partial Correlation
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param srsq Numeric vector.
#'   Squared semipartial correlation.
#' @param rsq Numeric.
#'   R-squared.
#'
#' @return Returns a numeric vector of the squared partial correlations.
#'
#' @examples
#' strRegression:::.PCorSq(srsq = c(0.1, 0.2), rsq = 0.5)
#'
#' @family Partial Correlation Functions
#' @keywords strRegression pcor internal
#' @noRd
.PCorSq <- function(srsq,
                    rsq) {
  srsq / (
    1 - (
      rsq - srsq
    )
  )
}
