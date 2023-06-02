#' Adjusted R-Squared
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param rsq Numeric.
#'   R-Squared.
#' @param k Positive integer.
#'   `p` regressors plus 1.
#' @param n Positive integer.
#'   Sample size.
#'
#' @family R-squared Functions
#' @keywords strRegression rsq internal
#' @noRd
.RSqBar <- function(rsq,
                    k,
                    n) {
  return(
    1 - (1 - rsq) * (
      (n - 1) / (n - k)
    )
  )
}
