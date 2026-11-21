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
#' @return Returns a numeric value, the adjusted R-squared.
#'
#' @examples
#' strRegression:::.RSqBar(rsq = 0.5, k = 3, n = 100)
#'
#' @family R-squared Functions
#' @keywords strRegression rsq internal
#' @noRd
.RSqBar <- function(rsq,
                    k,
                    n) {
  1 - (1 - rsq) * (
    (n - 1) / (n - k)
  )
}
