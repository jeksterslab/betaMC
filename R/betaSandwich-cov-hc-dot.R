#' Sampling Covariance Matrix of the Standardized Parameter Vector
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param acov Numeric matrix.
#'   Asymptotic covariance matrix of the standardized parameter vector.
#' @param type Character string.
#'   Correction type.
#'   Possible values are
#'   `"hc0"`,
#'   `"hc1"`,
#'   `"hc2"`,
#'   `"hc3"`,
#'   `"hc4"`,
#'   `"hc4m"`, and
#'   `"hc5"`.
#' @param n Integer.
#'   Sample size.
#' @param df Integer.
#'   Degrees of freedom.
#'
#' @family Beta Sandwich Functions
#' @keywords betaSandwich cov internal
#' @noRd
.CovHC <- function(acov,
                   type,
                   n,
                   df) {
  nstar <- ((n - 1)^2) / n
  out <- (1 / nstar) * acov
  if (type == "hc1") {
    out <- (n / df) * out
  }
  out
}
