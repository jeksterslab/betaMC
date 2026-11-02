#' Selectively Standardized Partial Regression Slopes
#'
#' Computes
#' \eqn{\beta^{\ast}_{j} \sigma_{y}^{1 - c} \sigma_{x_j}^{a_j - 1}},
#' that is,
#' \eqn{b_j \sigma_{x_j}^{a_j} / \sigma_{y}^{c}},
#' where \eqn{c = 1} when \eqn{y} is standardized
#' and \eqn{a_j = 1} when \eqn{x_j} is standardized.
#' With `std_y = TRUE` and every `std_x` `TRUE`
#' it returns `betastar`.
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param betastar Numeric vector of length `p`,
#'   or numeric matrix with `p` columns (one draw per row).
#'   Standardized regression slopes.
#' @param sigmay Numeric vector.
#'   Standard deviation of \eqn{y} (one value per row of `betastar`).
#' @param sigmax Numeric vector of length `p`,
#'   or numeric matrix with `p` columns.
#'   Standard deviations of the regressors.
#' @param std_y Logical.
#'   Standardize \eqn{y}.
#' @param std_x Logical vector of length `p`.
#'   Standardize each regressor.
#' @return Returns a numeric vector or matrix with the same shape as
#'   `betastar`.
#'
#' @family Structure of Regression Functions
#' @keywords strRegression internal
#' @noRd
.BetaStarSelect <- function(betastar,
                            sigmay,
                            sigmax,
                            std_y = TRUE,
                            std_x) {
  if (is.matrix(betastar)) {
    betastar * sigmay^(1 - std_y) * (
      sigmax^matrix(
        data = std_x - 1,
        nrow = nrow(betastar),
        ncol = ncol(betastar),
        byrow = TRUE
      )
    )
  } else {
    betastar * sigmay^(1 - std_y) * sigmax^(std_x - 1)
  }
}
