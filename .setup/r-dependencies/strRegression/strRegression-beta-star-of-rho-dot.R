#' Standardized Partial Regression Slopes
#' (\eqn{\mathbf{P}})
#'
#' Calculate standardized partial regression slopes
#' from the correlation matrix.
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param rhocap Numeric matrix.
#'   Correlation matrix of
#'   \eqn{\left\{ Y, X_{1}, \dots, X_{p} \right\}}.
#' @param k Positive integer.
#'   Dimension of the `k` by `k` correlation matrix.
#'
#' @return Returns a matrix.
#' @family Standardized Slopes Functions
#' @keywords strRegression slopesstd internal
#' @noRd
.BetaStarofRho <- function(rhocap,
                           k) {
  return(
    .Vec(
      solve(
        rhocap[
          2:k,
          2:k,
          drop = FALSE
        ],
        rhocap[
          2:k,
          1,
          drop = FALSE
        ]
      )
    )
  )
}
