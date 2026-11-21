#' Standardized Partial Regression Slopes of
#' \eqn{\boldsymbol{P}}
#'
#' Calculate standardized partial regression slopes
#' from the correlation matrix.
#'
#' @details Let the correlation matrix of \eqn{Y} and
#'   \eqn{\boldsymbol{X} = \left\{ X_{1}, \dots, X_{p} \right\}}
#'   be partitioned as follows
#'   \deqn{
#'     \boldsymbol{P}
#'     =
#'     \left(
#'     \begin{array}{cc}
#'     1
#'     &
#'     \boldsymbol{\rho}_{Y \boldsymbol{X}} \\
#'     \boldsymbol{\rho}_{\boldsymbol{X} Y}
#'     &
#'     \boldsymbol{P}_{\boldsymbol{X} \boldsymbol{X}}
#'     \end{array}
#'     \right) .
#'   }
#'   The vector of standardized partial regression slopes
#'   is given by
#'   \deqn{
#'     \boldsymbol{\beta}^{\ast}
#'     =
#'     \boldsymbol{P}_{\boldsymbol{X} \boldsymbol{X}}^{-1}
#'     \boldsymbol{\rho}_{Y \boldsymbol{X}} .
#'   }
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param rhocap Numeric matrix.
#'   \eqn{\boldsymbol{P}}.
#'   Correlation matrix of
#'   \eqn{\left\{ Y, X_{1}, \dots, X_{p} \right\}}.
#' @param k Positive integer.
#'   Dimension of the `k` by `k` correlation matrix.
#'
#' @return Returns a vector.
#' @examples
#' rhocap <- matrix(data = c(1, 0.5, 0.4, 0.5, 1, 0.3, 0.4, 0.3, 1), nrow = 3)
#' strRegression:::.BetaStarofRho(rhocap = rhocap, k = 3)
#'
#' @family Standardized Slopes Functions
#' @keywords strRegression slopesstd internal
#' @noRd
.BetaStarofRho <- function(rhocap,
                           k) {
  .Vec(
    x = solve(
      a = rhocap[
        2:k,
        2:k,
        drop = FALSE
      ],
      b = rhocap[
        2:k,
        1,
        drop = FALSE
      ]
    )
  )
}
