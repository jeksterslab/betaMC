#' Standardized Partial Regression Slopes of
#' \eqn{\mathbf{P}}
#'
#' Calculate standardized partial regression slopes
#' from the correlation matrix.
#'
#' @details Let the correlation matrix of \eqn{Y} and
#'   \eqn{\mathbf{X} = \left\{ X_{1}, \dots, \X_{p} \right\}}
#'   be partitioned as follows
#'   \deqn{
#'     \mathbf{P}
#'     =
#'     \left(
#'     \begin{array}{cc}
#'     1
#'     &
#'     \boldsymbol{\rho}_{Y \mathbf{X}} \\
#'     \boldsymbol{\rho}_{\mathbf{X} Y}
#'     &
#'     \mathbf{P}_{\mathbf{X} \mathbf{X}}
#'     \end{array}
#'     \right) .
#'   }
#'   The vector of standardized partial regression slopes
#'   is given by
#'   \deqn{
#'     \boldsymbol{\beta}^{\ast}
#'     =
#'     \mathbf{P}_{\mathbf{X} \mathbf{X}}^{-1}
#'     \boldsymbol{\rho}_{Y \mathbf{X}} .
#'   }
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param rhocap Numeric matrix.
#'   \eqn{\mathbf{P}}.
#'   Correlation matrix of
#'   \eqn{\left\{ Y, X_{1}, \dots, X_{p} \right\}}.
#' @param k Positive integer.
#'   Dimension of the `k` by `k` correlation matrix.
#'
#' @return Returns a vector.
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
