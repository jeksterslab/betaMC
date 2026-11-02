#' Monte Carlo Draws for Linear Regression Parameters
#'
#' Draws \eqn{\boldsymbol{\theta} =
#' \left( \boldsymbol{\beta}, \sigma^2,
#' \mathrm{vech} \left( \boldsymbol{\Sigma}_{\mathbf{X}} \right) \right)}
#' from its sampling distribution,
#' screens and redraws inadmissible draws in C++,
#' and returns the model-implied covariance matrices of
#' \eqn{\left( Y, \mathbf{X} \right)} as rows of a matrix.
#' Gives the same draws as `.MC()` in betaMC for the same seed.
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @inheritParams .ThetaHatStarScreen
#' @param p Positive integer.
#'   Number of regressors.
#' @param fixed_x Logical.
#'   If `fixed_x = TRUE`,
#'   `location` and `scale` exclude
#'   \eqn{\mathrm{vech} \left( \boldsymbol{\Sigma}_{\mathbf{X}} \right)},
#'   which is fixed at `vechsigmacapx`.
#' @param vechsigmacapx Numeric vector.
#'   \eqn{\mathrm{vech} \left( \boldsymbol{\Sigma}_{\mathbf{X}} \right)}
#'   used when `fixed_x = TRUE`.
#' @param seed Integer or `NULL`.
#'   Random seed.
#' @return Returns a list with the following elements:
#' \describe{
#'   \item{`thetahatstar`}{Numeric matrix of draws of
#'     \eqn{\boldsymbol{\theta}} (rows).}
#'   \item{`vechsigmacap`}{Numeric matrix of draws of
#'     \eqn{\mathrm{vech} \left( \boldsymbol{\Sigma} \right)} (rows).}
#' }
#'
#' @family Process LM Functions
#' @keywords processLM mc internal
#' @noRd
.MCLM <- function(scale,
                  location,
                  p,
                  fixed_x = FALSE,
                  vechsigmacapx = NULL,
                  R = 20000L,
                  decomposition = "eigen",
                  pd = TRUE,
                  tol = 1e-06,
                  iter = 1000L,
                  seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  if (fixed_x) {
    append <- vechsigmacapx
  } else {
    append <- NULL
  }
  thetahatstar <- .ThetaHatStarScreen(
    R = R,
    scale = scale,
    location = location,
    screen = function(x) {
      .MCScreenLM(
        thetahatstar = x,
        p = p
      )
    },
    append = append,
    decomposition = decomposition,
    pd = pd,
    tol = tol,
    iter = iter
  )
  list(
    thetahatstar = thetahatstar,
    vechsigmacap = .MCVechSigmaLM(
      thetahatstar = thetahatstar,
      p = p
    )
  )
}
