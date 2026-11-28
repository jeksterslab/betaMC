#' Monte Carlo Draws for Linear Regression Parameters
#'
#' Draws \eqn{\boldsymbol{\theta} =
#' \left( \boldsymbol{\beta}, \sigma^2,
#' \operatorname{vech}
#' \left( \boldsymbol{\Sigma}_{\boldsymbol{X}} \right) \right)}
#' from its sampling distribution,
#' screens and redraws inadmissible draws in C++,
#' and returns the model-implied covariance matrices of
#' \eqn{\left( Y, \boldsymbol{X} \right)} as rows of a matrix.
#' Gives the same draws as `.MC()` in betaMC for the same seed.
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param scale Numeric matrix.
#'   Sampling variance-covariance matrix of parameter estimates.
#' @param location Numeric vector.
#'   Vector of parameter estimates.
#' @param R Positive integer.
#'   Number of Monte Carlo replications.
#' @param decomposition Character string.
#'   Matrix decomposition of `scale`,
#'   `"chol"`, `"eigen"`, or `"svd"`.
#' @param pd Logical.
#'   If `pd = TRUE`, check that `scale` is positive semidefinite using `tol`
#'   (a singular matrix, with a zero eigenvalue, passes).
#'   If `decomposition = "chol"` and the Cholesky decomposition fails,
#'   the eigenvalue decomposition is used.
#' @param tol Numeric.
#'   Tolerance used for `pd`.
#' @param iter Positive integer.
#'   Maximum number of redraws for one draw.
#' @param p Positive integer.
#'   Number of regressors.
#' @param fixed_x Logical.
#'   If `fixed_x = TRUE`,
#'   `location` and `scale` exclude
#'   \eqn{\operatorname{vech}
#'   \left( \boldsymbol{\Sigma}_{\boldsymbol{X}} \right)},
#'   which is fixed at `vechsigmacapx`.
#' @param vechsigmacapx Numeric vector.
#'   \eqn{\operatorname{vech}
#'   \left( \boldsymbol{\Sigma}_{\boldsymbol{X}} \right)}
#'   used when `fixed_x = TRUE`.
#' @param seed Integer or `NULL`.
#'   Random seed.
#' @return Returns a list with the following elements:
#' \describe{
#'   \item{`thetahatstar`}{Numeric matrix of draws of
#'     \eqn{\boldsymbol{\theta}} (rows).}
#'   \item{`vechsigmacap`}{Numeric matrix of draws of
#'     \eqn{\operatorname{vech} \left( \boldsymbol{\Sigma} \right)} (rows).}
#'   \item{`screen`}{Named integer vector of the counts of the screened draws:
#'     `requested` (`R`), `retained` (rows returned),
#'     `rejected` (first-pass draws that fail the screen),
#'     `redraws` (all redraws, passed and failed),
#'     `exhausted` (rejected draws dropped after `iter` failed redraws),
#'     and `iter`.}
#' }
#'
#' @family Process lm Functions
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
  if (!is.null(x = seed)) {
    set.seed(seed = seed)
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
  screen <- attr(x = thetahatstar, which = "screen")
  attr(x = thetahatstar, which = "screen") <- NULL
  list(
    thetahatstar = thetahatstar,
    vechsigmacap = .MCVechSigmaLM(
      thetahatstar = thetahatstar,
      p = p
    ),
    screen = screen
  )
}
