#' Monte Carlo Draws and Effect Sizes
#'
#' Draws the parameters with `.MCLM()`
#' (the same random number stream as earlier versions),
#' and computes the effect sizes of every draw once, in C++,
#' with `.StrRegressionofVechSigmaMat()`.
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param lm_process List.
#'   Output of `.ProcessLM()`.
#' @inheritParams MC
#' @inheritParams .MCLM
#' @param vechsigmacapx Numeric vector.
#'   \eqn{\mathrm{vech} \left( \boldsymbol{\Sigma}_{\mathbf{X}} \right)}
#'   appended to every draw when `fixed_x = TRUE`.
#' @return Returns a list with the following elements:
#' \describe{
#'   \item{`thetahatstar`}{Numeric matrix of draws of
#'     \eqn{\left( \boldsymbol{\beta}, \sigma^2,
#'     \mathrm{vech} \left( \boldsymbol{\Sigma}_{\mathbf{X}} \right)
#'     \right)} (rows).}
#'   \item{`vechsigmacap`}{Numeric matrix of draws of
#'     \eqn{\mathrm{vech} \left( \boldsymbol{\Sigma} \right)} (rows).}
#'   \item{`es`}{List of matrices of draws of
#'     `betastar`, `rsq`, `spcor`, and `pcorsq`.}
#' }
#'
#' @family Beta Monte Carlo Functions
#' @keywords betaMC mc internal
#' @noRd
.MCDraws <- function(scale,
                     location,
                     lm_process,
                     fixed_x = FALSE,
                     vechsigmacapx = NULL,
                     R = 20000L,
                     decomposition = "eigen",
                     pd = TRUE,
                     tol = 1e-06,
                     seed = NULL) {
  draws <- .MCLM(
    scale = scale,
    location = location,
    p = lm_process$p,
    fixed_x = fixed_x,
    vechsigmacapx = vechsigmacapx,
    R = R,
    decomposition = decomposition,
    pd = pd,
    tol = tol,
    seed = seed
  )
  colnames(draws$thetahatstar) <- names(lm_process$theta)
  es <- .StrRegressionofVechSigmaMat(
    vechsigmacap = draws$vechsigmacap,
    k = lm_process$k
  )
  varnames <- lm_process$xnames
  colnames(es$betastar) <- varnames
  colnames(es$spcor) <- varnames
  colnames(es$pcorsq) <- varnames
  es$rsq <- matrix(
    data = es$rsq,
    ncol = 1
  )
  draws$es <- es
  draws
}
