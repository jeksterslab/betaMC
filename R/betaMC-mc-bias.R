#' Monte Carlo Simulation Bias
#'
#' @details
#' \deqn{
#'   \mathrm{Bias}_{\matnrm{MC}}
#'    =
#'    \left(
#'      R^{-1}
#'      \sum_{r = 1}^{R}
#'      \boldsymbol{\hat{\theta}}_{r}
#'    \right)
#'    -
#'    \boldsymbol{\hat{\theta}}
#' }
#'
#' @inheritParams BetaMC
#'
#' @family Beta Monte Carlo Functions
#' @keywords eval internal
#' @noRd
.MCBias <- function(object) {
  colMeans(
    unname(object$thetahatstar)
  ) - c(
    object$lm_process$beta,
    object$lm_process$sigmasq,
    object$lm_process$vechsigmacapx
  )
}
