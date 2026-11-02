#' Monte Carlo Simulation Root Mean Square Error
#'
#' @details
#' \deqn{
#'   \mathrm{RMSE}_{\matnrm{MC}}
#'    =
#'    \left[
#'      R^{-1}
#'      \left(
#'        \sum_{r = 1}^{R}
#'        \boldsymbol{\hat{\theta}}_{r}
#'        -
#'        \boldsymbol{\hat{\theta}}
#'      \right)^{2}
#'    \right]^{\frac{1}{2}}
#' }
#'
#' @inheritParams BetaMC
#'
#' @family Beta Monte Carlo Functions
#' @keywords eval internal
#' @noRd
.MCRMSE <- function(object) {
  est <- c(
    object$lm_process$beta,
    object$lm_process$sigmasq,
    object$lm_process$vechsigmacapx
  )
  sqrt(
    colMeans(
      (
        unname(object$thetahatstar) - rep(
          x = est,
          each = nrow(object$thetahatstar)
        )
      )^2
    )
  )
}
