#' Monte Carlo Simulation Variance
#'
#' @details
#' \deqn{
#'   \mathrm{Var}_{\matnrm{MC}}
#'    =
#'    \left[
#'      \left( R - 1 \right)^{-1}
#'      \sum_{r = 1}^{R}
#'      \boldsymbol{\hat{\theta}}_{r} - \boldsymbol{\bar{\hat{\theta}}}
#'    \right]
#' }
#'
#' @inheritParams BetaMC
#'
#' @family Beta Monte Carlo Functions
#' @keywords eval internal
#' @noRd
.MCVar <- function(object) {
  stats::var(
    do.call(
      what = "rbind",
      args = lapply(
        X = object$thetahatstar,
        FUN = function(i) {
          c(
            i$coef,
            i$sigmasq,
            i$vechsigmacapx
          )
        }
      )
    )
  )
}

#' Monte Carlo Simulation Mean
#'
#' @inheritParams BetaMC
#'
#' @family Beta Monte Carlo Functions
#' @keywords eval internal
#' @noRd
.MCMean <- function(object) {
  colMeans(
    do.call(
      what = "rbind",
      args = lapply(
        X = object$thetahatstar,
        FUN = function(i) {
          c(
            i$coef,
            i$sigmasq,
            i$vechsigmacapx
          )
        }
      )
    )
  )
}
