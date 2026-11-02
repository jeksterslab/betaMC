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
    unname(object$thetahatstar)
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
    unname(object$thetahatstar)
  )
}
