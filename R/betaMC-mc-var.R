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
#' @keywords internal
#' @noRd
.MCVar <- function(object) {
  return(
    stats::var(
      do.call(
        what = "rbind",
        args = lapply(
          X = object$thetahatstar,
          FUN = function(i) {
            return(
              c(
                i$coef,
                i$sigmasq,
                i$vechsigmacapx
              )
            )
          }
        )
      )
    )
  )
}

.MCMean <- function(object) {
  return(
    colMeans(
      do.call(
        what = "rbind",
        args = lapply(
          X = object$thetahatstar,
          FUN = function(i) {
            return(
              c(
                i$coef,
                i$sigmasq,
                i$vechsigmacapx
              )
            )
          }
        )
      )
    )
  )
}
