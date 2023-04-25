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
#' @keywords eval internal
#' @noRd
.MCRMSE <- function(object) {
  return(
    sqrt(
      colMeans(
        do.call(
          what = "rbind",
          args = lapply(
            X = object$thetahatstar,
            FUN = function(i,
                           est) {
              return(
                (
                  c(
                    i$coef,
                    i$sigmasq,
                    i$vechsigmacapx
                  )
                  -
                    est
                )^2
              )
            },
            est = c(
              object$lm_process$beta,
              object$lm_process$sigmasq,
              object$lm_process$vechsigmacapx
            )
          )
        )
      )
    )
  )
}
