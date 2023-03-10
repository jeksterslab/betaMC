#' Matrix of Monte Carlo Simulation Parameter Estimates
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @inheritParams BetaMC
#'
#' @return Returns a matrix of parameter estimates.
#'
#' @family Beta Monte Carlo Functions
#' @keywords betaMC mc internal
#' @noRd
.MCThetaHatMatrix <- function(object) {
  return(
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
}
