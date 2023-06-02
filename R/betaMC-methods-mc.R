#' Print Method for an Object of Class `mc`
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @return Prints the first set of simulated parameter estimates
#'   and model-implied covariance matrix.
#'
#' @param x Object of Class `mc`.
#' @param ... additional arguments.
#'
#' @examples
#' object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
#' mc <- MC(object, R = 100)
#' print(mc)
#'
#' @keywords methods
#' @export
print.mc <- function(x,
                     ...) {
  cat("Call:\n")
  base::print(x$call)
  cat(
    paste0(
      "The first set of simulated parameter estimates\n",
      "and model-implied covariance matrix.\n",
      "\n"
    )
  )
  base::print(
    x$thetahatstar[[1]]
  )
}

#' Summary Method for an Object of Class
#' `mc`
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @return Returns a list with the following elements:
#'   \describe{
#'     \item{mean}{Mean of the sampling distribution of
#'       \eqn{\boldsymbol{\hat{\theta}}}.}
#'     \item{var}{Variance of the sampling distribution of
#'       \eqn{\boldsymbol{\hat{\theta}}}.}
#'     \item{bias}{Monte Carlo simulation bias.}
#'     \item{rmse}{Monte Carlo simulation root mean square error.}
#'     \item{location}{Location parameter used in the Monte Carlo simulation.}
#'     \item{scale}{Scale parameter used in the Monte Carlo simulation.}
#'   }
#'
#' @param object Object of Class `mc`,
#'   that is,
#'   the output of the
#'   `MC()`
#'   function.
#' @param ... additional arguments.
#' @param digits Digits to print.
#'
#' @examples
#' # Fit the regression model
#' object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
#' mc <- MC(object, R = 100)
#' summary(mc)
#'
#' @keywords methods
#' @export
summary.mc <- function(object,
                       digits = 4,
                       ...) {
  varnames <- c(
    paste0("b", seq_len(object$lm_process$p)),
    "sigmasq",
    paste0(
      "sigma",
      .VechNames(
        x = paste0("x", seq_len(object$lm_process$p)),
        sep = ""
      )
    )
  )
  bias <- .MCBias(object)
  var <- .MCVar(object)
  mean <- .MCMean(object)
  rmse <- .MCRMSE(object)
  location <- object$location
  scale <- object$scale
  names(mean) <- varnames
  names(bias) <- varnames
  names(rmse) <- varnames
  rownames(var) <- colnames(var) <- varnames
  if (object$args$fixed_x) {
    names(location) <- varnames[seq_len(object$lm_process$k)]
    rownames(scale) <- colnames(scale) <- varnames[seq_len(object$lm_process$k)]
  } else {
    names(location) <- varnames
    rownames(scale) <- colnames(scale) <- varnames
  }
  base::print(object$call)
  return(
    list(
      mean = round(mean, digits = digits),
      var = round(var, digits = digits),
      bias = round(bias, digits = digits),
      rmse = round(rmse, digits = digits),
      location = round(location, digits = digits),
      scale = round(scale, digits = digits)
    )
  )
}
