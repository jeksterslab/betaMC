#' Print Method for an Object of Class
#' `betamc`
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @return Prints a matrix of
#'   estimates,
#'   standard errors,
#'   number of Monte Carlo replications,
#'   and
#'   confidence intervals.
#'
#' @param x Object of Class `betamc`,
#'   that is,
#'   the output of the
#'   `BetaMC()`,
#'   `RSqMC()`,
#'   `SCorMC()`,
#'   `DeltaRSqMC()`,
#'   `PCorMC()`, or
#'   `DiffBetaMC()`
#'   functions.
#' @param alpha Numeric vector.
#'   Significance level \eqn{\alpha}.
#'   If `alpha = NULL`,
#'   use the argument `alpha` used in `x`.
#' @inheritParams summary.betamc
#'
#' @keywords methods
#' @export
print.betamc <- function(x,
                         alpha = NULL,
                         digits = 4,
                         ...) {
  print.summary.betamc(
    summary.betamc(
      object = x,
      alpha = alpha,
      digits = digits
    )
  )
}

#' Summary Method for an Object of Class
#' `betamc`
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @return Returns a matrix of
#'   estimates,
#'   standard errors,
#'   number of Monte Carlo replications,
#'   and
#'   confidence intervals.
#'
#' @param object Object of Class `betamc`,
#'   that is,
#'   the output of the
#'   `BetaMC()`,
#'   `RSqMC()`,
#'   `SCorMC()`,
#'   `DeltaRSqMC()`,
#'   `PCorMC()`, or
#'   `DiffBetaMC()`
#'   functions.
#' @param ... additional arguments.
#' @param alpha Numeric vector.
#'   Significance level \eqn{\alpha}.
#'   If `alpha = NULL`,
#'   use the argument `alpha` used in `object`.
#' @param digits Digits to print.
#'
#' @keywords methods
#' @export
summary.betamc <- function(object,
                           alpha = NULL,
                           digits = 4,
                           ...) {
  ci <- .CI(
    object = object,
    alpha = alpha
  )
  print_summary <- round(
    x = ci,
    digits = digits
  )
  attr(
    x = ci,
    which = "fit"
  ) <- object
  attr(
    x = ci,
    which = "print_summary"
  ) <- print_summary
  attr(
    x = ci,
    which = "alpha"
  ) <- alpha
  attr(
    x = ci,
    which = "digits"
  ) <- digits
  class(ci) <- "summary.betamc"
  ci
}

#' @noRd
#' @keywords internal
#' @exportS3Method print summary.betamc
print.summary.betamc <- function(x,
                                 ...) {
  print_summary <- attr(
    x = x,
    which = "print_summary"
  )
  object <- attr(
    x = x,
    which = "fit"
  )
  type <- object$args$object$args$type
  if (object$fun == "BetaMC") {
    label <- "Standardized regression slopes"
  }
  if (object$fun == "RSqMC") {
    label <- "R-squared and adjusted R-squared"
  }
  if (object$fun == "SCorMC") {
    label <- "Semipartial correlations"
  }
  if (object$fun == "DeltaRSqMC") {
    label <- "Improvement in R-squared"
  }
  if (object$fun == "PCorMC") {
    label <- "Squared partial correlations"
  }
  if (object$fun == "DiffBetaMC") {
    label <- "Differences of standardized regression slopes"
  }
  cat("Call:\n")
  base::print(object$call)
  cat(
    paste0(
      "\n",
      label,
      "\n",
      "type = ",
      "\"",
      type,
      "\"",
      "\n"
    )
  )
  print(print_summary)
  invisible(x)
}

#' Sampling Variance-Covariance Matrix Method for an Object of Class
#' `betamc`
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @return Returns the variance-covariance matrix of estimates.
#'
#' @inheritParams summary.betamc
#'
#' @keywords methods
#' @export
vcov.betamc <- function(object,
                        ...) {
  thetahatstar <- do.call(
    what = "rbind",
    args = object$thetahatstar
  )
  thetahatstar <- unname(thetahatstar)
  colnames(thetahatstar) <- names(object$est)
  stats::var(thetahatstar)
}

#' Estimated Parameter Method for an Object of Class
#' `betamc`
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @return Returns a vector of estimated parameters.
#'
#' @inheritParams summary.betamc
#'
#' @keywords methods
#' @export
coef.betamc <- function(object,
                        ...) {
  object$est
}

#' Confidence Intervals Method for an Object of Class
#' `betamc`
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @return Returns a matrix of confidence intervals.
#'
#' @inheritParams summary.betamc
#' @param parm a specification of which parameters
#'   are to be given confidence intervals,
#'   either a vector of numbers or a vector of names.
#'   If missing, all parameters are considered.
#' @param level the confidence level required.
#'
#' @keywords methods
#' @export
confint.betamc <- function(object,
                           parm = NULL,
                           level = 0.95,
                           ...) {
  if (is.null(parm)) {
    parm <- seq_len(
      length(
        object$est
      )
    )
  }
  ci <- .CI(
    object = object,
    alpha = 1 - level[1]
  )[parm, 4:5, drop = FALSE]
  varnames <- colnames(ci)
  varnames <- gsub(
    pattern = "%",
    replacement = " %",
    x = varnames
  )
  colnames(ci) <- varnames
  ci
}
