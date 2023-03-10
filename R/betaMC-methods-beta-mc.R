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
#'   `DeltaRSqMC()`, or
#'   `PCorMC()`
#'   functions.
#' @inheritParams summary.betamc
#'
#' @examples
#' # Fit the regression model
#' object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
#' # Generate the sampling distribution of parameter estimates
#' # (use a large R, for example, R = 20000 for actual research)
#' mc <- MC(object, R = 100)
#' # Generate confidence intervals for standardized regression slopes
#' std <- BetaMC(mc)
#' # Method ---------------------------------------------------------
#' print(std)
#' @export
#' @keywords methods
print.betamc <- function(x,
                         alpha = c(0.05, 0.01, 0.001),
                         digits = 4,
                         ...) {
  type <- x$object$args$type
  if (x$fun == "BetaMC") {
    label <- "Standardized regression slopes"
  }
  if (x$fun == "RSqMC") {
    label <- "R-squared and adjusted R-squared"
  }
  if (x$fun == "SCorMC") {
    label <- "Semipartial correlations"
  }
  if (x$fun == "DeltaRSqMC") {
    label <- "Improvement in R-squared"
  }
  if (x$fun == "PCorMC") {
    label <- "Squared partial correlations"
  }
  if (x$fun == "DiffBetaMC") {
    label <- "Differences of standardized regression slopes"
  }
  cat(
    paste0(
      label,
      "\n",
      "type = ",
      "\"",
      type,
      "\"",
      "\n"
    )
  )
  base::print(
    round(
      .CI(
        object = x,
        alpha = alpha
      ),
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
#'   `DeltaRSqMC()`, or
#'   `PCorMC()`
#'   functions.
#' @param ... additional arguments.
#' @param alpha Significance level.
#' @param digits Digits to print.
#'
#' @examples
#' # Fit the regression model
#' object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
#' # Generate the sampling distribution of parameter estimates
#' # (use a large R, for example, R = 20000 for actual research)
#' mc <- MC(object, R = 100)
#' # Generate confidence intervals for standardized regression slopes
#' std <- BetaMC(mc)
#' # Method ---------------------------------------------------------
#' summary(std)
#' @export
#' @keywords methods
summary.betamc <- function(object,
                           alpha = c(0.05, 0.01, 0.001),
                           digits = 4,
                           ...) {
  type <- object$object$args$type
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
  cat(
    paste0(
      label,
      "\n",
      "type = ",
      "\"",
      type,
      "\"",
      "\n"
    )
  )
  return(
    round(
      .CI(
        object = object,
        alpha = alpha
      ),
      digits = digits
    )
  )
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
#' @examples
#' # Fit the regression model
#' object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
#' # Generate the sampling distribution of parameter estimates
#' # (use a large R, for example, R = 20000 for actual research)
#' mc <- MC(object, R = 100)
#' # Generate confidence intervals for standardized regression slopes
#' std <- BetaMC(mc)
#' # Method ---------------------------------------------------------
#' vcov(std)
#' @export
#' @keywords methods
vcov.betamc <- function(object,
                        ...) {
  thetahatstar <- do.call(
    what = "rbind",
    args = object$thetahatstar
  )
  thetahatstar <- unname(thetahatstar)
  colnames(thetahatstar) <- names(object$est)
  return(
    stats::var(thetahatstar)
  )
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
#' @examples
#' # Fit the regression model
#' object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
#' # Generate the sampling distribution of parameter estimates
#' # (use a large R, for example, R = 20000 for actual research)
#' mc <- MC(object, R = 100)
#' # Generate confidence intervals for standardized regression slopes
#' std <- BetaMC(mc)
#' # Method ---------------------------------------------------------
#' coef(std)
#' @export
#' @keywords methods
coef.betamc <- function(object,
                        ...) {
  return(
    object$est
  )
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
#' @examples
#' # Fit the regression model
#' object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
#' # Generate the sampling distribution of parameter estimates
#' # (use a large R, for example, R = 20000 for actual research)
#' mc <- MC(object, R = 100)
#' # Generate confidence intervals for standardized regression slopes
#' std <- BetaMC(mc)
#' # Method ---------------------------------------------------------
#' confint(std, level = 0.95)
#' @export
#' @keywords methods
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
  return(
    .CI(
      object = object,
      alpha = 1 - level[1]
    )[parm, 4:5]
  )
}
