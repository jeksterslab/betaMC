#' Print Method for an Object of Class `betamc`
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param x Object of Class `betamc`.
#' @param ... additional arguments.
#' @param alpha Significance level.
#' @param digits Digits to print.
#' @return Returns a matrix of standardized regression slopes,
#'   standard errors, test statistics, p-values, and confidence intervals.
#' @examples
#' object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
#' std <- BetaMC(object)
#' print(std)
#' @export
#' @keywords methods
print.betamc <- function(x,
                         alpha = c(0.05, 0.01, 0.001),
                         digits = 4,
                         ...) {
  cat("Call:\n")
  base::print(x$call)
  cat(
    toupper(x$type),
    "sampling variance-covariance matrix:\n"
  )
  base::print(
    round(
      .BetaCI(
        object = x,
        alpha = alpha
      ),
      digits = digits
    )
  )
}

#' Summary Method for an Object of Class `betamc`
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param object Object of class `betamc`.
#' @param ... additional arguments.
#' @param alpha Significance level.
#' @param digits Digits to print.
#' @return Returns a matrix of standardized regression slopes,
#'   standard errors, test statistics, p-values, and confidence intervals.
#' @examples
#' object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
#' std <- BetaMC(object)
#' summary(std)
#' @export
#' @keywords methods
summary.betamc <- function(object,
                           alpha = c(0.05, 0.01, 0.001),
                           digits = 4,
                           ...) {
  cat("Call:\n")
  base::print(object$call)
  cat(
    "\nStandardized regression slopes with",
    toupper(object$type),
    "standard errors:\n"
  )
  return(
    round(
      .BetaCI(
        object = object,
        alpha = alpha
      ),
      digits = digits
    )
  )
}

#' Sampling Covariance Matrix of the Standardized Regression Slopes
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param object Object of class `betamc`.
#' @param ... additional arguments.
#' @return Returns a matrix of the variance-covariance matrix
#'   of standardized slopes.
#' @examples
#' object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
#' std <- BetaMC(object)
#' vcov(std)
#' @export
#' @keywords methods
vcov.betamc <- function(object,
                        ...) {
  p <- length(object$beta)
  out <- object$vcov[1:p, 1:p, drop = FALSE]
  rownames(out) <- colnames(out) <- names(object$beta)
  return(out)
}

#' Standardized Regression Slopes
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param object Object of class `betamc`.
#' @param ... additional arguments.
#' @return Returns a vector of standardized regression slopes.
#' @examples
#' object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
#' std <- BetaMC(object)
#' coef(std)
#' @export
#' @keywords methods
coef.betamc <- function(object,
                        ...) {
  object$beta
}

#' Confidence Intervals for Standardized Regression Slopes
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param object Object of class `betamc`.
#' @param ... additional arguments.
#' @param parm a specification of which parameters
#'   are to be given confidence intervals,
#'   either a vector of numbers or a vector of names.
#'   If missing, all parameters are considered.
#' @param level the confidence level required.
#' @return Returns a matrix of confidence intervals.
#' @examples
#' object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
#' std <- BetaMC(object)
#' confint(std, level = 0.95)
#' @export
#' @keywords methods
confint.betamc <- function(object,
                           parm = NULL,
                           level = 0.95,
                           ...) {
  if (is.null(parm)) {
    parm <- 1:object$p
  }
  return(
    .BetaCI(
      object = object,
      alpha = 1 - level[1]
    )[parm, 4:5]
  )
}
