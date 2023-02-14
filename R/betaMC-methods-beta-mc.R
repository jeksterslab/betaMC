#' Print Method for an Object of Class `betamc`
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @return Returns a matrix of
#'   standardized regression slopes,
#'   standard errors,
#'   number of Monte Carlo replications,
#'   and
#'   confidence intervals.
#'
#' @param x Object of Class `betamc`.
#' @param ... additional arguments.
#' @param alpha Significance level.
#' @param digits Digits to print.
#'
#' @examples
#' object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
#' std <- BetaMC(object, R = 100)
#' # use a large R, for example, R = 20000 for actual research
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
    paste0(
      "\nStandardized regression slopes\n",
      "type = ",
      "\"",
      x$type,
      "\"",
      "\n"
    )
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
#' @return Returns a matrix of
#'   standardized regression slopes,
#'   standard errors,
#'   number of Monte Carlo replications,
#'   and
#'   confidence intervals.
#'
#' @param object Object of class `betamc`.
#' @param ... additional arguments.
#' @param alpha Significance level.
#' @param digits Digits to print.
#'
#' @examples
#' object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
#' std <- BetaMC(object, R = 100)
#' # use a large R, for example, R = 20000 for actual research
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
    paste0(
      "\nStandardized regression slopes\n",
      "type = ",
      "\"",
      object$type,
      "\"",
      "\n"
    )
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
#' @return Returns a matrix of the
#'   variance-covariance matrix
#'   of standardized slopes.
#'
#' @param object Object of class `betamc`.
#' @param ... additional arguments.
#'
#' @examples
#' object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
#' std <- BetaMC(object, R = 100)
#' # use a large R, for example, R = 20000 for actual research
#' vcov(std)
#' @export
#' @keywords methods
vcov.betamc <- function(object,
                        ...) {
  return(
    object$vcov[
      seq_len(object$lm_process$p),
      seq_len(object$lm_process$p),
      drop = FALSE
    ]
  )
}

#' Standardized Regression Slopes
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @return Returns a vector of standardized regression slopes.
#'
#' @param object Object of class `betamc`.
#' @param ... additional arguments.
#'
#' @examples
#' object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
#' std <- BetaMC(object, R = 100)
#' # use a large R, for example, R = 20000 for actual research
#' coef(std)
#' @export
#' @keywords methods
coef.betamc <- function(object,
                        ...) {
  return(
    object$est
  )
}

#' Confidence Intervals for Standardized Regression Slopes
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @return Returns a matrix of confidence intervals.
#'
#' @param object Object of class `betamc`.
#' @param ... additional arguments.
#' @param parm a specification of which parameters
#'   are to be given confidence intervals,
#'   either a vector of numbers or a vector of names.
#'   If missing, all parameters are considered.
#' @param level the confidence level required.
#'
#' @examples
#' object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
#' std <- BetaMC(object, R = 100)
#' # use a large R, for example, R = 20000 for actual research
#' confint(std, level = 0.95)
#' @export
#' @keywords methods
confint.betamc <- function(object,
                           parm = NULL,
                           level = 0.95,
                           ...) {
  if (is.null(parm)) {
    parm <- seq_len(object$lm_process$p)
  }
  return(
    .BetaCI(
      object = object,
      alpha = 1 - level[1]
    )[parm, 4:5]
  )
}
