#' Print Method for an Object of Class `pcorbetamc`
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @return Returns a matrix of
#'   partial and semipartial correlation coefficients,
#'   standard errors,
#'   number of Monte Carlo replications,
#'   and
#'   confidence intervals.
#'
#' @param x Object of class `pcorbetamc`.
#' @param ... additional arguments.
#' @param alpha Significance level.
#' @param digits Digits to print.
#'
#' @examples
#' object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
#' std <- BetaMC(object, R = 100)
#' # use a large R, for example, R = 20000 for actual research
#' pcor <- PCorBetaMC(std)
#' print(pcor)
#' @export
#' @keywords methods
print.pcorbetamc <- function(x,
                             alpha = c(0.05, 0.01, 0.001),
                             digits = 4,
                             ...) {
  cat(
    paste0(
      "Semipartial correlation (*)\nSquared semipartial correlation (^)\nSquared partial correlation (+)\n",
      "type = ",
      "\"",
      x$fit$type,
      "\"",
      "\n"
    )
  )
  base::print(
    round(
      .PCorCI(
        object = x,
        alpha = alpha
      ),
      digits = digits
    )
  )
}

#' Summary Method for an Object of Class `pcorbetamc`
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @return Returns a matrix of
#'   partial and semipartial correlation coefficients,
#'   standard errors,
#'   number of Monte Carlo replications,
#'   and
#'   confidence intervals.
#'
#' @param object Object of class `pcorbetamc`.
#' @param ... additional arguments.
#' @param alpha Significance level.
#' @param digits Digits to print.
#'
#' @examples
#' object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
#' std <- BetaMC(object, R = 100)
#' # use a large R, for example, R = 20000 for actual research
#' pcor <- PCorBetaMC(std)
#' summary(pcor)
#' @export
#' @keywords methods
summary.pcorbetamc <- function(object,
                               alpha = c(0.05, 0.01, 0.001),
                               digits = 4,
                               ...) {
  cat(
    paste0(
      "Semipartial correlation (*)\nSquared semipartial correlation (^)\nSquared partial correlation (+)\n",
      "type = ",
      "\"",
      object$fit$type,
      "\"",
      "\n"
    )
  )
  return(
    round(
      .PCorCI(
        object = object,
        alpha = alpha
      ),
      digits = digits
    )
  )
}

#' Sampling Covariance Matrix of
#' Partial and Semipartial Correlation Coefficients
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @return Returns a matrix of the
#'   variance-covariance matrix
#'   of partial and semipartial correlation coefficients.
#'
#' @param object Object of class `pcorbetamc`.
#' @param ... additional arguments.
#'
#' @examples
#' object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
#' std <- BetaMC(object, R = 100)
#' # use a large R, for example, R = 20000 for actual research
#' pcor <- PCorBetaMC(std)
#' vcov(pcor)
#' @export
#' @keywords methods
vcov.pcorbetamc <- function(object,
                            ...) {
  return(
    object$vcov
  )
}

#' Partial and Semipartial Correlation Coefficients
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @return Returns a vector of partial and semipartial correlation coefficients
#'
#' @param object Object of class `pcorbetamc`.
#' @param ... additional arguments.
#'
#' @examples
#' object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
#' std <- BetaMC(object, R = 100)
#' # use a large R, for example, R = 20000 for actual research
#' pcor <- PCorBetaMC(std)
#' coef(pcor)
#' @export
#' @keywords methods
coef.pcorbetamc <- function(object,
                            ...) {
  return(
    object$est
  )
}

#' Confidence Intervals for
#' Partial and Semipartial Correlation Coefficients
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @return Returns a matrix of confidence intervals.
#'
#' @param object Object of class `pcorbetamc`.
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
#' pcor <- PCorBetaMC(std)
#' confint(pcor, level = 0.95)
#' @export
#' @keywords methods
confint.pcorbetamc <- function(object,
                               parm = NULL,
                               level = 0.95,
                               ...) {
  if (is.null(parm)) {
    parm <- seq_len(
      length(object$est)
    )
  }
  return(
    .PCorCI(
      object = object,
      alpha = 1 - level[1]
    )[parm, 4:5]
  )
}
