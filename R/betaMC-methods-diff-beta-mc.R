#' Print Method for an Object of Class `diffbetamc`
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @return Returns a matrix of
#'   differences of standardized regression slopes,
#'   standard errors,
#'   number of Monte Carlo replications,
#'   and
#'   confidence intervals.
#'
#' @param x Object of class `diffbetamc`.
#' @param ... additional arguments.
#' @param alpha Significance level.
#' @param digits Digits to print.
#'
#' @examples
#' object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
#' std <- BetaMC(object)
#' diff <- DiffBetaMC(std)
#' print(diff)
#' @export
#' @keywords methods
print.diffbetamc <- function(x,
                             alpha = c(0.05, 0.01, 0.001),
                             digits = 4,
                             ...) {
  cat(
    paste0(
      "Difference between standardized regression coefficients\n",
      "type = ",
      "\"",
      x$fit$type,
      "\"",
      "\n"
    )
  )
  base::print(
    round(
      .DiffBetaCI(
        object = x,
        alpha = alpha
      ),
      digits = digits
    )
  )
}

#' Summary Method for an Object of Class `diffbetamc`
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @return Returns a matrix of
#'   differences of standardized regression slopes,
#'   standard errors,
#'   number of Monte Carlo replications,
#'   and
#'   confidence intervals.
#'
#' @param object Object of class `diffbetamc`.
#' @param ... additional arguments.
#' @param alpha Significance level.
#' @param digits Digits to print.
#'
#' @examples
#' object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
#' std <- BetaMC(object)
#' diff <- DiffBetaMC(std)
#' summary(diff)
#' @export
#' @keywords methods
summary.diffbetamc <- function(object,
                               alpha = c(0.05, 0.01, 0.001),
                               digits = 4,
                               ...) {
  cat(
    paste0(
      "Difference between standardized regression coefficients\n",
      "type = ",
      "\"",
      object$fit$type,
      "\"",
      "\n"
    )
  )
  return(
    round(
      .DiffBetaCI(
        object = object,
        alpha = alpha
      ),
      digits = digits
    )
  )
}

#' Sampling Covariance Matrix of
#' Differences of Standardized Regression Slopes
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @return Returns a matrix of the
#'   variance-covariance matrix
#'   of differences of standardized regression slopes.
#'
#' @param object Object of class `diffbetamc`.
#' @param ... additional arguments.
#'
#' @examples
#' object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
#' std <- BetaMC(object)
#' diff <- DiffBetaMC(std)
#' vcov(diff)
#' @export
#' @keywords methods
vcov.diffbetamc <- function(object,
                            ...) {
  return(
    object$vcov
  )
}

#' Differences of Standardized Regression Slopes
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @return Returns a vector of differences of standardized regression slopes.
#'
#' @param object Object of class `diffbetamc`.
#' @param ... additional arguments.
#'
#' @examples
#' object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
#' std <- BetaMC(object)
#' diff <- DiffBetaMC(std)
#' coef(diff)
#' @export
#' @keywords methods
coef.diffbetamc <- function(object,
                            ...) {
  return(
    object$est
  )
}

#' Confidence Intervals for Differences
#' of Standardized Regression Slopes
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @return Returns a matrix of confidence intervals.
#'
#' @param object Object of class `diffbetamc`.
#' @param ... additional arguments.
#' @param parm a specification of which parameters
#'   are to be given confidence intervals,
#'   either a vector of numbers or a vector of names.
#'   If missing, all parameters are considered.
#' @param level the confidence level required.
#'
#' @examples
#' object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
#' std <- BetaMC(object)
#' diff <- DiffBetaMC(std)
#' confint(diff, level = 0.95)
#' @export
#' @keywords methods
confint.diffbetamc <- function(object,
                               parm = NULL,
                               level = 0.95,
                               ...) {
  if (is.null(parm)) {
    parm <- seq_len(
      length(object$est)
    )
  }
  return(
    .DiffBetaCI(
      object = object,
      alpha = 1 - level[1]
    )[parm, 4:5]
  )
}
