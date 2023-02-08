#' Print Method for an Object of Class `rsqbetamc`
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param x Object of class `rsqbetamc`.
#' @param ... additional arguments.
#' @param alpha Significance level.
#' @param digits Digits to print.
#' @return Returns a matrix of standardized regression slopes,
#'   standard errors, test statistics, p-values, and confidence intervals.
#' @examples
#' object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
#' std <- BetaMC(object)
#' out <- rsq(std)
#' print(out)
#' @export
#' @keywords methods
print.rsqbetamc <- function(x,
                            alpha = c(0.05, 0.01, 0.001),
                            digits = 4,
                            ...) {
  cat(
    paste0(
      "Multiple correlation ",
      "type = ",
      "\"",
      toupper(x$fit$type),
      "\"",
      "\n"
    )
  )
  base::print(
    round(
      .RSqCI(
        object = x,
        alpha = alpha
      ),
      digits = digits
    )
  )
}

#' Summary Method for an Object of Class `rsqbetamc`
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param object Object of class `rsqbetamc`.
#' @param ... additional arguments.
#' @param alpha Significance level.
#' @param digits Digits to print.
#' @return Returns a matrix of standardized regression slopes,
#'   standard errors, test statistics, p-values, and confidence intervals.
#' @examples
#' object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
#' std <- BetaMC(object)
#' out <- rsq(std)
#' summary(out)
#' @export
#' @keywords methods
summary.rsqbetamc <- function(object,
                              alpha = c(0.05, 0.01, 0.001),
                              digits = 4,
                              ...) {
  cat(
    paste0(
      "Multiple correlation ",
      "type = ",
      "\"",
      toupper(object$fit$type),
      "\"",
      "\n"
    )
  )
  return(
    round(
      .RSqCI(
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
#' @param object Object of class `rsqbetamc`.
#' @param ... additional arguments.
#' @return Returns a matrix of the variance-covariance matrix
#'   of differences of standardized regression slopes.
#' @examples
#' object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
#' std <- BetaMC(object)
#' out <- rsq(std)
#' vcov(out)
#' @export
#' @keywords methods
vcov.rsqbetamc <- function(object,
                           ...) {
  return(
    object$vcov
  )
}

#' Differences of Standardized Regression Slopes
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param object Object of class `rsqbetamc`.
#' @param ... additional arguments.
#' @return Returns a vector of differences of standardized regression slopes.
#' @examples
#' object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
#' std <- BetaMC(object)
#' out <- rsq(std)
#' coef(out)
#' @export
#' @keywords methods
coef.rsqbetamc <- function(object,
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
#' @param object Object of class `rsqbetamc`.
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
#' out <- rsq(std)
#' confint(out, level = 0.95)
#' @export
#' @keywords methods
confint.rsqbetamc <- function(object,
                              parm = NULL,
                              level = 0.95,
                              ...) {
  if (is.null(parm)) {
    parm <- seq_len(
      length(object$est)
    )
  }
  return(
    .RSqCI(
      object = object,
      alpha = 1 - level[1]
    )[parm, 4:5]
  )
}
