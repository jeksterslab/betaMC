#' Print Method for an Object of Class `rsqbetamc`
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @return Returns a matrix of
#'   multiple correlation coefficients
#'   (R-squared and adjusted R-squared),
#'   standard errors,
#'   number of Monte Carlo replications,
#'   and
#'   confidence intervals.
#'
#' @param x Object of class `rsqbetamc`.
#' @param ... additional arguments.
#' @param alpha Significance level.
#' @param digits Digits to print.
#'
#' @examples
#' object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
#' std <- BetaMC(object, R = 100)
#' # use a large R, for example, R = 20000 for actual research
#' rsq <- RSqBetaMC(std)
#' print(rsq)
#' @export
#' @keywords methods
print.rsqbetamc <- function(x,
                            alpha = c(0.05, 0.01, 0.001),
                            digits = 4,
                            ...) {
  cat(
    paste0(
      "Multiple correlation\n",
      "type = ",
      "\"",
      x$fit$type,
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
#' @return Returns a matrix of
#'   multiple correlation coefficients
#'   (R-squared and adjusted R-squared),
#'   standard errors,
#'   number of Monte Carlo replications,
#'   and
#'   confidence intervals.
#'
#' @param object Object of class `rsqbetamc`.
#' @param ... additional arguments.
#' @param alpha Significance level.
#' @param digits Digits to print.
#'
#' @examples
#' object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
#' std <- BetaMC(object, R = 100)
#' # use a large R, for example, R = 20000 for actual research
#' rsq <- RSqBetaMC(std)
#' summary(rsq)
#' @export
#' @keywords methods
summary.rsqbetamc <- function(object,
                              alpha = c(0.05, 0.01, 0.001),
                              digits = 4,
                              ...) {
  cat(
    paste0(
      "Multiple correlation\n",
      "type = ",
      "\"",
      object$fit$type,
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
#' Multiple Correlation Coefficients
#' (R-Squared and Adjusted R-Squared)
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @return Returns a matrix of the
#'   variance-covariance matrix
#'   of multiple correlation coefficients
#'   (R-squared and adjusted R-squared).
#'
#' @param object Object of class `rsqbetamc`.
#' @param ... additional arguments.
#'
#' @examples
#' object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
#' std <- BetaMC(object, R = 100)
#' # use a large R, for example, R = 20000 for actual research
#' rsq <- RSqBetaMC(std)
#' vcov(rsq)
#' @export
#' @keywords methods
vcov.rsqbetamc <- function(object,
                           ...) {
  return(
    object$vcov
  )
}

#' Multiple Correlation Coefficients
#' (R-Squared and Adjusted R-Squared)
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @return Returns a vector of multiple correlation coefficients
#' (R-squared and adjusted R-squared)
#'
#' @param object Object of class `rsqbetamc`.
#' @param ... additional arguments.
#'
#' @examples
#' object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
#' std <- BetaMC(object, R = 100)
#' # use a large R, for example, R = 20000 for actual research
#' rsq <- RSqBetaMC(std)
#' coef(rsq)
#' @export
#' @keywords methods
coef.rsqbetamc <- function(object,
                           ...) {
  return(
    object$est
  )
}

#' Confidence Intervals for
#' Multiple Correlation Coefficients
#' (R-Squared and Adjusted R-Squared)
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @return Returns a matrix of confidence intervals.
#'
#' @param object Object of class `rsqbetamc`.
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
#' rsq <- RSqBetaMC(std)
#' confint(rsq, level = 0.95)
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
