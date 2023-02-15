#' Estimate Partial and Semipartial Correlation Coefficients
#' and Generate the Corresponding Sampling Distribution
#' Using the Monte Carlo Method
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @return Returns an object
#' of class `pcorbetamc` which is a list with the following elements:
#'   \describe{
#'     \item{fit}{The argument `object`.}
#'     \item{thetahatstar}{Sampling distribution of
#'       partial and semipartial correlation coefficients.}
#'     \item{vcov}{Sampling covariance matrix of
#'       partial and semipartial correlation coefficients.}
#'     \item{est}{Vector of
#'       partial and semipartial correlation coefficients.}
#'   }
#'
#' @param object Object of class `betamc`,
#'   that is,
#'   the output of the `BetaMC()` function.
#'
#' @examples
#' object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
#' std <- BetaMC(object, R = 100)
#' # use a large R, for example, R = 20000 for actual research
#' pcor <- PCorBetaMC(std)
#' # Methods -------------------------------------------------------
#' print(pcor)
#' summary(pcor)
#' coef(pcor)
#' vcov(pcor)
#' confint(pcor, level = 0.95)
#' @export
#' @family Beta Monte Carlo Functions
#' @keywords betaMC pcor
PCorBetaMC <- function(object) {
  stopifnot(
    methods::is(
      object,
      "betamc"
    )
  )
  if (object$lm_process$p < 2) {
    stop("Two or more regressors is required.")
  }
  rsq <- lapply(
    X = object$thetahatstar,
    FUN = function(x) {
      return(
        .RSqofSigma(
          sigmacap = x$sigmacap,
          k = object$lm_process$k
        )
      )
    }
  )
  thetahatstar <- mapply(
    theta = object$thetahatstar,
    std = as.data.frame(
      t(
        object$thetahatstar_std
      )
    ),
    rsq = rsq,
    SIMPLIFY = FALSE,
    FUN = function(theta,
                   std,
                   rsq) {
      sr <- .SPCor(
        betastar = std,
        sigmacapx = theta$sigmacapx
      )
      srsq <- sr^2
      prsq <- .PCorSq(
        srsq = sr^2,
        rsq = rsq
      )
      return(
        c(
          sr,
          srsq,
          prsq
        )
      )
    }
  )
  thetahatstar <- do.call(
    what = "rbind",
    args = thetahatstar
  )
  sr <- .SPCor(
    betastar = object$lm_process$betastar,
    sigmacapx = object$lm_process$sigmacap[
      2:object$lm_process$k,
      2:object$lm_process$k,
      drop = FALSE
    ]
  )
  names(sr) <- paste0(
    "*",
    object$lm_process$xnames
  )
  srsq <- sr^2
  names(srsq) <- paste0(
    "^",
    object$lm_process$xnames
  )
  prsq <- .PCorSq(
    srsq = sr^2,
    rsq = object$lm_process$summary_lm$r.squared
  )
  names(prsq) <- paste0(
    "+",
    object$lm_process$xnames
  )
  colnames(thetahatstar) <- c(
    names(sr),
    names(srsq),
    names(prsq)
  )
  est <- c(
    sr,
    srsq,
    prsq
  )
  out <- list(
    fit = object,
    thetahatstar = thetahatstar,
    vcov = stats::var(thetahatstar),
    est = est
  )
  class(out) <- c(
    "pcorbetamc",
    class(out)
  )
  return(
    out
  )
}
