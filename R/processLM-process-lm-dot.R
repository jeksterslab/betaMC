#' Process the lm object
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @return Returns a list with the following elements:
#'   \describe{
#'     \item{call}{[lm()] function call.}
#'     \item{object}{Object of class `lm`.}
#'     \item{X}{Model matrix (\eqn{1, X_{1}, \dots, X_{p}} ).}
#'     \item{x}{Data matrix (\eqn{Y, X_{1}, \dots, X_{p}} ).}
#'     \item{varnames}{Variable names of the model matrix.}
#'     \item{xnames}{Variable names of the regressors in the model matrix.}
#'     \item{dims}{Dimensions of the model matrix.}
#'     \item{n}{Sample size.}
#'     \item{p}{Number of regressors.}
#'     \item{k}{`k = p + 1`.}
#'     \item{q}{Length of the parameters in the covariance structure.}
#'     \item{df}{`n - k` degrees of freedom.}
#'     \item{mu}{Mean vector of the model matrix.}
#'     \item{sigmacap}{Covariance matrix of the model matrix.}
#'     \item{vechsigmacap}{Half-vectorization of the covariance matrix
#'       of the model matrix.}
#'     \item{sigmacapx}{Covariance matrix of the regressors
#'       in the model matrix.}
#'     \item{vechsigmacapx}{Half-vectorization of the covariance matrix
#'       of the regressors in the model matrix.}
#'     \item{sigma}{Standard deviation vector of the model matrix.}
#'     \item{sigmacap_consistent}{Consistent estimate of the covariance matrix
#'       of the model matrix.}
#'     \item{vechsigmacap_consistent}{Half-vectorization
#'       of the consistent estimate
#'       of the covariance matrix of the model matrix.}
#'     \item{pinv_of_dcap}{Moore-Penrose inverse of the duplication matrix.}
#'     \item{rhocap}{Correlation matrix of the model matrix.}
#'     \item{coef}{Vector of intercept and partial regression slopes.}
#'     \item{beta0}{Intercept.}
#'     \item{beta}{Vector of partial regression slopes.}
#'     \item{sigmasq}{Error variance.}
#'     \item{theta}{Parameters in the covariance structure,
#'       that is, `beta`, `sigmasq`, `vechsigmacapx`.}
#'     \item{betastar}{Vector of standardized regression slopes.}
#'     \item{scor}{Vector of semipatial correlations.}
#'     \item{pcor}{Vector of squared patial correlations.}
#'     \item{rsq}{Vector of multiple correlation coefficients
#'       (R-squared and adjusted R-squared).}
#'     \item{dif_beta}{Differences of partial regression slopes.}
#'     \item{dif_betastar}{Differences of standardized
#'       partial regression slopes.}
#'     \item{dif_idx}{Differences index.}
#'   }
#'
#' @param object Object of class `lm`.
#'
#' @family Process lm Functions
#' @keywords processLM lm internal
#' @noRd
.ProcessLM <- function(object) {
  stopifnot(
    inherits(
      object,
      "lm"
    )
  )
  # call
  call0 <- stats::getCall(object)
  # data set used by lm
  y <- object$model[, 1]
  x <- stats::model.matrix(object)
  X <- x
  x[, 1] <- y
  varnames <- colnames(x)
  varnames[1] <- colnames(object$model)[1]
  colnames(x) <- varnames
  xnames <- varnames[-1]
  # n, k, p, q, df
  dims <- dim(x)
  n <- dims[1]
  k <- dims[2]
  p <- k - 1
  df <- n - k
  q <- p + 1 + 0.5 * p * (p + 1)
  # moments
  ## means
  mu <- colMeans(x)
  ## covariances
  sigmacap <- stats::cov(x)
  vechsigmacap <- .Vech(
    sigmacap
  )
  sigmacapx <- sigmacap[2:k, 2:k, drop = FALSE]
  vechsigmacapx <- .Vech(
    sigmacapx
  )
  sigma <- sqrt(diag(sigmacap))
  sigmacap_consistent <- (
    sigmacap * (
      n - 1
    ) / n
  )
  vechsigmacap_consistent <- .Vech(
    sigmacap_consistent
  )
  pinv_of_dcap <- .PInvDmat(.DMat(k))
  ## correlations
  rhocap <- .RhoofSigma(
    sigmacap,
    q = 1 / sigma
  )
  ## parameter estimates
  coef <- beta <- object$coefficients
  beta0 <- coef[1]
  beta <- coef[-1]
  sigmasq <- stats::sigma(object)^2
  theta <- unname(
    c(
      beta,
      sigmasq,
      vechsigmacapx
    )
  )
  # effect sizes
  ## standardized partial regression slopes
  betastar <- .BetaStarofRho(
    rhocap = rhocap,
    k = k
  )
  names(betastar) <- xnames
  ## R-squared
  rsq <- .RSqofSigma(
    sigmacap = sigmacap,
    k = k
  )
  adj <- .RSqBar(
    rsq = rsq,
    k = k,
    n = n
  )
  rsq <- c(
    rsq = rsq,
    adj = adj
  )
  ## semi-partial correlations
  ## squared partial correlations
  if (p > 1) {
    scor <- .SPCor(
      betastar = betastar,
      sigmacapx = sigmacapx
    )
    pcor <- .PCorSq(
      srsq = scor^2,
      rsq = rsq[1]
    )
    names(scor) <- xnames
    names(pcor) <- xnames
  } else {
    scor <- NA
    pcor <- NA
  }
  ## differences of slopes
  dif <- .Dif(
    beta = beta,
    betastar = betastar,
    p = p,
    xnames = xnames
  )
  return(
    list(
      # lm
      call = call0,
      object = object,
      # data
      ## data used by lm
      X = X, # {1, X} model matrix
      x = x, # {y, X}
      # names
      varnames = varnames,
      xnames = xnames,
      # dimensions
      dims = dims,
      n = n,
      p = p,
      k = k,
      q = q,
      df = df,
      # moments
      ## means
      mu = mu,
      ## covariances
      sigmacap = sigmacap,
      vechsigmacap = vechsigmacap,
      sigmacapx = sigmacapx,
      vechsigmacapx = vechsigmacapx,
      sigma = sigma, # standard deviations
      sigmacap_consistent = sigmacap_consistent,
      vechsigmacap_consistent = vechsigmacap_consistent,
      pinv_of_dcap = pinv_of_dcap,
      ## correlations
      rhocap = rhocap,
      # parameter estimates
      coef = coef,
      beta0 = beta0,
      beta = beta,
      sigmasq = sigmasq,
      theta = theta,
      # effect sizes
      betastar = betastar,
      scor = scor,
      pcor = pcor,
      rsq = rsq,
      dif_beta = dif$dif_beta,
      dif_betastar = dif$dif_betastar,
      dif_idx = dif$dif_idx
    )
  )
}
