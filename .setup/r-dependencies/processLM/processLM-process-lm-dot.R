#' Process the lm object
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param object Object of class `lm`.
#'
#' @family Process lm Functions
#' @keywords processLM lm internal
#' @noRd
.ProcessLM <- function(object) {
  stopifnot(
    methods::is(
      object,
      "lm"
    )
  )
  summary_lm <- summary(object)
  y <- object$model[, 1]
  x <- stats::model.matrix(object)
  x[, 1] <- y
  varnames <- colnames(x)
  xnames <- varnames[-1]
  dims <- dim(x)
  n <- dims[1]
  k <- dims[2]
  p <- k - 1
  df <- n - k
  q <- p + 1 + 0.5 * p * (p + 1)
  beta <- object$coefficients[-1]
  sigmacap <- stats::cov(x)
  vechsigmacap <- .Vech(
    sigmacap
  )
  sigmacapx <- sigmacap[2:k, 2:k, drop = FALSE]
  vechsigmacapx <- .Vech(
    sigmacapx
  )
  sigma <- sqrt(diag(sigmacap))
  rhocap <- .RhoofSigma(
    sigmacap,
    q = 1 / sigma
  )
  betastar <- .BetaStarofRho(
    rhocap = rhocap,
    k = k
  )
  names(betastar) <- xnames
  sigmasq <- summary_lm$sigma^2
  theta <- c(
    beta,
    sigmasq,
    vechsigmacapx
  )
  sigmacap_consistent <- (
    sigmacap * (
      n - 1
    ) / n
  )
  vechsigmacap_consistent <- .Vech(
    sigmacap_consistent
  )
  pinv_of_dcap <- .PInvDmat(.DMat(k))
  dif <- .Dif(
    beta = beta,
    betastar = betastar,
    p = p,
    xnames = xnames
  )
  rsq <- c(
    rsq = summary_lm$r.squared,
    adj = summary_lm$adj.r.squared
  )
  list(
    summary_lm = summary_lm,
    x = x, # {y, X}
    dims = dims,
    n = n,
    k = k,
    p = p,
    q = q,
    df = df,
    pinv_of_dcap = pinv_of_dcap,
    varnames = varnames,
    xnames = xnames,
    sigmacap = sigmacap,
    vechsigmacap = vechsigmacap,
    sigmacapx = sigmacapx,
    vechsigmacapx = vechsigmacapx,
    sigma = sigma, # standard deviations
    sigmacap_consistent = sigmacap_consistent,
    vechsigmacap_consistent = vechsigmacap_consistent,
    rhocap = rhocap,
    betastar = betastar,
    beta = beta,
    sigmasq = sigmasq,
    theta = unname(theta),
    dif_betastar = dif$dif_betastar,
    dif_beta = dif$dif_beta,
    dif_idx = dif$dif_idx,
    rsq = rsq
  )
}
