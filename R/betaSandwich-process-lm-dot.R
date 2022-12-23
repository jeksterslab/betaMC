# Process the lm object
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param object Object of class `lm`.
#'
#' @family Beta Sandwich Functions
#' @keywords betaSandwich lm internal
#' @noRd
.ProcessLM <- function(object) {
  stopifnot(
    methods::is(
      object,
      "lm"
    )
  )
  y <- object$model[, 1]
  x <- stats::model.matrix(object)
  beta <- object$coefficients[-1]
  x[, 1] <- y
  varnames <- colnames(x)
  xnames <- varnames[-1]
  dims <- dim(x)
  n <- dims[1]
  k <- dims[2]
  p <- k - 1
  df <- n - k
  q <- p + 1 + 0.5 * p * (p + 1)
  sigmacap <- stats::cov(x)
  vechsigmacap <- .Vech(
    sigmacap
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
  theta <- c(
    beta,
    summary(object)$sigma^2,
    .Vech(sigmacap[2:k, 2:k, drop = FALSE])
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
  list(
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
    sigma = sigma, # standard deviations
    sigmacap_consistent = sigmacap_consistent,
    vechsigmacap_consistent = vechsigmacap_consistent,
    rhocap = rhocap,
    betastar = betastar,
    beta = beta,
    theta = theta
  )
}
