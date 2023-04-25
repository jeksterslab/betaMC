.ProcessLMUpdate <- function(beta,
                             sigmasq,
                             vechsigmacapx,
                             n,
                             mi,
                             lm_process) {
  names(beta) <- lm_process$xnames
  sigmacapx <- .SymofVech(
    x = vechsigmacapx,
    k = lm_process$p
  )
  colnames(sigmacapx) <- rownames(sigmacapx) <- lm_process$xnames
  theta <- c(
    beta,
    sigmasq,
    vechsigmacapx
  )
  sigmaysq <- .SigmaYSq(
    beta = beta,
    sigmasq = sigmasq,
    sigmacapx = sigmacapx
  )
  sigmayx <- .SigmaYX(
    beta = beta,
    sigmacapx = sigmacapx
  )
  sigmacap <- rbind(
    sigmayx,
    sigmacapx
  )
  sigmacap <- cbind(
    c(sigmaysq, sigmayx),
    sigmacap
  )
  colnames(sigmacap) <- rownames(sigmacap) <- c(lm_process$varnames)
  vechsigmacap <- .Vech(sigmacap)
  sigma <- sqrt(diag(sigmacap))
  rhocap <- .RhoofSigma(
    sigmacap,
    q = 1 / sigma
  )
  betastar <- .BetaStarofRho(
    rhocap = rhocap,
    k = lm_process$k
  )
  names(betastar) <- lm_process$xnames
  sigmacap_consistent <- (
    sigmacap * (
      n - 1
    ) / n
  )
  vechsigmacap_consistent <- .Vech(
    sigmacap_consistent
  )
  dif <- .Dif(
    beta = beta,
    betastar = betastar,
    p = lm_process$p,
    xnames = lm_process$xnames
  )
  rsq <- .RSqofSigma(
    sigmacap = sigmacap,
    k = lm_process$k
  )
  rsq <- c(
    rsq = rsq,
    adj = 1 - (1 - rsq) * ((n - 1) / (n - lm_process$k))
  )
  lm_process$x <- mi
  lm_process$n <- n
  lm_process$dims <- c(n, lm_process$k)
  lm_process$df <- n - lm_process$k
  lm_process$sigmacap <- sigmacap
  lm_process$vechsigmacap <- vechsigmacap
  lm_process$sigmacapx <- sigmacapx
  lm_process$vechsigmacapx <- vechsigmacapx
  lm_process$sigma <- sigma # standard deviations
  lm_process$sigmacap_consistent <- sigmacap_consistent
  lm_process$vechsigmacap_consistent <- vechsigmacap_consistent
  lm_process$rhocap <- rhocap
  lm_process$betastar <- betastar
  lm_process$beta <- beta
  lm_process$sigmasq <- sigmasq
  lm_process$theta <- unname(theta)
  lm_process$dif_betastar <- dif$dif_betastar
  lm_process$dif_beta <- dif$dif_beta
  lm_process$dif_idx <- dif$dif_idx
  lm_process$rsq <- rsq
  return(lm_process)
}
