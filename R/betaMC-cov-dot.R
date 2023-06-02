#' Sampling Covariance Matrix of Parameter Estimates
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @return Returns a matrix.
#'
#' @param lm_process Ouput of the `.ProcessLM()` function.
#' @param jcap Output of the `.J()` function.
#'
#' @inheritParams MC
#'
#' @family Beta Monte Carlo Functions
#' @keywords mc internal
#' @noRd
.Cov <- function(lm_process,
                 type,
                 g1,
                 g2,
                 k,
                 jcap = NULL,
                 fixed_x) {
  if (is.null(jcap)) {
    jcap <- .J(
      lm_process = lm_process,
      rsq = NULL,
      fixed_x = fixed_x
    )
  }
  if (type == "adf") {
    gammacapmvn_consistent <- .GammaN(
      sigmacap = lm_process$sigmacap_consistent,
      pinv_of_dcap = lm_process$pinv_of_dcap
    )
    gammacap <- .GammaADFUnbiased(
      gammacapadf_consistent = .GammaADFConsistent(
        d = .DofMat(
          lm_process$x,
          center = colMeans(lm_process$x),
          n = lm_process$n,
          k = lm_process$k
        ),
        vechsigmacap_consistent = lm_process$vechsigmacap_consistent,
        n = lm_process$n
      ),
      gammacapmvn_consistent = gammacapmvn_consistent,
      vechsigmacap_consistent = lm_process$vechsigmacap_consistent,
      n = lm_process$n
    )
  }
  if (type == "mvn") {
    gammacap <- .GammaN(
      sigmacap = lm_process$sigmacap,
      pinv_of_dcap = lm_process$pinv_of_dcap
    )
  }
  if (type %in% c("adf", "mvn")) {
    # the procedure from here is the same for adf and mvn
    acov <- chol2inv(
      chol(
        .ACovSEMInverse(
          jcap = jcap,
          acov = gammacap
        )
      )
    )
    vcov <- (1 / lm_process$n) * acov
  }
  if (
    type %in% c(
      "hc0",
      "hc1",
      "hc2",
      "hc3",
      "hc4",
      "hc4m",
      "hc5"
    )
  ) {
    gammacap_mvn <- .GammaN(
      sigmacap = lm_process$sigmacap,
      pinv_of_dcap = lm_process$pinv_of_dcap
    )
    gammacap_hc <- .GammaHC(
      d = .DofMat(
        lm_process$x,
        center = colMeans(lm_process$x),
        n = lm_process$n,
        k = lm_process$k
      ),
      sigmacap = lm_process$sigmacap,
      qcap = .QMat(
        h = stats::hatvalues(lm_process$object),
        k = lm_process$k,
        type = type,
        g1 = g1,
        g2 = g2,
        constant = k
      ),
      n = lm_process$n
    )
    avcov <- .ACovHC(
      jcap = jcap,
      gammacap = gammacap_hc,
      gammacap_mvn = gammacap_mvn
    )
    vcov <- .CovHC(
      acov = avcov,
      type = type,
      n = lm_process$n,
      df = lm_process$df
    )
  }
  return(vcov)
}
