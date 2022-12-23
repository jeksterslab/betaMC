#' Estimate Standardized Regression Coefficients
#' and Generate Sampling Distributions
#' Using the Monte Carlo Method
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @return Returns an object of class `betamc`
#'   which is a list with the following elements:
#' \describe{
#'   \item{call}{Function call.}
#'   \item{lm}{Object of class `lm`.}
#'   \item{type}{Standard error type.}
#'   \item{beta}{Vector of standardized slopes.}
#'   \item{vcov}{Sampling covariance matrix of the standardized slopes.}
#'   \item{thetahatstar}{Sampling distribution
#'                         of estimates of standardized slopes.}
#'   \item{n}{Sample size.}
#'   \item{p}{Number of regressors.}
#'   \item{df}{\eqn{n - p - 1} degrees of freedom.}
#' }
#' @param object Object of class `lm`.
#' @param R Positive integer.
#'   Number of Monte Carlo replications.
#' @param type Character string.
#'   Correction type.
#'   Possible values are
#'   `"adf"`,
#'   `"hc0"`,
#'   `"hc1"`,
#'   `"hc2"`,
#'   `"hc3"`,
#'   `"hc4"`,
#'   `"hc4m"`,
#'   `"hc5"`, and
#'   `"mvn"`.
#' @param g1 Numeric.
#'   `g1` value for `type = "hc4m"` or `type = "hc5"`.
#' @param g2 Numeric.
#'   `g2` value for `type = "hc4m"`.
#' @param k Numeric.
#'   Constant for `type = "hc5"`
#' @param decomposition Character string.
#'   Matrix decomposition of the sampling variance-covariance matrix
#'   for the data generation.
#'   If `decomposition = "chol"`, use Cholesky decomposition.
#'   If `decomposition = "eigen"`, use eigenvalue decomposition.
#'   If `decomposition = "svd"`, use singular value decomposition.
#' @param pd Logical.
#'   If `pd = TRUE`,
#'   check if the sampling variance-covariance matrix
#'   is positive definite using `tol`.
#' @param tol Numeric.
#'   Tolerance used for `pd`.
#' @references
#' Dudgeon, P. (2017).
#' Some improvements in confidence intervals
#' for standardized regression coefficients.
#' *Psychometrika*, *82*(4), 928–951.
#' \doi{10.1007/s11336-017-9563-z}
#'
#' Preacher, K. J., & Selig, J. P. (2012).
#' Advantages of Monte Carlo confidence intervals for indirect effects.
#' *Communication Methods and Measures*, *6*(2), 77-98.
#' \doi{10.1080/19312458.2012.679848}
#' @examples
#' object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
#' std <- BetaMC(object)
#' # Methods -------------------------------------------------------
#' print(std)
#' summary(std)
#' coef(std)
#' vcov(std)
#' confint(std, level = 0.95)
#' @export
#' @family Beta Monte Carlo Functions
#' @keywords betaMC
BetaMC <- function(object,
                   R = 20000L,
                   type = "hc3",
                   g1 = 1,
                   g2 = 1.5,
                   k = 0.7,
                   decomposition = "eigen",
                   pd = TRUE,
                   tol = 1e-06) {
  input <- .ProcessLM(object)
  stopifnot(
    type %in% c(
      "adf",
      "hc0",
      "hc1",
      "hc2",
      "hc3",
      "hc4",
      "hc4m",
      "hc5",
      "mvn"
    )
  )
  stopifnot(0 < k & k < 1)
  constant <- k
  jcap <- .JacobianVechSigmaWRTTheta(
    beta = input$beta,
    sigmacapx = input$sigmacap[2:input$k, 2:input$k, drop = FALSE],
    q = input$q,
    p = input$p
  )
  if (type == "adf") {
    gammacapmvn_consistent <- .GammaN(
      sigmacap = input$sigmacap_consistent,
      pinv_of_dcap = input$pinv_of_dcap
    )
    gammacap <- .GammaADFUnbiased(
      gammacapadf_consistent = .GammaADFConsistent(
        d = .DofMat(
          input$x,
          center = colMeans(input$x),
          n = input$n,
          k = input$k
        ),
        vechsigmacap_consistent = input$vechsigmacap_consistent,
        n = input$n
      ),
      gammacapmvn_consistent = gammacapmvn_consistent,
      vechsigmacap_consistent = input$vechsigmacap_consistent,
      n = input$n
    )
  }
  if (type == "mvn") {
    gammacap <- .GammaN(
      sigmacap = input$sigmacap,
      pinv_of_dcap = input$pinv_of_dcap
    )
  }
  if (type %in% c("adf", "mvn")) {
    # the procedure from here is the same for adf and mvn
    avcov <- .ACovN(
      jcap = jcap,
      gammacap_mvn = gammacap
    )
    vcov <- .CovN(
      acov = avcov,
      n = input$n
    )
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
      sigmacap = input$sigmacap,
      pinv_of_dcap = input$pinv_of_dcap
    )
    gammacap_hc <- .GammaHC(
      d = .DofMat(
        input$x,
        center = colMeans(input$x),
        n = input$n,
        k = input$k
      ),
      sigmacap = input$sigmacap,
      qcap = .QMat(
        h = stats::hatvalues(object),
        k = input$k,
        type = type,
        g1 = g1,
        g2 = g2,
        constant = constant
      ),
      n = input$n
    )
    avcov <- .ACovHC(
      jcap = jcap,
      gammacap = gammacap_hc,
      gammacap_mvn = gammacap_mvn
    )
    vcov <- .CovHC(
      acov = avcov,
      type = type,
      n = input$n,
      df = input$df
    )
  }
  thetahatstar <- .ThetaHatStar(
    R = R,
    scale = vcov,
    location = input$theta,
    decomposition = decomposition,
    pd = pd,
    tol = tol
  )$thetahatstar
  # rerun cases with negative variances
  # max iterations = counter_max
  thetahatstar <- lapply(
    X = as.data.frame(
      t(
        thetahatstar
      )
    ),
    FUN = function(x) {
      beta <- x[1:input$p]
      sigmasq <- x[input$k]
      sigmacapx <- matrix(
        data = 0,
        nrow = input$p,
        ncol = input$p
      )
      sigmacapx[lower.tri(sigmacapx, diag = TRUE)] <- x[
        (input$k + 1):input$q
      ]
      sigmacapx[upper.tri(sigmacapx)] <- t(sigmacapx)[upper.tri(sigmacapx)]
      sigmasqx <- diag(sigmacapx)
      counter_max <- 100000
      count <- 0
      while (
        any(
          c(
            sigmasq,
            sigmasqx
          ) <= 0
        )
      ) {
        x <- .Vec(
          .ThetaHatStar(
            R = 1,
            scale = vcov,
            location = input$theta,
            decomposition = decomposition,
            pd = FALSE
          )$thetahatstar
        )
        beta <- x[1:input$p]
        sigmasq <- x[input$k]
        sigmacapx[lower.tri(sigmacapx, diag = TRUE)] <- x[
          (input$k + 1):input$q
        ]
        sigmacapx[upper.tri(sigmacapx)] <- t(sigmacapx)[upper.tri(sigmacapx)]
        sigmasqx <- diag(sigmacapx)
        if (count >= counter_max) {
          return(
            rep(x = NA, times = input$p)
          )
        }
      }
      return(
        (
          sqrt(
            sigmasqx
          ) / sqrt(
            .SigmaYSq(
              beta = beta,
              sigmasq = sigmasq,
              sigmacapx = sigmacapx
            )
          )
        ) * beta
      )
    }
  )
  thetahatstar <- do.call(
    what = "rbind",
    args = thetahatstar
  )
  colnames(thetahatstar) <- input$xnames
  rownames(thetahatstar) <- NULL
  out <- list(
    call = match.call(),
    lm = object,
    type = type,
    beta = input$betastar,
    vcov = stats::var(thetahatstar),
    thetahatstar = thetahatstar,
    n = input$n,
    p = input$p,
    df = input$df
  )
  class(out) <- c(
    "betamc",
    class(out)
  )
  return(
    out
  )
}
