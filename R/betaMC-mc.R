#' Generate the Sampling Distribution of Regression Parameters
#' Using the Monte Carlo Method
#'
#' @details Let the parameter vector
#'   of the unstandardized regression model be given by
#'   \deqn{
#'     \boldsymbol{\theta}
#'     =
#'     \left\{
#'     \mathbf{b},
#'     \sigma^{2},
#'     \mathrm{vech}
#'     \left(
#'       \boldsymbol{\Sigma}_{\mathbf{X}\mathbf{X}}
#'     \right)
#'     \right\}
#'   }
#'   where \eqn{\mathbf{b}} is the vector of regression slopes,
#'   \eqn{\sigma^{2}} is the error variance,
#'   and
#'   \eqn{
#'     \mathrm{vech}
#'     \left(
#'       \boldsymbol{\Sigma}_{\mathbf{X}\mathbf{X}}
#'     \right)
#'   }
#'   is the vector of unique elements
#'   of the covariance matrix of the regressor variables.
#'   The empirical sampling distribution
#'   of \eqn{\boldsymbol{\theta}}
#'   is generated using the Monte Carlo method,
#'   that is, random values of parameter estimates
#'   are sampled from the multivariate normal distribution
#'   using the estimated parameter vector as the mean vector
#'   and the specified sampling covariance matrix using the `type` argument
#'   as the covariance matrix.
#'   A replacement sampling approach is implemented
#'   to ensure that the model-implied covariance matrix
#'   is positive definite.
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @return Returns an object
#' of class `mc` which is a list with the following elements:
#'   \describe{
#'     \item{call}{Function call.}
#'     \item{args}{Function arguments.}
#'     \item{lm_process}{Processed `lm` object.}
#'     \item{scale}{Sampling variance-covariance matrix of parameter estimates.}
#'     \item{location}{Parameter estimates.}
#'     \item{thetahatstar}{Sampling distribution of parameter estimates.}
#'     \item{fun}{Function used ("MC").}
#' }
#'
#' @param object Object of class `lm`.
#' @param R Positive integer.
#'   Number of Monte Carlo replications.
#' @param type Character string.
#'   Sampling covariance matrix type.
#'   Possible values are
#'   `"mvn"`,
#'   `"adf"`,
#'   `"hc0"`,
#'   `"hc1"`,
#'   `"hc2"`,
#'   `"hc3"`,
#'   `"hc4"`,
#'   `"hc4m"`, and
#'   `"hc5"`.
#'   `type = "mvn"` uses the normal-theory sampling covariance matrix.
#'   `type = "adf"` uses the asymptotic distribution-free
#'   sampling covariance matrix.
#'   `type = "hc0"` through `"hc5"` uses different versions of
#'   heteroskedasticity-consistent sampling covariance matrix.
#' @param g1 Numeric.
#'   `g1` value for `type = "hc4m"`.
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
#' @param fixed_x Logical.
#'   If `fixed_x = TRUE`, treat the regressors as fixed.
#'   If `fixed_x = FALSE`, treat the regressors as random.
#' @param seed Integer.
#'   Seed number for reproducibility.
#'
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
#'
#' @examples
#' # Data ---------------------------------------------------------------------
#' data("nas1982", package = "betaMC")
#'
#' # Fit Model in lm ----------------------------------------------------------
#' object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
#'
#' # MC -----------------------------------------------------------------------
#' mc <- MC(
#'   object,
#'   R = 100, # use a large value e.g., 20000L for actual research
#'   seed = 0508
#' )
#' mc
#' # The `mc` object can be passed as the first argument
#' # to the following functions
#' #   - BetaMC
#' #   - DeltaRSqMC
#' #   - DiffBetaMC
#' #   - PCorMC
#' #   - RSqMC
#' #   - SCorMC
#'
#' @family Beta Monte Carlo Functions
#' @keywords betaMC mc
#' @export
MC <- function(object,
               R = 20000L,
               type = "hc3",
               g1 = 1,
               g2 = 1.5,
               k = 0.7,
               decomposition = "eigen",
               pd = TRUE,
               tol = 1e-06,
               fixed_x = FALSE,
               seed = NULL) {
  lm_process <- .ProcessLM(object)
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
  scale <- .Cov(
    lm_process = lm_process,
    type = type,
    g1 = g1,
    g2 = g2,
    k = k,
    jcap = .J(
      lm_process = lm_process,
      rsq = NULL,
      fixed_x = fixed_x
    )
  )
  location <- lm_process$theta
  theta <- location
  vechsigmacapx <- lm_process$theta[
    (lm_process$k + 1):lm_process$q
  ]
  if (fixed_x) {
    location <- location[seq_len(lm_process$k)]
  }
  out <- list(
    call = match.call(),
    args = list(
      object = object,
      R = R,
      type = type,
      g1 = g1,
      g2 = g2,
      k = k,
      decomposition = decomposition,
      pd = pd,
      tol = tol,
      fixed_x = fixed_x,
      seed = seed
    ),
    lm_process = lm_process,
    scale = scale,
    location = location,
    theta = theta,
    thetahatstar = .MC(
      scale = scale,
      location = location,
      p = lm_process$p,
      k = lm_process$k,
      q = lm_process$q,
      fixed_x = fixed_x,
      vechsigmacapx = vechsigmacapx,
      R = R,
      decomposition = decomposition,
      pd = pd,
      tol = tol,
      seed = seed
    ),
    fun = "MC"
  )
  class(out) <- c(
    "mc",
    class(out)
  )
  return(
    out
  )
}
