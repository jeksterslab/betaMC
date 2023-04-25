#' Generate the Sampling Distribution of Regression Parameters
#' Using the Monte Carlo Method for Data with Missing Values
#'
#' @details Multiple imputation (`mice::mice()`)
#' is used to deal with missing values in a data set.
#' The vector of parameter estimates
#' and the corresponding sampling covariance matrix
#' are estimated for each of the imputed data sets.
#' Results are combined to arrive at the pooled vector of parameter estimates
#' and the corresponding sampling covariance matrix.
#' The pooled estimates are then used to generate the sampling distribution
#' of regression parameters.
#' See `MC()` for more details on the Monte Carlo method.
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
#'     \item{fun}{Function used ("MCMI").}
#' }
#'
#' @param ... Additional arguments to pass to `mice::mice()`.
#' @inheritParams MC
#' @inheritParams mice::mice
#'
#' @references
#' Dudgeon, P. (2017).
#' Some improvements in confidence intervals
#' for standardized regression coefficients.
#' *Psychometrika*, *82*(4), 928–951.
#' \doi{10.1007/s11336-017-9563-z}
#'
#' Pesigan, I. J. A., & Cheung, S. F. (2023).
#' Monte Carlo confidence intervals for the indirect effect with missing data.
#' *Behavior Research Methods*.
#' \doi{10.3758/s13428-023-02114-4}
#'
#' Preacher, K. J., & Selig, J. P. (2012).
#' Advantages of Monte Carlo confidence intervals for indirect effects.
#' *Communication Methods and Measures*, *6*(2), 77-98.
#' \doi{10.1080/19312458.2012.679848}
#'
#' @examples
#' set.seed(42)
#' nas1982_missing <- mice::ampute(nas1982)$amp
#' # Fit the regression model
#' ## Note that this does not deal with missing values.
#' ## The fitted model (`object`) is updated with each imputed data
#' ## within the `MCMI()` function.
#' object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982_missing)
#' # Generate the sampling distribution of parameter estimates
#' # (use large values R and m, for example, R = 20000 and m = 100,
#' # for actual research)
#' MCMI(object, R = 100, data = nas1982_missing, m = 5)
#' @export
#' @family Beta Monte Carlo Functions
#' @keywords betaMC mc
MCMI <- function( # mc argumemts
                 object,
                 R = 20000L,
                 type = "hc3",
                 g1 = 1,
                 g2 = 1.5,
                 k = 0.7,
                 decomposition = "eigen",
                 pd = TRUE,
                 tol = 1e-06,
                 fixed_x = FALSE,
                 seed = NULL,
                 # mi arguments
                 data, # data with missing values
                 m = 5,
                 method = NULL,
                 ...) {
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
  n <- dim(data)[1]
  if (is.null(seed)) {
    mi_seed <- NA
  } else {
    mi_seed <- seed
  }
  mi <- mice::complete(
    data = mice::mice(
      data = data,
      m = m,
      method = method,
      print = FALSE,
      seed = mi_seed,
      ...
    ),
    action = "all"
  )
  foo <- function(data,
                  orig_object,
                  type,
                  g1,
                  g2,
                  k,
                  fixed_x) {
    object <- stats::update(
      object = orig_object,
      data = data
    )
    lm_process <- .ProcessLM(object)
    return(
      list(
        coef = lm_process$theta,
        vcov = .Cov(
          object = object,
          type = type,
          g1 = g1,
          g2 = g2,
          k = k,
          lm_process = lm_process,
          jcap = .J(
            lm_process = lm_process,
            rsq = NULL,
            fixed_x = fixed_x
          ),
          fixed_x = fixed_x
        )
      )
    )
  }
  est <- lapply(
    X = mi,
    FUN = foo,
    orig_object = object,
    type = type,
    g1 = g1,
    g2 = g2,
    k = k,
    fixed_x = fixed_x
  )
  # pool estimates
  combine <- .Combine(
    param_vec = lapply(
      X = est,
      FUN = function(x) {
        x$coef
      }
    ),
    var_mat = lapply(
      X = est,
      FUN = function(x) {
        x$vcov
      }
    ),
    m = m
  )
  # update lm_process with multiple imputation estimates
  beta <- combine$est[seq_len(lm_process$p)]
  sigmasq <- combine$est[lm_process$k]
  vechsigmacapx <- combine$est[(lm_process$k + 1):lm_process$q]
  lm_process <- .ProcessLMUpdate(
    beta = beta,
    sigmasq = sigmasq,
    vechsigmacapx = vechsigmacapx,
    n = n,
    mi = mi,
    lm_process = lm_process
  )
  # estimates
  theta <- combine$est
  vechsigmacapx <- theta[
    (lm_process$k + 1):lm_process$q
  ]
  vechsigmacapx <- .SymofVech(
    x = vechsigmacapx,
    k = lm_process$p
  )
  if (fixed_x) {
    theta <- theta[seq_len(lm_process$k)]
  }
  # sampling covariance matrix of estimates
  vcov <- combine$total
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
      seed = seed,
      m = m,
      method = method,
      print = print,
      list(...)
    ),
    lm_process = lm_process,
    scale = vcov,
    location = theta,
    thetahatstar = .MC(
      scale = vcov,
      location = theta,
      vechsigmacapx = vechsigmacapx,
      p = lm_process$p,
      k = lm_process$k,
      q = lm_process$q,
      R = R,
      decomposition = decomposition,
      pd = pd,
      tol = tol,
      fixed_x = fixed_x,
      seed = seed
    ),
    fun = "MCMI"
  )
  class(out) <- c(
    "mc",
    class(out)
  )
  return(
    out
  )
}
