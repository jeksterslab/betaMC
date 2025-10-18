#' Generate the Sampling Distribution of Regression Parameters
#' Using the Monte Carlo Method for Data with Missing Values
#'
#' @details Multiple imputation
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
#' @inheritParams MC
#' @param mi Object of class `mids` (output of [mice::mice()]),
#'   object of class `amelia` (output of [Amelia::amelia()]),
#'   or a list of multiply imputed data sets.
#'
#' @references
#' Dudgeon, P. (2017).
#' Some improvements in confidence intervals
#' for standardized regression coefficients.
#' *Psychometrika*, *82*(4), 928–951.
#' \doi{10.1007/s11336-017-9563-z}
#'
#' MacKinnon, D. P., Lockwood, C. M., & Williams, J. (2004).
#' Confidence limits for the indirect effect:
#' Distribution of the product and resampling methods.
#' *Multivariate Behavioral Research*, *39*(1), 99-128.
#' \doi{10.1207/s15327906mbr3901_4}
#'
#' Pesigan, I. J. A., & Cheung, S. F. (2024).
#' Monte Carlo confidence intervals for the indirect effect with missing data.
#' *Behavior Research Methods*.
#' \doi{10.3758/s13428-023-02114-4}
#'
#' Preacher, K. J., & Selig, J. P. (2012).
#' Advantages of Monte Carlo confidence intervals for indirect effects.
#' *Communication Methods and Measures*, *6*(2), 77–98.
#' \doi{10.1080/19312458.2012.679848}
#'
#' @examples
#' # Data ---------------------------------------------------------------------
#' data("nas1982", package = "betaMC")
#' nas1982_missing <- mice::ampute(nas1982)$amp # data set with missing values
#'
#' # Multiple Imputation
#' mi <- mice::mice(nas1982_missing, m = 5, seed = 42, print = FALSE)
#'
#' # Fit Model in lm ----------------------------------------------------------
#' ## Note that this does not deal with missing values.
#' ## The fitted model (`object`) is updated with each imputed data
#' ## within the `MCMI()` function.
#' object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982_missing)
#'
#' # Monte Carlo --------------------------------------------------------------
#' mc <- MCMI(
#'   object,
#'   mi = mi,
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
MCMI <- function(object,
                 mi,
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
  stopifnot(
    inherits(
      x = object,
      what = "lm"
    )
  )
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
  constant <- k
  call0 <- stats::getCall(object)
  call1 <- call0
  data0 <- eval(
    call0$data,
    envir = parent.frame()
  )
  for (i in seq_along(call0)) {
    call1[[i]] <- eval(
      expr = call0[[i]],
      envir = parent.frame()
    )
  }
  if (fixed_x) {
    # check if the X matrix has complete data
    n_data <- dim(data0)[1]
    n_x <- lm_process$x[, -1]
    n_x <- dim(
      n_x[
        stats::complete.cases(n_x), ,
        drop = FALSE
      ]
    )[1]
    if (n_data - n_x > 0) {
      stop(
        paste0(
          "\n",
          "There are missing cases in the matrix of regressors.",
          "\n",
          "Consider using \'fixed_x = FALSE\'.",
          "\n"
        )
      )
    }
  }
  if (
    inherits(
      x = mi,
      what = "mids"
    )
  ) {
    imp <- mice::complete(
      mi,
      action = "all"
    )
  } else if (
    inherits(
      x = mi,
      what = "amelia"
    )
  ) {
    imp <- mi$imputations
  } else if (
    inherits(
      x = mi,
      what = "list"
    )
  ) {
    imp <- mi
  } else {
    stop("Invalid \'mi\' argument.")
  }
  fits <- lapply(
    X = imp,
    FUN = function(x) {
      call1$data <- x
      eval(expr = call1)
    }
  )
  lm_processes <- lapply(
    X = fits,
    FUN = .ProcessLM
  )
  coefs <- lapply(
    X = lm_processes,
    FUN = function(x) {
      x$theta
    }
  )
  theta <- colMeans(
    do.call(
      what = "rbind",
      args = coefs
    )
  )
  vechsigmacapx <- theta[
    (lm_process$k + 1):lm_process$q
  ]
  if (fixed_x) {
    coefs <- lapply(
      X = coefs,
      FUN = function(x) {
        x[seq_len(lm_process$k)]
      }
    )
  }
  vcovs <- lapply(
    X = lm_processes,
    FUN = .Cov,
    type = type,
    g1 = g1,
    g2 = g2,
    k = constant,
    jcap = NULL,
    fixed_x = fixed_x
  )
  pooled <- .MICombine(
    coefs = coefs,
    vcovs = vcovs,
    M = length(coefs),
    k = length(coefs[[1]]),
    adj = TRUE
  )
  # use default total covariance
  scale <- pooled$total
  location <- pooled$est
  mi_output <- list(
    mi = mi,
    lm = fits,
    lm_process = lm_processes
  )
  out <- list(
    call = match.call(),
    args = list(
      object = object,
      mi = mi,
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
      mi_output = mi_output
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
    fun = "MCMI"
  )
  class(out) <- c(
    "mc",
    class(out)
  )
  out
}
