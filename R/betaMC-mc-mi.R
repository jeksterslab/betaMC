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
#' @inheritParams MC
#' @param adj Logical.
#'   If `adj = TRUE`,
#'   use Li, Raghunathan, and Rubin (1991)
#'   sampling covariance matrix adjustment.
#'   If `adj = FALSE`,
#'   use the multivariate version of Rubin's (1987)
#'   sampling covariance matrix.
#' @param seed_mc Integer.
#'   Random seed for the Monte Carlo method.
#' @param seed_mi Integer.
#'   Random seed for multiple imputation.
#' @param fun Character string.
#'   Multiple imputation function.
#'   If `fun = "mice"`, use [mice::mice()].
#'   If `fun = "amelia"`, use [Amelia::amelia()].
#' @param imp Optional argument.
#'   A list of multiply imputed data sets.
#' @param ... Additional arguments to pass to `fun`.
#'   If `fun = "mice"`, DO NOT supply `data`, `seed`, or `print`.
#'   If `fun = "amelia"`, DO NOT supply `x` or `p2s`.
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
#' MCMI(object, R = 100, M = 5)
#' @export
#' @family Beta Monte Carlo Functions
#' @keywords betaMC mc
MCMI <- function(object,
                 R = 20000L,
                 type = "hc3",
                 g1 = 1,
                 g2 = 1.5,
                 k = 0.7,
                 decomposition = "eigen",
                 pd = TRUE,
                 tol = 1e-06,
                 fixed_x = FALSE,
                 seed_mc = NULL,
                 adj = FALSE,
                 seed_mi = NA,
                 fun = "mice",
                 imp = NULL,
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
  if (is.null(imp)) {
    if (fun == "mice") {
      mi <- mice::complete(
        mice::mice(
          data = data0,
          print = FALSE,
          seed = seed_mi,
          ...
        ),
        action = "all"
      )
    }
    if (fun == "amelia") {
      if (is.na(seed_mi)) {
        seed_mi <- NULL
      }
      set.seed(seed_mi)
      mi <- Amelia::amelia(
        x = data0,
        p2s = 0,
        ...
      )$imputations
    }
  } else {
    stopifnot(
      inherits(
        imp,
        "list"
      )
    )
    mi <- imp
  }
  fits <- lapply(
    X = mi,
    FUN = function(x) {
      call1$data <- x
      return(
        eval(expr = call1)
      )
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
    adj = adj
  )
  if (adj) {
    scale <- pooled$total_adj
  } else {
    scale <- pooled$total
  }
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
      R = R,
      type = type,
      g1 = g1,
      g2 = g2,
      k = k,
      decomposition = decomposition,
      pd = pd,
      tol = tol,
      fixed_x = fixed_x,
      seed_mc = seed_mc,
      adj = adj,
      seed_mi = seed_mi,
      fun = fun,
      imp = imp,
      dots = list(...)
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
      seed = seed_mc
    ),
    mi = mi_output,
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
