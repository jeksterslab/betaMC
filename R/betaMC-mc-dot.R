#' Generate the Sampling Distribution of Regression Parameters
#' Using the Monte Carlo Method
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @return Returns a dataframe.
#'
#' @param scale Numeric matrix.
#'   Sampling covariance matrix of the parameter estimates.
#'   If `fixed_x = TRUE`, scale should exclude `vechsigmacapx`.
#' @param location Numeric vector.
#'   Parameter estimates.
#'   If `fixed_x = TRUE`, scale should exclude `vechsigmacapx`.
#' @param vechsigmacapx Numeric vector.
#'   If `fixed_x = TRUE`,
#'   unique elements of the covariance matrix of the regressors.
#' @param p Positive integer.
#'   `p` regressors.
#' @param k Positive integer.
#'   `p` regressors plus 1.
#' @param q Positive integer.
#'   `k` plus the length of `vechsigmacapx`.
#'
#' @inheritParams MC
#'
#' @family Beta Monte Carlo Functions
#' @keywords mc internal
#' @noRd
.MC <- function(scale,
                location,
                p,
                k,
                q,
                fixed_x = FALSE,
                vechsigmacapx,
                R = 20000L,
                decomposition = "eigen",
                pd = TRUE,
                tol = 1e-06,
                seed = NULL) {
  set.seed(seed)
  thetahatstar <- .ThetaHatStar(
    R = R,
    scale = scale,
    location = location,
    decomposition = decomposition,
    pd = pd,
    tol = tol
  )$thetahatstar
  # replace cases with nonpositive definite model-implied covariance matrix
  # max iterations = iter
  foo <- function(x,
                  p,
                  k,
                  q,
                  fixed_x,
                  vechsigmacapx,
                  iter = 1000L) {
    count <- 0
    if (fixed_x) {
      x <- c(
        x,
        vechsigmacapx
      )
    }
    params <- .MCThetaHat(
      thetahat = x,
      p = p,
      k = k,
      q = q
    )
    pd <- params$pd
    while (!pd) {
      x <- .Vec(
        .ThetaHatStar(
          R = 1,
          scale = scale,
          location = location,
          decomposition = decomposition,
          pd = FALSE
        )$thetahatstar
      )
      if (fixed_x) {
        x <- c(
          x,
          vechsigmacapx
        )
      }
      params <- .MCThetaHat(
        thetahat = x,
        p = p,
        k = k,
        q = q
      )
      pd <- params$pd
      count <- count + 1
      if (count >= iter) {
        return(NA)
      }
    }
    return(params)
  }
  thetahatstar <- lapply(
    X = as.data.frame(
      t(
        thetahatstar
      )
    ),
    FUN = foo,
    p = p,
    k = k,
    q = q,
    fixed_x = fixed_x,
    vechsigmacapx = vechsigmacapx
  )
  thetahatstar <- unname(
    thetahatstar[!is.na(thetahatstar)]
  )
  return(thetahatstar)
}
