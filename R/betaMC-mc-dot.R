#' Generate the Sampling Distribution of Regression Parameters
#' Using the Monte Carlo Method
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @return Returns a dataframe.
#'
#' @param scale Numeric matrix.
#'   Sampling covariance matrix of the parameter estimates.
#' @param location Numeric vector.
#'   Parameter estimates.
#' @param vechsigmacapx Numeric vector.
#'   Unique elements of the covariance matrix of the regressors.
#' @param p Positive integer.
#'   `p` regressors.
#' @param k Positive integer.
#'   `p` regressors plus 1.
#' @param q Positive integer.
#'   `k` plus the length of `vechsigmacapx`.
#'
#' @inheritParams MC
#'
#' @keywords mc internal
#' @noRd
.MC <- function(scale,
                location,
                vechsigmacapx,
                p,
                k,
                q,
                R = 20000L,
                decomposition = "eigen",
                pd = TRUE,
                tol = 1e-06,
                fixed_x = FALSE,
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
  if (fixed_x) {
    thetahatstar <- cbind(
      thetahatstar,
      t(
        matrix(
          data = vechsigmacapx,
          ncol = dim(thetahatstar)[1],
          nrow = length(vechsigmacapx)
        )
      )
    )
  }
  # replace cases with nonpositive definite model-implied covariance matrix
  # max iterations = iter
  foo <- function(x,
                  iter = 1000L) {
    count <- 0
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
    FUN = foo
  )
  thetahatstar <- unname(
    thetahatstar[!is.na(thetahatstar)]
  )
  return(thetahatstar)
}
