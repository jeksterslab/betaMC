#' Create Index for the Parameter Vector
#'
#' @param p Positive integer.
#'   `p` regressors.
#'
#' @family Parameters Functions
#' @keywords strRegression parameters dot
#' @noRd
.ThetaIndex <- function(p) {
  list(
    beta = paste0(
      "beta",
      seq_len(p)
    ),
    sigmasq = "sigmasq",
    vechsigmacapx = paste0(
      "sigma",
      .VechNames(
        x = paste0("x", seq_len(p)),
        sep = ""
      )
    ),
    beta0 = "beta0",
    mux = paste0(
      "mux",
      seq_len(p)
    )
  )
}
