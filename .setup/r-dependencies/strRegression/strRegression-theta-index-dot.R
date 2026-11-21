#' Create Index for the Parameter Vector
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param p Positive integer.
#'   `p` regressors.
#'
#' @return Returns a list of names for the elements of the parameter vector.
#'
#' @examples
#' strRegression:::.ThetaIndex(p = 2)
#'
#' @family Parameters Functions
#' @keywords strRegression parameters internal
#' @noRd
.ThetaIndex <- function(p) {
  list(
    beta = paste0(
      "beta",
      seq_len(length.out = p)
    ),
    sigmasq = "sigmasq",
    vechsigmacapx = paste0(
      "sigma",
      .VechNames(
        x = paste0("x", seq_len(length.out = p)),
        sep = ""
      )
    ),
    beta0 = "beta0",
    mux = paste0(
      "mux",
      seq_len(length.out = p)
    )
  )
}
