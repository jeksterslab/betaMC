#' Create Index for Moments Vector
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param p Positive integer.
#'   `p` regressors.
#'
#' @return Returns a list of indices.
#' @examples
#' strRegression:::.MomentsIndex(p = 2)
#'
#' @family Moments Functions
#' @keywords strRegression moments internal
#' @noRd
.MomentsIndex <- function(p) {
  list(
    sigmaysq = "sigmaysq",
    sigmayx = paste0(
      "sigmayx",
      seq_len(length.out = p)
    ),
    vechsigmacapx = paste0(
      "sigma",
      .VechNames(
        x = paste0("x", seq_len(length.out = p)),
        sep = ""
      )
    ),
    muy = "muy",
    mux = paste0(
      "mux",
      seq_len(length.out = p)
    )
  )
}
