#' Regressors to Standardize
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param std_x Character vector or `NULL`.
#'   Names of the regressors to standardize.
#'   `NULL` standardizes all regressors;
#'   `character(0)` standardizes none.
#' @param xnames Character vector.
#'   Names of the regressors.
#' @return Returns a logical vector along `xnames`.
#'
#' @examples
#' strRegression:::.BetaStarSelectIndex(std_x = "x2", xnames = c("x1", "x2"))
#'
#' @family Structure of Regression Functions
#' @keywords strRegression internal
#' @noRd
.BetaStarSelectIndex <- function(std_x,
                                 xnames) {
  if (is.null(x = std_x)) {
    out <- rep(x = TRUE, times = length(x = xnames))
  } else {
    stopifnot(
      is.character(x = std_x),
      all(std_x %in% xnames)
    )
    out <- xnames %in% std_x
  }
  out
}
