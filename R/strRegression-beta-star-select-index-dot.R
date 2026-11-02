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
#' @family Structure of Regression Functions
#' @keywords strRegression internal
#' @noRd
.BetaStarSelectIndex <- function(std_x,
                                 xnames) {
  if (is.null(std_x)) {
    out <- rep(x = TRUE, times = length(xnames))
  } else {
    stopifnot(
      is.character(std_x),
      all(std_x %in% xnames)
    )
    out <- xnames %in% std_x
  }
  out
}
