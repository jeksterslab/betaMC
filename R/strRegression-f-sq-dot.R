#' Cohen's f-Squared and Local f-Squared
#'
#' Computes \eqn{f^2 = R^2 / (1 - R^2)} and the local
#' \eqn{f^2_j = sr^2_j / (1 - R^2)} for each regressor.
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param rsq Numeric vector.
#'   \eqn{R^2} (one value per row of `spcor`).
#' @param spcor Numeric matrix.
#'   Semipartial correlations (one row per value of `rsq`,
#'   one column per regressor).
#' @return Returns a numeric matrix with the column `fsq`
#'   followed by one column per regressor.
#'
#' @family Structure of Regression Functions
#' @keywords strRegression internal
#' @noRd
.FSq <- function(rsq,
                 spcor) {
  rsq <- as.vector(rsq)
  out <- cbind(
    fsq = rsq / (1 - rsq),
    spcor^2 / (1 - rsq)
  )
  colnames(out) <- c("fsq", colnames(spcor))
  out
}
