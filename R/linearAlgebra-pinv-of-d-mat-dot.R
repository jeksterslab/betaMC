#' The Moore-Penrose Inverse of the Duplication Matrix
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param d Numeric matrix.
#'   Duplication matrix.
#'
#' @return Returns a matrix.
#' @family Symmetric Functions
#' @keywords linearAlgebra symmetric internal
#' @noRd
.PInvDmat <- function(d) {
  return(
    tcrossprod(
      chol2inv(
        chol(
          crossprod(d)
        )
      ),
      d
    )
  )
}
