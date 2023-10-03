#' Test for a Positive Definite Matrix
#'
#' Returns `TRUE` if input
#' is a positive definite matrix,
#' and `FALSE` otherwise.
#'
#' A
#' \eqn{k \times k}
#' symmetric matrix
#' \eqn{\mathbf{A}}
#' is positive definite
#' if Cholesky decomposition is successful.
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param x an object for which a method exists.
#'   The default method applies
#'   to numeric (or logical) symmetric,
#'   positive-definite matrices.
#' @param ... arguments to be based to or from methods.
#' @param pivot Should pivoting be used?
#' @param tol A numeric tolerance for use with pivot = TRUE.
#'
#' @references
#'   [Wikipedia: Definite matrix](https://en.wikipedia.org/wiki/Definite_matrix)
#'
#' @return Logical.
#'
#' @family Linear Algebra Functions
#' @keywords linearAlgebra test internal
#' @noRd
.TestPositiveDefinite2 <- function(x,
                                   pivot = FALSE,
                                   tol = -1,
                                   ...) {
  return(
    tryCatch(
      {
        chol(
          x = x,
          pivot = pivot,
          tol = tol,
          ...
        )
        return(TRUE)
      },
      warning = function(w) {
        return(FALSE)
      },
      error = function(e) {
        return(FALSE)
      }
    )
  )
}
