#' Test for a Positive Definite Matrix by Cholesky (Internal)
#'
#' Returns `TRUE` if input
#' is a positive definite matrix,
#' and `FALSE` otherwise.
#'
#' A
#' \eqn{k \times k}
#' symmetric matrix
#' \eqn{\boldsymbol{A}}
#' is taken as positive definite
#' if its Cholesky decomposition succeeds
#' without a warning or an error.
#' No input checks.
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param x Numeric symmetric matrix.
#' @param pivot Logical.
#'   Should pivoting be used? Passed to [chol()].
#' @param tol Numeric.
#'   Tolerance for use with `pivot = TRUE`. Passed to [chol()].
#' @param ... Other arguments passed to [chol()].
#'
#' @references
#'   [Wikipedia: Definite matrix](https://en.wikipedia.org/wiki/Definite_matrix)
#'
#' @return Logical.
#'
#' @examples
#' .TestPositiveDefinite2(x = diag(x = 2, nrow = 2))
#' .TestPositiveDefinite2(x = matrix(data = c(1, 2, 2, 1), nrow = 2))
#'
#' @family Linear Algebra Functions
#' @keywords linearAlgebra test internal
#' @noRd
.TestPositiveDefinite2 <- function(x,
                                   pivot = FALSE,
                                   tol = -1,
                                   ...) {
  out <- tryCatch(
    expr = {
      chol(
        x = x,
        pivot = pivot,
        tol = tol,
        ...
      )
      TRUE
    },
    warning = function(w) {
      FALSE
    },
    error = function(e) {
      FALSE
    }
  )
  out
}
