#' The Moore-Penrose Inverse of the Duplication Matrix (Internal)
#'
#' Returns the Moore-Penrose inverse of the duplication matrix,
#' \eqn{\boldsymbol{D}_{k}^{+} =
#' \left( \boldsymbol{D}_{k}^{\prime} \boldsymbol{D}_{k} \right)^{-1}
#' \boldsymbol{D}_{k}^{\prime}},
#' with \eqn{\boldsymbol{D}_{k}^{+}
#' \operatorname{vec} \! \left( \boldsymbol{A} \right) =
#' \operatorname{vech} \! \left( \boldsymbol{A} \right)}
#' for every symmetric \eqn{k \times k} matrix \eqn{\boldsymbol{A}}.
#' \eqn{\boldsymbol{D}_{k}^{\prime} \boldsymbol{D}_{k}} is diagonal,
#' with the column sums of \eqn{\boldsymbol{D}_{k}} on the diagonal,
#' so the inverse is a division of each row of
#' \eqn{\boldsymbol{D}_{k}^{\prime}} by its sum.
#' No input checks: see `PInvDmat()`.
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param d Numeric matrix.
#'   Duplication matrix \eqn{\boldsymbol{D}_{k}}, as returned by `DMat()`.
#'
#' @return Returns a numeric \eqn{k (k + 1) / 2 \times k^2} matrix.
#'
#' @examples
#' .PInvDmat(d = DMat(k = 3))
#'
#' @family Symmetric Functions
#' @keywords linearAlgebra symmetric internal
#' @noRd
.PInvDmat <- function(d) {
  t(x = d) / colSums(x = d)
}
