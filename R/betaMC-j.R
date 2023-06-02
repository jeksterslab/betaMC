#' Jacobian Matrix of the Half-Vectorization
#' of the Model-Implied Covariance Matrix
#' with Respect to the Parameter Vector
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param lm_process Ouput of the `.ProcessLM()` function.
#' @param rsq Numeric.
#'   R-squared.
#'   If `rsq = NULL`, the kth element in `theta` is \eqn{R^{2}}.
#'   If `rsq = Numeric`, the kth element in `theta` is \eqn{\sigma^{2}}.
#' @param fixed_x Logical.
#'   If `fixed_x = TRUE`, treat the regressors as fixed.
#'   If `fixed_x = FALSE`, treat the regressors as random.
#'
#' @return Returns a matrix.
#'
#' @family Beta Monte Carlo Functions
#' @keywords mc internal
#' @noRd
.J <- function(lm_process,
               rsq = NULL,
               fixed_x) {
  return(
    .JacobianVechSigmaWRTTheta(
      beta = lm_process$beta,
      sigmacapx = lm_process$sigmacap[
        2:lm_process$k,
        2:lm_process$k,
        drop = FALSE
      ],
      q = lm_process$q,
      p = lm_process$p,
      rsq = rsq,
      fixed_x = fixed_x
    )
  )
}
