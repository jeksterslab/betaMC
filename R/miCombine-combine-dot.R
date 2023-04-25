#' Confidence Intervals
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param param_vec List of vectors of paramater estimates.
#' @param var_mat List of matrices of sampling variances and covariances.
#' @param m Positive integer.
#'   Number of imputations.
#' @return Returns a list with the following elements:
#' \describe{
#'   \item{`m`}{Number of imputations..}
#'   \item{`est`}{Combined estimates.}
#'   \item{`within`}{Covariance matrix within imputations.}
#'   \item{`between`}{Covariance matrix between imputations.}
#'   \item{`total`}{Total covariance matrix.}
#' }
#' @keywords combine internal
#' @noRd
.Combine <- function(param_vec,
                     var_mat,
                     m) {
  est <- colMeans(
    do.call(
      what = "rbind",
      args = param_vec
    )
  )
  within <- (
    1 / m
  ) * Reduce(
    f = `+`,
    x = var_mat
  )
  between <- (
    1 / (
      m - 1
    )
  ) * Reduce(
    f = `+`,
    x = lapply(
      X = param_vec,
      FUN = function(i,
                     est) {
        tcrossprod(i - est)
      },
      est = est
    )
  )
  colnames(between) <- rownames(between) <- rownames(within)
  total <- within + between + (1 / m) * between
  return(
    list(
      m = m,
      est = est,
      within = within,
      between = between,
      total = total
    )
  )
}
