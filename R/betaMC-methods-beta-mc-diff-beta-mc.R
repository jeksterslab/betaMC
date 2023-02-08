#' @rdname dif
#' @method dif betamc
#' @export
dif.betamc <- function(object, # nolint: object_name_linter
                       ...) {
  if (object$lm_process$p < 2) {
    stop("Two or more regressors is required.")
  }
  est <- object$lm_process$dif_betastar
  p_dif <- dim(object$lm_process$dif_idx)[2]
  dif_betastar <- matrix(
    data = 0.0,
    ncol = p_dif,
    nrow = dim(object$thetahatstar)[1]
  )
  for (i in seq_len(p_dif)) {
    dif_betastar[, i] <- object$thetahatstar[
      ,
      object$lm_process$dif_idx[1, i]
    ] - object$thetahatstar[
      ,
      object$lm_process$dif_idx[2, i]
    ]
  }
  colnames(dif_betastar) <- names(object$lm_process$dif_betastar)
  out <- list(
    fit = object,
    thetahatstar = dif_betastar,
    vcov = stats::var(dif_betastar),
    est = est
  )
  out <- list(
    fit = object,
    thetahatstar = dif_betastar,
    vcov = stats::var(dif_betastar),
    est = est
  )
  class(out) <- c(
    "difbetamc",
    class(out)
  )
  return(
    out
  )
}
