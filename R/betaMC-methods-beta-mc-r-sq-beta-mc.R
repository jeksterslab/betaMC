#' @rdname rsq
#' @method rsq betamc
#' @export
rsq.betamc <- function(object, # nolint: object_name_linter
                       ...) {
  thetahatstar <- object$thetahatstar[
    ,
    (object$lm_process$k):(object$lm_process$k + 1)
  ]
  est <- c(
    rsq = object$lm_process$summary_lm$r.squared,
    adj = object$lm_process$summary_lm$adj.r.squared
  )
  out <- list(
    fit = object,
    thetahatstar = thetahatstar,
    vcov = stats::var(thetahatstar),
    est = est
  )
  class(out) <- c(
    "rsqbetamc",
    class(out)
  )
  return(
    out
  )
}
