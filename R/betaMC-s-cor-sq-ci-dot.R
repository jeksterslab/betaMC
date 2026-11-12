#' Square a Signed Confidence Interval as a Set
#'
#' Retain the entire image under squaring, including zero when covered.
#' @author Ivan Jacob Agaloos Pesigan
#' @param est Named signed estimates.
#' @param ci Matrix with ordered lower and upper endpoints.
#' @return Data frame with squared estimates, lower and upper limits.
#' @examples
#' .SCorSqCI(est = c(x = 0), ci = matrix(c(-0.2, 0.3),
#'   nrow = 1,
#'   dimnames = list("x", c("lower", "upper"))
#' ))
#' @noRd
.SCorSqCI <- function(est, ci) {
  stopifnot(
    is.matrix(x = ci), ncol(x = ci) == 2L,
    !is.null(x = rownames(x = ci)),
    all(x = rownames(x = ci) %in% names(x = est)),
    all(x = ci[, 1] <= ci[, 2], na.rm = TRUE)
  )
  lo <- pmin(ci[, 1]^2, ci[, 2]^2)
  cross <- !is.na(x = ci[, 1]) & !is.na(x = ci[, 2]) &
    ci[, 1] <= 0 & ci[, 2] >= 0
  lo[cross] <- 0
  data.frame(
    est = unname(obj = est[rownames(x = ci)]^2),
    lower = unname(obj = lo),
    upper = unname(obj = pmax(ci[, 1]^2, ci[, 2]^2)),
    row.names = rownames(x = ci)
  )
}
