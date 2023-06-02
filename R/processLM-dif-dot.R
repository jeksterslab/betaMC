#' Differences of Regression Coefficients
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param beta Numeric vector.
#'   Partial regression slopes
#'   \eqn{\boldsymbol{\beta}}.
#' @param betastar Numeric vector.
#'   Standardized partial regression slopes
#'   \eqn{\boldsymbol{\beta}^{\ast}}.
#' @param p Positive integer.
#'   `p` regressors.
#' @param xnames Character vector.
#'   Column names of regressors.
#'
#' @family Process lm Functions
#' @keywords processLM lm internal
#' @noRd
.Dif <- function(beta,
                 betastar,
                 p,
                 xnames) {
  if (p > 1) {
    dif_idx <- utils::combn(seq_len(p), 2)
    p_dif <- dim(dif_idx)[2]
    dif_betastar <- rep(x = 0.0, times = p_dif)
    dif_beta <- rep(x = 0.0, times = p_dif)
    dif_names <- rep(x = 0.0, times = p_dif)
    for (i in seq_len(p_dif)) {
      dif_betastar[i] <- betastar[dif_idx[1, i]] - betastar[dif_idx[2, i]]
      dif_beta[i] <- beta[dif_idx[1, i]] - beta[dif_idx[2, i]]
      dif_names[i] <- paste0(
        xnames[dif_idx[1, i]],
        "-",
        xnames[dif_idx[2, i]]
      )
    }
    names(dif_betastar) <- dif_names
    names(dif_beta) <- dif_names
  } else {
    dif_betastar <- NA
    dif_beta <- NA
    dif_idx <- NA
  }
  return(
    list(
      dif_beta = dif_beta,
      dif_betastar = dif_betastar,
      dif_idx = dif_idx
    )
  )
}
