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
    dif_idx <- utils::combn(
      x = seq_len(length.out = p),
      m = 2
    )
    first <- dif_idx[1, ]
    second <- dif_idx[2, ]
    dif_betastar <- betastar[first] - betastar[second]
    dif_beta <- beta[first] - beta[second]
    dif_names <- paste0(
      xnames[first],
      "-",
      xnames[second]
    )
    names(x = dif_betastar) <- dif_names
    names(x = dif_beta) <- dif_names
  } else {
    dif_betastar <- NA
    dif_beta <- NA
    dif_idx <- NA
  }
  list(
    dif_beta = dif_beta,
    dif_betastar = dif_betastar,
    dif_idx = dif_idx
  )
}
