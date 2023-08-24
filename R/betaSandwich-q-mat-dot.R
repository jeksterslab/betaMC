#' Leverage Adjustment (\eqn{\mathbf{Q}})
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param h Numeric vector.
#'   Leverage values.
#' @param k Positive integer.
#'   `p` number of regressors plus 1.
#' @param type Character string.
#'   Correction type.
#'   Possible values are
#'   `"hc0"`,
#'   `"hc1"`,
#'   `"hc2"`,
#'   `"hc3"`,
#'   `"hc4"`,
#'   `"hc4m"`, and
#'   `"hc5"`.
#' @param g1 Numeric.
#'   `g1` value for `type = "hc4m"`.
#' @param g2 Numeric.
#'   `g2` value for `type = "hc4m"`.
#' @param constant Numeric.
#'   Constant `k` for `type = "hc5"`
#'   \eqn{0 \leq k \leq 1}.
#'
#' @family Beta Sandwich Functions
#' @keywords betaSandwich leverage internal
#' @noRd
.QMat <- function(h,
                  k,
                  type = "hc3",
                  g1 = 1,
                  g2 = 1.5,
                  constant = 0.7) {
  n <- length(h)
  if (type %in% c("hc0", "hc1")) {
    return(
      rep(
        x = 1,
        times = n
      )
    )
  }
  if (type == "hc2") {
    return(
      1 / (
        (1 - h)^1
      )
    )
  }
  if (type == "hc3") {
    return(
      1 / (
        (1 - h)^2
      )
    )
  }
  if (type == "hc4") {
    delta <- sapply(
      X = h,
      FUN = function(i) {
        return(
          min(
            4,
            (n * i / k)
          )
        )
      }
    )
    return(
      1 / (
        (1 - h)^delta
      )
    )
  }
  if (type == "hc4m") {
    lambda <- sapply(
      X = h,
      FUN = function(i) {
        tmp <- n * i / k
        return(
          min(
            g1,
            tmp
          ) + min(
            g2,
            tmp
          )
        )
      }
    )
    return(
      1 / (
        (1 - h)^lambda
      )
    )
  }
  if (type == "hc5") {
    tmp <- n * constant * max(h) / k
    gamma <- sapply(
      X = h,
      FUN = function(i) {
        return(
          min(
            (n * i / k),
            max(
              4,
              tmp
            )
          )
        )
      }
    )
    return(
      1 / sqrt(
        (1 - h)^gamma
      )
    )
  }
}
