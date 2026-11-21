## ---- test-betaMC-lm-check
lapply(
  X = 1,
  FUN = function(i,
                 text) {
    message(text)
    testthat::test_that(
      desc = paste(text, "unsupported models"),
      code = {
        data("nas1982", package = "betaMC")
        no_intercept <- stats::lm(
          formula = QUALITY ~ 0 + NARTIC + PCTGRT + PCTSUPP,
          data = nas1982
        )
        weighted <- stats::lm(
          formula = QUALITY ~ NARTIC + PCTGRT + PCTSUPP,
          data = nas1982,
          weights = rep(x = c(1, 2), length.out = nrow(x = nas1982))
        )
        offset <- stats::lm(
          formula = QUALITY ~ NARTIC + PCTGRT + offset(PCTSUPP),
          data = nas1982
        )
        testthat::expect_error(
          object = MC(object = no_intercept, R = 10L),
          regexp = "intercept"
        )
        testthat::expect_error(
          object = MC(object = weighted, R = 10L),
          regexp = "weights"
        )
        testthat::expect_error(
          object = MC(object = offset, R = 10L),
          regexp = "offset"
        )
        testthat::expect_error(
          object = MC(object = "not an lm", R = 10L)
        )
        testthat::expect_error(
          object = MCMI(object = no_intercept, mi = list(), R = 10L),
          regexp = "intercept"
        )
        testthat::expect_error(
          object = MCMI(object = weighted, mi = list(), R = 10L),
          regexp = "weights"
        )
        testthat::expect_error(
          object = MCMI(object = offset, mi = list(), R = 10L),
          regexp = "offset"
        )
        ok <- stats::lm(
          formula = QUALITY ~ NARTIC + PCTGRT + PCTSUPP,
          data = nas1982
        )
        testthat::expect_s3_class(
          object = MC(object = ok, R = 10L, seed = 1),
          class = "mc"
        )
      }
    )
  },
  text = "test-betaMC-lm-check"
)
