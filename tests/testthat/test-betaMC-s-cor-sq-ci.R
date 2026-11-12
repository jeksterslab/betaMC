## ---- test-betaMC-s-cor-sq-ci
lapply(
  X = 1,
  FUN = function(i, text) {
    testthat::skip_on_cran()
    testthat::test_that(
      desc = paste(text, "handles sign, zero, and failed endpoints"),
      code = {
        ci <- matrix(
          data = c(-0.6, -0.2, 0.1, 0.4, -0.2, 0.3, 0, 0.5, NA, 0.4),
          ncol = 2, byrow = TRUE,
          dimnames = list(letters[1:5], c("lower", "upper"))
        )
        est <- stats::setNames(
          object = c(-0.4, 0.2, 0, 0.1, 0.1),
          nm = letters[1:5]
        )
        out <- .SCorSqCI(est = est, ci = ci)
        testthat::expect_equal(
          object = out$lower,
          expected = c(0.04, 0.01, 0, 0, NA)
        )
        testthat::expect_equal(
          object = out$upper,
          expected = c(0.36, 0.16, 0.09, 0.25, NA)
        )
        testthat::expect_equal(object = out$est, expected = unname(obj = est^2))
      }
    )
  },
  text = "test-betaMC-s-cor-sq-ci"
)
