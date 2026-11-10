## ---- test-betaMC-f-sq-mc
lapply(
  X = 1,
  FUN = function(i,
                 text,
                 tol) {
    testthat::skip_on_cran()
    message(text)
    testthat::test_that(
      desc = paste(text, "f-squared"),
      code = {
        object <- stats::lm(
          formula = QUALITY ~ NARTIC + PCTGRT + PCTSUPP,
          data = nas1982
        )
        rsq <- summary(object = object)$r.squared
        mc <- MC(object = object, R = 100L, seed = 42)
        out <- FSqMC(object = mc)
        sr <- stats::coef(object = SCorMC(object = mc))
        testthat::expect_true(
          object = abs(
            x = stats::coef(object = out)[["fsq"]] - rsq / (1 - rsq)
          ) <= tol
        )
        testthat::expect_true(
          object = all(
            abs(x = stats::coef(object = out)[-1] - sr^2 / (1 - rsq)) <= tol
          )
        )
        draws <- as.matrix(x = mc$es$rsq)[, 1]
        testthat::expect_true(
          object = all(
            abs(x = out$thetahatstar[, "fsq"] - draws / (1 - draws)) <= tol
          )
        )
      }
    )
  },
  text = "test-betaMC-f-sq-mc",
  tol = 1e-10
)
