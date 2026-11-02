## ---- test-betaMC-f-sq-mc
lapply(
  X = 1,
  FUN = function(i,
                 text,
                 tol) {
    message(text)
    if (!identical(Sys.getenv("NOT_CRAN"), "true") && !interactive()) {
      message("CRAN: tests skipped.")
      # nolint start
      return(invisible(NULL))
      # nolint end
    }
    testthat::test_that(
      paste(text, "f-squared"),
      {
        testthat::skip_on_cran()
        object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
        rsq <- summary(object)$r.squared
        mc <- MC(object, R = 100L, seed = 42)
        out <- FSqMC(mc)
        sr <- coef(SCorMC(mc))
        testthat::expect_true(abs(coef(out)[["fsq"]] - rsq / (1 - rsq)) <= tol)
        testthat::expect_true(
          all(abs(coef(out)[-1] - sr^2 / (1 - rsq)) <= tol)
        )
        draws <- as.matrix(mc$es$rsq)[, 1]
        testthat::expect_true(
          all(abs(out$thetahatstar[, "fsq"] - draws / (1 - draws)) <= tol)
        )
      }
    )
  },
  text = "test-betaMC-f-sq-mc",
  tol = 1e-10
)
