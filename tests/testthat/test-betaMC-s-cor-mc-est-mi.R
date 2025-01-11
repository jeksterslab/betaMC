## ---- test-betaMC-s-cor-mc-est-mi
lapply(
  X = 1,
  FUN = function(i,
                 text,
                 R,
                 tol) {
    message(text)
    testthat::test_that(
      paste(text, "semipartial correlation coefficients"),
      {
        testthat::skip_on_cran()
        set.seed(42)
        if (!exists("nas1982")) {
          try(
            data(
              "nas1982",
              package = "betaMC"
            ),
            silent = TRUE
          )
        }
        df <- nas1982
        object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = df)
        mi <- mice::mice(
          df,
          m = 10,
          seed = 42,
          print = FALSE
        )
        mc <- MCMI(object, mi = mi, R = R, type = "mvn")
        out <- SCorMC(mc)
        print.betamc(out)
        summary.betamc(out)
        coef.betamc(out)
        vcov.betamc(out)
        confint.betamc(out)
        testthat::expect_true(
          all(
            abs(
              coef.betamc(out) - c(
                .4312,
                .3430,
                .2385
              )
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "error"),
      {
        testthat::skip_on_cran()
        set.seed(42)
        if (!exists("nas1982")) {
          try(
            data(
              "nas1982",
              package = "betaMC"
            ),
            silent = TRUE
          )
        }
        df <- nas1982
        object <- lm(QUALITY ~ NARTIC, data = df)
        mi <- mice::mice(
          df,
          m = 10,
          seed = 42,
          print = FALSE
        )
        mc <- MCMI(object, mi = mi, R = R, type = "mvn")
        testthat::expect_error(
          SCorMC(mc)
        )
      }
    )
  },
  text = "test-betaMC-s-cor-mc-est-mi",
  R = 5L,
  tol = 0.0001
)
