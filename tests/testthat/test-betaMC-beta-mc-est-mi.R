## ---- test-betaMC-beta-mc-est-mi
lapply(
  X = 1,
  FUN = function(i,
                 text,
                 R,
                 tol) {
    message(text)
    testthat::test_that(
      paste(text, "standardized regression slopes"),
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
        out <- BetaMC(mc)
        print.betamc(out)
        summary.betamc(out)
        print.summary.betamc(summary.betamc(out))
        coef.betamc(out)
        vcov.betamc(out)
        confint.betamc(out)
        testthat::expect_true(
          all(
            abs(
              coef.betamc(out) - c(
                .4951,
                .3915,
                .2632
              )
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "simple linear regression"),
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
        out <- BetaMC(mc)
        print.betamc(out)
        summary.betamc(out)
        print.summary.betamc(summary.betamc(out))
        coef.betamc(out)
        vcov.betamc(out)
        confint.betamc(out)
        testthat::expect_true(
          TRUE
        )
      }
    )
  },
  text = "test-betaMC-beta-mc-est-mi",
  R = 5L,
  tol = 0.0001
)
