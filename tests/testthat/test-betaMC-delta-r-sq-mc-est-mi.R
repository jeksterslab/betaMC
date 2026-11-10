## ---- test-betaMC-delta-r-sq-mc-est-mi
lapply(
  X = 1,
  FUN = function(i,
                 text,
                 R,
                 tol) {
    testthat::skip_on_cran()
    message(text)

    set.seed(seed = 42)

    testthat::test_that(
      desc = paste(text, "improvement in R-squared"),
      code = {
        set.seed(seed = 42)
        if (!exists(x = "nas1982")) {
          try(
            expr = utils::data(
              "nas1982",
              package = "betaMC"
            ),
            silent = TRUE
          )
        }
        df <- nas1982
        object <- stats::lm(
          formula = QUALITY ~ NARTIC + PCTGRT + PCTSUPP,
          data = df
        )
        mi <- mice::mice(
          data = df,
          m = 10,
          seed = 42,
          print = FALSE
        )
        mc <- MCMI(object = object, mi = mi, R = R, type = "mvn")
        out <- DeltaRSqMC(object = mc)
        print.betamc(x = out)
        summary.betamc(object = out)
        print.summary.betamc(x = summary.betamc(object = out))
        coef.betamc(object = out)
        vcov.betamc(object = out)
        confint.betamc(object = out)
        testthat::expect_true(
          object = all(
            abs(
              x = coef.betamc(object = out) - c(
                .1859,
                .1177,
                .0569
              )
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      desc = paste(text, "error"),
      code = {
        set.seed(seed = 42)
        if (!exists(x = "nas1982")) {
          try(
            expr = utils::data(
              "nas1982",
              package = "betaMC"
            ),
            silent = TRUE
          )
        }
        df <- nas1982
        object <- stats::lm(formula = QUALITY ~ NARTIC, data = df)
        mi <- mice::mice(
          data = df,
          m = 10,
          seed = 42,
          print = FALSE
        )
        mc <- MCMI(object = object, mi = mi, R = R, type = "mvn")
        testthat::expect_error(
          object = DeltaRSqMC(object = mc)
        )
      }
    )
  },
  text = "test-betaMC-delta-r-sq-mc-est-mi",
  R = 5L,
  tol = 0.0001
)
