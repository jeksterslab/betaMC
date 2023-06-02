## ---- test-betaMC-p-cor-mc-est-mi
lapply(
  X = 1,
  FUN = function(i,
                 text,
                 R,
                 tol) {
    message(text)
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
    out <- PCorMC(mc)
    print.betamc(out)
    summary.betamc(out)
    coef.betamc(out)
    vcov.betamc(out)
    confint.betamc(out)
    testthat::test_that(
      paste(text, "squared partial correlations"),
      {
        testthat::expect_true(
          all(
            abs(
              coef.betamc(out) - c(
                .4874,
                .3757,
                .2254
              )
            ) <= tol
          )
        )
      }
    )
    object <- lm(QUALITY ~ NARTIC, data = df)
    mc <- MCMI(object, mi = mi, R = R, type = "mvn")
    testthat::test_that(
      paste(text, "error"),
      {
        testthat::expect_error(
          PCorMC(mc)
        )
      }
    )
  },
  text = "test-betaMC-p-cor-mc-est-mi",
  R = 5L,
  tol = 0.0001
)
