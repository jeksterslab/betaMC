## ---- test-betaMC-delta-r-sq-mc-est
lapply(
  X = 1,
  FUN = function(i,
                 text,
                 R,
                 tol) {
    message(text)
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
    mc <- MC(object, R = R, type = "mvn")
    out <- DeltaRSqMC(mc)
    print.betamc(out)
    summary.betamc(out)
    coef.betamc(out)
    vcov.betamc(out)
    confint.betamc(out)
    object <- lm(QUALITY ~ NARTIC, data = df)
    testthat::test_that(
      paste(text, "improvement in R-squared"),
      {
        testthat::expect_true(
          all(
            abs(
              coef.betamc(out) - c(
                .1859,
                .1177,
                .0569
              )
            ) <= tol
          )
        )
      }
    )
    mc <- MC(object, R = R, type = "mvn")
    testthat::test_that(
      paste(text, "error"),
      {
        testthat::expect_error(
          DeltaRSqMC(mc)
        )
      }
    )
  },
  text = "test-betaMC-delta-r-sq-mc-est",
  R = 5L,
  tol = 0.0001
)
