## ---- test-betaMC-s-cor-mc-est-mi
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
    mc <- MCMI(object, R = R, type = "mvn")
    out <- SCorMC(mc)
    print.betamc(out)
    summary.betamc(out)
    coef.betamc(out)
    vcov.betamc(out)
    confint.betamc(out)
    testthat::test_that(
      paste(text, "semipartial correlation coefficients"),
      {
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
    object <- lm(QUALITY ~ NARTIC, data = df)
    mc <- MCMI(object, R = R, type = "mvn")
    testthat::test_that(
      paste(text, "error"),
      {
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
