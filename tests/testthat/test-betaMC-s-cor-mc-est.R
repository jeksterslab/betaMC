## ---- test-betaMC-s-cor-mc-est
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
      desc = paste(text, "semipartial correlation coefficients"),
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
        mc <- MC(object = object, R = R, type = "mvn")
        out <- SCorMC(object = mc)
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
        mc <- MC(object = object, R = R, type = "mvn")
        testthat::expect_error(
          object = SCorMC(object = mc)
        )
      }
    )
  },
  text = "test-betaMC-s-cor-mc-est",
  R = 5L,
  tol = 0.0001
)
