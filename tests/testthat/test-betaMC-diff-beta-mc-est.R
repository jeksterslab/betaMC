## ---- test-betaMC-diff-beta-mc-est
lapply(
  X = 1,
  FUN = function(i,
                 text,
                 R,
                 tol) {
    message(text)
    testthat::test_that(
      paste(text, "differences of standardized slopes"),
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
        mc <- MC(object, R = R, type = "mvn")
        out <- DiffBetaMC(mc)
        print.betamc(out)
        summary.betamc(out)
        coef.betamc(out)
        vcov.betamc(out)
        confint.betamc(out)
        testthat::expect_true(
          all(
            abs(
              coef.betamc(out) - c(
                .4951 - .3915,
                .4951 - .2632,
                .3915 - .2632
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
        mc <- MC(object, R = R, type = "mvn")
        testthat::expect_error(
          DiffBetaMC(mc)
        )
      }
    )
  },
  text = "test-betaMC-diff-beta-mc-est",
  R = 5L,
  tol = 0.0001
)
