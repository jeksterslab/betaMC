## ---- test-betaMC-r-sq-mc-est
lapply(
  X = 1,
  FUN = function(i,
                 text,
                 R,
                 tol) {
    message(text)
    testthat::test_that(
      paste(text, "multiple regression", "coef"),
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
        lm_summary <- summary(object)
        rsq <- lm_summary$r.squared
        adj <- lm_summary$adj.r.squared
        mc <- MC(object, R = R, type = "mvn")
        out <- RSqMC(mc)
        print.betamc(out)
        summary.betamc(out)
        print.summary.betamc(summary.betamc(out))
        coef.betamc(out)
        vcov.betamc(out)
        confint.betamc(out)
        testthat::expect_true(
          all(
            abs(
              coef.betamc(out) - c(rsq, adj)
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "simple regression"),
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
        lm_summary <- summary(object)
        rsq <- lm_summary$r.squared
        adj <- lm_summary$adj.r.squared
        mc <- MC(object, R = R, type = "mvn")
        out <- RSqMC(mc)
        testthat::expect_true(
          all(
            abs(
              coef.betamc(out) - c(rsq, adj)
            ) <= tol
          )
        )
      }
    )
  },
  text = "test-betaMC-r-sq-mc-est",
  R = 5L,
  tol = 0.0001
)
