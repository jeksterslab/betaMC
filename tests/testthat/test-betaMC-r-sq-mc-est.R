## ---- test-betaMC-r-sq-mc-est
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
      desc = paste(text, "multiple regression", "coef"),
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
        lm_summary <- summary(object = object)
        rsq <- lm_summary$r.squared
        adj <- lm_summary$adj.r.squared
        mc <- MC(object = object, R = R, type = "mvn")
        out <- RSqMC(object = mc)
        print.betamc(x = out)
        summary.betamc(object = out)
        print.summary.betamc(x = summary.betamc(object = out))
        coef.betamc(object = out)
        vcov.betamc(object = out)
        confint.betamc(object = out)
        testthat::expect_true(
          object = all(
            abs(
              x = coef.betamc(object = out) - c(rsq, adj)
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      desc = paste(text, "simple regression"),
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
        lm_summary <- summary(object = object)
        rsq <- lm_summary$r.squared
        adj <- lm_summary$adj.r.squared
        mc <- MC(object = object, R = R, type = "mvn")
        out <- RSqMC(object = mc)
        testthat::expect_true(
          object = all(
            abs(
              x = coef.betamc(object = out) - c(rsq, adj)
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
