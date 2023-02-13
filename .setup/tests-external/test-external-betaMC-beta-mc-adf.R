## ---- test-external-betaMC-beta-mc-adf
lapply(
  X = 1,
  FUN = function(i,
                 text,
                 n,
                 tol) {
    library(semmcci)
    set.seed(42)
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
    df <- as.data.frame(
      MASS::mvrnorm(
        n = n,
        mu = colMeans(
          df[, c("QUALITY", "NARTIC", "PCTGRT", "PCTSUPP")]
        ),
        Sigma = cov(
          df[, c("QUALITY", "NARTIC", "PCTGRT", "PCTSUPP")]
        ),
        empirical = TRUE
      )
    )
    object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = df)
    mc <- BetaMC(object, type = "adf")
    model <- "QUALITY ~ b1 * NARTIC + b2 * PCTGRT + b3 * PCTSUPP"
    lav <- lavaan::sem(
      model = model,
      data = df,
      estimator = "WLS"
    )
    std <- summary(MCStd(MC(lav)))
    testthat::test_that(
      paste(text, "coef"),
      {
        testthat::expect_true(
          all(
            abs(
              coef(mc) - std[c("b1", "b2", "b3"), "est"]
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "se"),
      {
        testthat::expect_true(
          all(
            abs(
              sqrt(diag(vcov(mc))) - std[c("b1", "b2", "b3"), "se"]
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "ci.lower"),
      {
        testthat::expect_true(
          all(
            abs(
              confint(mc)[, 1] - std[c("b1", "b2", "b3"), "2.5%"]
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "ci.upper"),
      {
        testthat::expect_true(
          all(
            abs(
              confint(mc)[, 2] - std[c("b1", "b2", "b3"), "97.5%"]
            ) <= tol
          )
        )
      }
    )
  },
  text = "test-external-betaMC-beta-mc-adf",
  n = 100000L,
  tol = 0.001
)
