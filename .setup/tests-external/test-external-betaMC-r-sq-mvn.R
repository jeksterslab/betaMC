## ---- test-external-betaMC-r-sq-mvn
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
    mc <- BetaMC(object, type = "mvn")
    rsq <- RSqBetaMC(mc)
    model <- paste(
      "QUALITY ~ NARTIC + PCTGRT + PCTSUPP;",
      "QUALITY ~~ sigmasq * QUALITY;",
      "rsq := 1 - sigmasq;",
      "adj := 1 - ((", n, " - 1) / (", n, " - 4)) * (1 - rsq)"
    )
    lav <- lavaan::sem(
      model = model,
      data = df,
      estimator = "ML"
    )
    std <- summary(MCStd(MC(lav)))
    testthat::test_that(
      paste(text, "coef"),
      {
        testthat::expect_true(
          all(
            abs(
              coef(rsq) - std[c("rsq", "adj"), "est"]
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
              sqrt(diag(vcov(rsq))) - std[c("rsq", "adj"), "se"]
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
              confint(rsq)[, 1] - std[c("rsq", "adj"), "2.5%"]
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
              confint(rsq)[, 2] - std[c("rsq", "adj"), "97.5%"]
            ) <= tol
          )
        )
      }
    )
  },
  text = "test-external-betaMC-r-sq-mvn",
  n = 100000L,
  tol = 0.001
)
