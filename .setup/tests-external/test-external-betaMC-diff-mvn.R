## ---- test-external-betaMC-diff-mvn
lapply(
  X = 1,
  FUN = function(i,
                 text,
                 n,
                 tol) {
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
    diff <- DiffBetaMC(MC(object, type = "mvn"))
    model <- paste(
      "QUALITY ~ b1 * NARTIC + b2 * PCTGRT + b3 * PCTSUPP;",
      "diff12 := b1 - b2; diff13 := b1 - b3; diff23 := b2 - b3"
    )
    lav <- lavaan::sem(
      model = model,
      data = df,
      estimator = "ML"
    )
    std <- semmcci:::summary.semmcci(semmcci::MCStd(semmcci::MC(lav)))
    testthat::test_that(
      paste(text, "coef"),
      {
        testthat::expect_true(
          all(
            abs(
              coef(diff) - std[c("diff12", "diff13", "diff23"), "est"]
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
              sqrt(diag(vcov(diff))) - std[c("diff12", "diff13", "diff23"), "se"]
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
              confint(diff)[, 1] - std[c("diff12", "diff13", "diff23"), "2.5%"]
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
              confint(diff)[, 2] - std[c("diff12", "diff13", "diff23"), "97.5%"]
            ) <= tol
          )
        )
      }
    )
  },
  text = "test-external-betaMC-diff-mvn",
  n = 100000L,
  tol = 0.001
)
