## ---- test-external-betaMC-r-sq-adf
lapply(
  X = 1,
  FUN = function(i,
                 text,
                 n,
                 tol) {
    set.seed(seed = 42)
    message(text)
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
    df <- as.data.frame(
      x = MASS::mvrnorm(
        n = n,
        mu = colMeans(
          x = df[, c("QUALITY", "NARTIC", "PCTGRT", "PCTSUPP")]
        ),
        Sigma = stats::cov(
          x = df[, c("QUALITY", "NARTIC", "PCTGRT", "PCTSUPP")]
        ),
        empirical = TRUE
      )
    )
    object <- stats::lm(formula = QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = df)
    rsq <- RSqMC(object = MC(object = object, type = "adf"))
    model <- paste(
      "QUALITY ~ NARTIC + PCTGRT + PCTSUPP;",
      "QUALITY ~~ sigmasq * QUALITY;",
      "rsq := 1 - sigmasq;",
      "adj := 1 - ((", n, " - 1) / (", n, " - 4)) * (1 - rsq)"
    )
    lav <- lavaan::sem(
      model = model,
      data = df,
      estimator = "WLS"
    )
    std <- semmcci:::summary.semmcci(semmcci::MCStd(mc = semmcci::MC(lav = lav)))
    testthat::test_that(
      desc = paste(text, "coef"),
      code = {
        testthat::skip_on_cran()
        testthat::expect_true(
          object = all(
            abs(
              x = stats::coef(object = rsq) - std[c("rsq", "adj"), "est"]
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      desc = paste(text, "se"),
      code = {
        testthat::skip_on_cran()
        testthat::expect_true(
          object = all(
            abs(
              x = sqrt(x = diag(x = stats::vcov(object = rsq))) - std[c("rsq", "adj"), "se"]
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      desc = paste(text, "ci.lower"),
      code = {
        testthat::skip_on_cran()
        testthat::expect_true(
          object = all(
            abs(
              x = stats::confint(object = rsq)[, 1] - std[c("rsq", "adj"), "2.5%"]
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      desc = paste(text, "ci.upper"),
      code = {
        testthat::skip_on_cran()
        testthat::expect_true(
          object = all(
            abs(
              x = stats::confint(object = rsq)[, 2] - std[c("rsq", "adj"), "97.5%"]
            ) <= tol
          )
        )
      }
    )
  },
  text = "test-external-betaMC-r-sq-adf",
  n = 100000L,
  tol = 0.001
)
