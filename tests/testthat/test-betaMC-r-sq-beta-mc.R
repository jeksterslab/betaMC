## ---- test-betaMC-r-sq-beta-mc
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
    r_sq <- summary(object)$r.squared
    adj <- summary(object)$adj.r.squared
    mvn <- RSqBetaMC(BetaMC(object, type = "mvn", R = R))
    adf <- RSqBetaMC(BetaMC(object, type = "adf", R = R))
    hc0 <- RSqBetaMC(BetaMC(object, type = "hc0", R = R))
    hc1 <- RSqBetaMC(BetaMC(object, type = "hc1", R = R))
    hc2 <- RSqBetaMC(BetaMC(object, type = "hc2", R = R))
    hc3 <- RSqBetaMC(BetaMC(object, type = "hc3", R = R))
    hc4 <- RSqBetaMC(BetaMC(object, type = "hc4", R = R))
    hc4m <- RSqBetaMC(BetaMC(object, type = "hc4m", R = R))
    hc5 <- RSqBetaMC(BetaMC(object, type = "hc5", R = R))
    testthat::test_that(
      paste(text, "mvn", "multiple regression"),
      {
        testthat::expect_true(
          all(
            abs(
              c(
                r_sq,
                adj
              ) - coef(mvn)
            ) <= tol
          )
        )
        testthat::expect_true(
          all(
            abs(
              summary(mvn)[, "est"] - coef(mvn)
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "adf", "multiple regression"),
      {
        testthat::expect_true(
          all(
            abs(
              c(
                r_sq,
                adj
              ) - coef(adf)
            ) <= tol
          )
        )
        testthat::expect_true(
          all(
            abs(
              summary(adf)[, "est"] - coef(adf)
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "hc0", "multiple regression"),
      {
        testthat::expect_true(
          all(
            abs(
              c(
                r_sq,
                adj
              ) - coef(hc0)
            ) <= tol
          )
        )
        testthat::expect_true(
          all(
            abs(
              summary(hc0)[, "est"] - coef(hc0)
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "hc1", "multiple regression"),
      {
        testthat::expect_true(
          all(
            abs(
              c(
                r_sq,
                adj
              ) - coef(hc1)
            ) <= tol
          )
        )
        testthat::expect_true(
          all(
            abs(
              summary(hc1)[, "est"] - coef(hc1)
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "hc2", "multiple regression"),
      {
        testthat::expect_true(
          all(
            abs(
              c(
                r_sq,
                adj
              ) - coef(hc2)
            ) <= tol
          )
        )
        testthat::expect_true(
          all(
            abs(
              summary(hc2)[, "est"] - coef(hc2)
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "hc3", "multiple regression"),
      {
        testthat::expect_true(
          all(
            abs(
              c(
                r_sq,
                adj
              ) - coef(hc3)
            ) <= tol
          )
        )
        testthat::expect_true(
          all(
            abs(
              summary(hc3)[, "est"] - coef(hc3)
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "hc4", "multiple regression"),
      {
        testthat::expect_true(
          all(
            abs(
              c(
                r_sq,
                adj
              ) - coef(hc4)
            ) <= tol
          )
        )
        testthat::expect_true(
          all(
            abs(
              summary(hc4)[, "est"] - coef(hc4)
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "hc4m", "multiple regression"),
      {
        testthat::expect_true(
          all(
            abs(
              c(
                r_sq,
                adj
              ) - coef(hc4m)
            ) <= tol
          )
        )
        testthat::expect_true(
          all(
            abs(
              summary(hc4m)[, "est"] - coef(hc4m)
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "hc5", "multiple regression"),
      {
        testthat::expect_true(
          all(
            abs(
              c(
                r_sq,
                adj
              ) - coef(hc5)
            ) <= tol
          )
        )
        testthat::expect_true(
          all(
            abs(
              summary(hc5)[, "est"] - coef(hc5)
            ) <= tol
          )
        )
      }
    )
    object <- lm(QUALITY ~ NARTIC, data = df)
    r_sq <- summary(object)$r.squared
    adj <- summary(object)$adj.r.squared
    mvn <- RSqBetaMC(BetaMC(object, type = "mvn", R = R))
    adf <- RSqBetaMC(BetaMC(object, type = "adf", R = R))
    hc0 <- RSqBetaMC(BetaMC(object, type = "hc0", R = R))
    hc1 <- RSqBetaMC(BetaMC(object, type = "hc1", R = R))
    hc2 <- RSqBetaMC(BetaMC(object, type = "hc2", R = R))
    hc3 <- RSqBetaMC(BetaMC(object, type = "hc3", R = R))
    hc4 <- RSqBetaMC(BetaMC(object, type = "hc4", R = R))
    hc4m <- RSqBetaMC(BetaMC(object, type = "hc4m", R = R))
    hc5 <- RSqBetaMC(BetaMC(object, type = "hc5", R = R))
    testthat::test_that(
      paste(text, "mvn", "multiple regression"),
      {
        testthat::expect_true(
          all(
            abs(
              c(
                r_sq,
                adj
              ) - coef(mvn)
            ) <= tol
          )
        )
        testthat::expect_true(
          all(
            abs(
              summary(mvn)[, "est"] - coef(mvn)
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "adf", "multiple regression"),
      {
        testthat::expect_true(
          all(
            abs(
              c(
                r_sq,
                adj
              ) - coef(adf)
            ) <= tol
          )
        )
        testthat::expect_true(
          all(
            abs(
              summary(adf)[, "est"] - coef(adf)
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "hc0", "simple regression"),
      {
        testthat::expect_true(
          all(
            abs(
              c(
                r_sq,
                adj
              ) - coef(hc0)
            ) <= tol
          )
        )
        testthat::expect_true(
          all(
            abs(
              summary(hc0)[, "est"] - coef(hc0)
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "hc1", "simple regression"),
      {
        testthat::expect_true(
          all(
            abs(
              c(
                r_sq,
                adj
              ) - coef(hc1)
            ) <= tol
          )
        )
        testthat::expect_true(
          all(
            abs(
              summary(hc1)[, "est"] - coef(hc1)
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "hc2", "simple regression"),
      {
        testthat::expect_true(
          all(
            abs(
              c(
                r_sq,
                adj
              ) - coef(hc2)
            ) <= tol
          )
        )
        testthat::expect_true(
          all(
            abs(
              summary(hc2)[, "est"] - coef(hc2)
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "hc3", "simple regression"),
      {
        testthat::expect_true(
          all(
            abs(
              c(
                r_sq,
                adj
              ) - coef(hc3)
            ) <= tol
          )
        )
        testthat::expect_true(
          all(
            abs(
              summary(hc3)[, "est"] - coef(hc3)
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "hc4", "simple regression"),
      {
        testthat::expect_true(
          all(
            abs(
              c(
                r_sq,
                adj
              ) - coef(hc4)
            ) <= tol
          )
        )
        testthat::expect_true(
          all(
            abs(
              summary(hc4)[, "est"] - coef(hc4)
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "hc4m", "simple regression"),
      {
        testthat::expect_true(
          all(
            abs(
              c(
                r_sq,
                adj
              ) - coef(hc4m)
            ) <= tol
          )
        )
        testthat::expect_true(
          all(
            abs(
              summary(hc4m)[, "est"] - coef(hc4m)
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "hc5", "simple regression"),
      {
        testthat::expect_true(
          all(
            abs(
              c(
                r_sq,
                adj
              ) - coef(hc5)
            ) <= tol
          )
        )
        testthat::expect_true(
          all(
            abs(
              summary(hc5)[, "est"] - coef(hc5)
            ) <= tol
          )
        )
      }
    )
  },
  text = "test-betaMC-r-sq-beta-mc",
  R = 10L,
  tol = 0.0001
)
