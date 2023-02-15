## ---- test-betaMC-p-cor-beta-mc
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
    mvn <- PCorBetaMC(BetaMC(object, type = "mvn", R = R))
    adf <- PCorBetaMC(BetaMC(object, type = "adf", R = R))
    hc0 <- PCorBetaMC(BetaMC(object, type = "hc0", R = R))
    hc1 <- PCorBetaMC(BetaMC(object, type = "hc1", R = R))
    hc2 <- PCorBetaMC(BetaMC(object, type = "hc2", R = R))
    hc3 <- PCorBetaMC(BetaMC(object, type = "hc3", R = R))
    hc4 <- PCorBetaMC(BetaMC(object, type = "hc4", R = R))
    hc4m <- PCorBetaMC(BetaMC(object, type = "hc4m", R = R))
    hc5 <- PCorBetaMC(BetaMC(object, type = "hc5", R = R))
    testthat::test_that(
      paste(text, "mvn", "multiple regression"),
      {
        testthat::expect_true(
          all(
            abs(
              c(
                .4312,
                .3430,
                .2385,
                .1859,
                .1177,
                .0569,
                .4874,
                .3757,
                .2254
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
                .4312,
                .3430,
                .2385,
                .1859,
                .1177,
                .0569,
                .4874,
                .3757,
                .2254
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
                .4312,
                .3430,
                .2385,
                .1859,
                .1177,
                .0569,
                .4874,
                .3757,
                .2254
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
                .4312,
                .3430,
                .2385,
                .1859,
                .1177,
                .0569,
                .4874,
                .3757,
                .2254
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
                .4312,
                .3430,
                .2385,
                .1859,
                .1177,
                .0569,
                .4874,
                .3757,
                .2254
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
                .4312,
                .3430,
                .2385,
                .1859,
                .1177,
                .0569,
                .4874,
                .3757,
                .2254
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
                .4312,
                .3430,
                .2385,
                .1859,
                .1177,
                .0569,
                .4874,
                .3757,
                .2254
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
                .4312,
                .3430,
                .2385,
                .1859,
                .1177,
                .0569,
                .4874,
                .3757,
                .2254
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
                .4312,
                .3430,
                .2385,
                .1859,
                .1177,
                .0569,
                .4874,
                .3757,
                .2254
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
    testthat::test_that(
      paste(text, "mvn", "simple regression"),
      {
        testthat::expect_error(
          PCorBetaMC(BetaMC(object, type = "mvn"))
        )
      }
    )
    testthat::test_that(
      paste(text, "adf", "simple regression"),
      {
        testthat::expect_error(
          PCorBetaMC(BetaMC(object, type = "adf"))
        )
      }
    )
    testthat::test_that(
      paste(text, "hc0", "simple regression"),
      {
        testthat::expect_error(
          PCorBetaMC(BetaMC(object, type = "hc0"))
        )
      }
    )
    testthat::test_that(
      paste(text, "hc1", "simple regression"),
      {
        testthat::expect_error(
          PCorBetaMC(BetaMC(object, type = "hc1"))
        )
      }
    )
    testthat::test_that(
      paste(text, "hc2", "simple regression"),
      {
        testthat::expect_error(
          PCorBetaMC(BetaMC(object, type = "hc2"))
        )
      }
    )
    testthat::test_that(
      paste(text, "hc3", "simple regression"),
      {
        testthat::expect_error(
          PCorBetaMC(BetaMC(object, type = "hc3"))
        )
      }
    )
    testthat::test_that(
      paste(text, "hc4", "simple regression"),
      {
        testthat::expect_error(
          PCorBetaMC(BetaMC(object, type = "hc4"))
        )
      }
    )
    testthat::test_that(
      paste(text, "hc4m", "simple regression"),
      {
        testthat::expect_error(
          PCorBetaMC(BetaMC(object, type = "hc4m"))
        )
      }
    )
    testthat::test_that(
      paste(text, "hc5", "simple regression"),
      {
        testthat::expect_error(
          PCorBetaMC(BetaMC(object, type = "hc5"))
        )
      }
    )
  },
  text = "test-betaMC-p-cor-beta-mc",
  R = 5L,
  tol = 0.0001
)
