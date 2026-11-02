## ---- test-betaMC-beta-mc-std
lapply(
  X = 1,
  FUN = function(i,
                 text,
                 tol) {
    message(text)
    if (!identical(Sys.getenv("NOT_CRAN"), "true") && !interactive()) {
      message("CRAN: tests skipped.")
      # nolint start
      return(invisible(NULL))
      # nolint end
    }
    set.seed(42)
    n <- 200
    x1 <- rnorm(n)
    x2 <- rbinom(n, size = 1, prob = 0.4)
    x3 <- rnorm(n) + 0.5 * x1
    y <- 0.4 * x1 + 0.3 * x2 - 0.2 * x3 + rnorm(n)
    df <- data.frame(y = y, x1 = x1, x2 = x2, x3 = x3)
    object <- lm(y ~ x1 + x2 + x3, data = df)
    b <- coef(object)[-1]
    sds <- sapply(df, sd)
    testthat::test_that(
      paste(text, "selective standardization"),
      {
        testthat::skip_on_cran()
        mc <- MC(object, R = 5000L, type = "hc3", seed = 42)
        raw <- BetaMC(mc, std_y = FALSE, std_x = character(0))
        testthat::expect_true(all(abs(coef(raw) - b) <= tol))
        dummy <- BetaMC(mc, std_x = c("x1", "x3"))
        testthat::expect_true(
          abs(coef(dummy)["x2"] - b["x2"] / sds["y"]) <= tol
        )
        # normal-theory draws: raw-slope SEs near the lm SEs
        # (HC3 draws estimate HC3 SEs, which differ from the lm SEs)
        raw_mvn <- BetaMC(
          MC(object, R = 5000L, type = "mvn", seed = 42),
          std_y = FALSE,
          std_x = character(0)
        )
        testthat::expect_true(
          all(
            abs(
              sqrt(diag(vcov(raw_mvn))) / sqrt(diag(vcov(object)))[-1] - 1
            ) <= 0.15
          )
        )
        full <- BetaMC(mc)
        same <- BetaMC(mc, std_x = c("x1", "x2", "x3"))
        testthat::expect_true(all(abs(vcov(full) - vcov(same)) <= tol))
      }
    )
  },
  text = "test-betaMC-beta-mc-std",
  tol = 1e-10
)
