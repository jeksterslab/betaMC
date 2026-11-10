## ---- test-betaMC-smoke
# The one test file that runs on CRAN: fast, and it checks that the main
# functions run. rProject writes this file once; it is yours after that.
lapply(
  X = 1,
  FUN = function(i,
                 text) {
    testthat::test_that(
      desc = paste(text, "the package loads"),
      code = {
        testthat::expect_true(
          object = requireNamespace(package = "betaMC", quietly = TRUE)
        )
      }
    )
  },
  text = "test-betaMC-smoke"
)
