## ---- test-zzz-coverage
lapply(
  X = 1,
  FUN = function(i,
                 text) {
    testthat::skip_on_cran()
    message(text)

    set.seed(seed = 42)

    testthat::test_that(
      desc = paste(text, "methods"),
      code = {
        set.seed(seed = 42)
        df <- nas1982
        object <- stats::lm(
          formula = QUALITY ~ NARTIC + PCTGRT + PCTSUPP,
          data = df
        )
        lm_process <- betaMC:::.ProcessLM(object = object)
        print(
          x = betaMC:::.JacVechSigmaWRTTheta(
            beta = lm_process$beta,
            sigmacapx = lm_process$sigmacapx,
            q = lm_process$q,
            p = lm_process$p,
            rsq = NULL,
            fixed_x = FALSE
          )
        )
        print(
          x = betaMC:::.JacVechSigmaWRTTheta(
            beta = lm_process$beta,
            sigmacapx = lm_process$sigmacapx,
            q = lm_process$q,
            p = lm_process$p,
            rsq = NULL,
            fixed_x = TRUE
          )
        )
        print(
          x = betaMC:::.JacVechSigmaWRTTheta(
            beta = lm_process$beta,
            sigmacapx = lm_process$sigmacapx,
            q = lm_process$q,
            p = lm_process$p,
            rsq = lm_process$rsq[1],
            fixed_x = FALSE
          )
        )
        print(
          x = betaMC:::.JacVechSigmaWRTTheta(
            beta = lm_process$beta,
            sigmacapx = lm_process$sigmacapx,
            q = lm_process$q,
            p = lm_process$p,
            rsq = lm_process$rsq[1],
            fixed_x = TRUE
          )
        )
        betaMC:::.ThetaHatStar(
          scale = diag(x = 2),
          location = c(0, 0),
          pd = FALSE
        )
        betaMC:::.ThetaHatStar(
          scale = matrix(
            data = c(1, -1, 1, 1),
            nrow = 2
          ),
          location = c(0, 0),
          pd = FALSE
        )
        betaMC:::.TestPositiveDefinite2(
          x = matrix(
            data = c(1, -1, 1, 1),
            nrow = 2
          )
        )
        testthat::expect_true(
          object = TRUE
        )
      }
    )
  },
  text = "test-zzz-coverage"
)
