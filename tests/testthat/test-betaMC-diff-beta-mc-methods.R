## ---- test-betaMC-diff-beta-mc-methods
lapply(
  X = 1,
  FUN = function(i,
                 text,
                 R) {
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
    mvn <- DiffBetaMC(BetaMC(object, type = "mvn", R = R))
    print.diffbetamc(mvn)
    summary.diffbetamc(mvn)
    coef.diffbetamc(mvn)
    vcov.diffbetamc(mvn)
    confint.diffbetamc(mvn)
    adf <- DiffBetaMC(BetaMC(object, type = "adf", R = R))
    print.diffbetamc(adf)
    summary.diffbetamc(adf)
    coef.diffbetamc(adf)
    vcov.diffbetamc(adf)
    confint.diffbetamc(adf)
    hc0 <- DiffBetaMC(BetaMC(object, type = "hc0", R = R))
    print.diffbetamc(hc0)
    summary.diffbetamc(hc0)
    coef.diffbetamc(hc0)
    vcov.diffbetamc(hc0)
    confint.diffbetamc(hc0)
    hc1 <- DiffBetaMC(BetaMC(object, type = "hc1", R = R))
    print.diffbetamc(hc1)
    summary.diffbetamc(hc1)
    coef.diffbetamc(hc1)
    vcov.diffbetamc(hc1)
    confint.diffbetamc(hc1)
    hc2 <- DiffBetaMC(BetaMC(object, type = "hc2", R = R))
    print.diffbetamc(hc2)
    summary.diffbetamc(hc2)
    coef.diffbetamc(hc2)
    vcov.diffbetamc(hc2)
    confint.diffbetamc(hc2)
    hc3 <- DiffBetaMC(BetaMC(object, type = "hc3", R = R))
    print.diffbetamc(hc3)
    summary.diffbetamc(hc3)
    coef.diffbetamc(hc3)
    vcov.diffbetamc(hc3)
    confint.diffbetamc(hc3)
    hc4 <- DiffBetaMC(BetaMC(object, type = "hc4", R = R))
    print.diffbetamc(hc4)
    summary.diffbetamc(hc4)
    coef.diffbetamc(hc4)
    vcov.diffbetamc(hc4)
    confint.diffbetamc(hc4)
    hc4m <- DiffBetaMC(BetaMC(object, type = "hc4m", R = R))
    print.diffbetamc(hc4m)
    summary.diffbetamc(hc4m)
    coef.diffbetamc(hc4m)
    vcov.diffbetamc(hc4m)
    confint.diffbetamc(hc4m)
    hc5 <- DiffBetaMC(BetaMC(object, type = "hc5", R = R))
    print.diffbetamc(hc5)
    summary.diffbetamc(hc5)
    coef.diffbetamc(hc5)
    vcov.diffbetamc(hc5)
    confint.diffbetamc(hc5)
  },
  text = "test-betaMC-diff-beta-mc-methods",
  R = 5L
)
