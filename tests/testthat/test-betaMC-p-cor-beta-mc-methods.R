## ---- test-betaMC-p-cor-beta-mc-methods
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
    mvn <- PCorBetaMC(BetaMC(object, type = "mvn", R = R))
    print.pcorbetamc(mvn)
    summary.pcorbetamc(mvn)
    coef.pcorbetamc(mvn)
    vcov.pcorbetamc(mvn)
    confint.pcorbetamc(mvn)
    adf <- PCorBetaMC(BetaMC(object, type = "adf", R = R))
    print.pcorbetamc(adf)
    summary.pcorbetamc(adf)
    coef.pcorbetamc(adf)
    vcov.pcorbetamc(adf)
    confint.pcorbetamc(adf)
    hc0 <- PCorBetaMC(BetaMC(object, type = "hc0", R = R))
    print.pcorbetamc(hc0)
    summary.pcorbetamc(hc0)
    coef.pcorbetamc(hc0)
    vcov.pcorbetamc(hc0)
    confint.pcorbetamc(hc0)
    hc1 <- PCorBetaMC(BetaMC(object, type = "hc1", R = R))
    print.pcorbetamc(hc1)
    summary.pcorbetamc(hc1)
    coef.pcorbetamc(hc1)
    vcov.pcorbetamc(hc1)
    confint.pcorbetamc(hc1)
    hc2 <- PCorBetaMC(BetaMC(object, type = "hc2", R = R))
    print.pcorbetamc(hc2)
    summary.pcorbetamc(hc2)
    coef.pcorbetamc(hc2)
    vcov.pcorbetamc(hc2)
    confint.pcorbetamc(hc2)
    hc3 <- PCorBetaMC(BetaMC(object, type = "hc3", R = R))
    print.pcorbetamc(hc3)
    summary.pcorbetamc(hc3)
    coef.pcorbetamc(hc3)
    vcov.pcorbetamc(hc3)
    confint.pcorbetamc(hc3)
    hc4 <- PCorBetaMC(BetaMC(object, type = "hc4", R = R))
    print.pcorbetamc(hc4)
    summary.pcorbetamc(hc4)
    coef.pcorbetamc(hc4)
    vcov.pcorbetamc(hc4)
    confint.pcorbetamc(hc4)
    hc4m <- PCorBetaMC(BetaMC(object, type = "hc4m", R = R))
    print.pcorbetamc(hc4m)
    summary.pcorbetamc(hc4m)
    coef.pcorbetamc(hc4m)
    vcov.pcorbetamc(hc4m)
    confint.pcorbetamc(hc4m)
    hc5 <- PCorBetaMC(BetaMC(object, type = "hc5", R = R))
    print.pcorbetamc(hc5)
    summary.pcorbetamc(hc5)
    coef.pcorbetamc(hc5)
    vcov.pcorbetamc(hc5)
    confint.pcorbetamc(hc5)
  },
  text = "test-betaMC-p-cor-beta-mc-methods",
  R = 5L
)
