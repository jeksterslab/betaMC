## ---- test-betaMC-r-sq-beta-mc-methods
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
    mvn <- RSqBetaMC(BetaMC(object, type = "mvn", R = R))
    print.rsqbetamc(mvn)
    summary.rsqbetamc(mvn)
    coef.rsqbetamc(mvn)
    vcov.rsqbetamc(mvn)
    confint.rsqbetamc(mvn)
    adf <- RSqBetaMC(BetaMC(object, type = "adf", R = R))
    print.rsqbetamc(adf)
    summary.rsqbetamc(adf)
    coef.rsqbetamc(adf)
    vcov.rsqbetamc(adf)
    confint.rsqbetamc(adf)
    hc0 <- RSqBetaMC(BetaMC(object, type = "hc0", R = R))
    print.rsqbetamc(hc0)
    summary.rsqbetamc(hc0)
    coef.rsqbetamc(hc0)
    vcov.rsqbetamc(hc0)
    confint.rsqbetamc(hc0)
    hc1 <- RSqBetaMC(BetaMC(object, type = "hc1", R = R))
    print.rsqbetamc(hc1)
    summary.rsqbetamc(hc1)
    coef.rsqbetamc(hc1)
    vcov.rsqbetamc(hc1)
    confint.rsqbetamc(hc1)
    hc2 <- RSqBetaMC(BetaMC(object, type = "hc2", R = R))
    print.rsqbetamc(hc2)
    summary.rsqbetamc(hc2)
    coef.rsqbetamc(hc2)
    vcov.rsqbetamc(hc2)
    confint.rsqbetamc(hc2)
    hc3 <- RSqBetaMC(BetaMC(object, type = "hc3", R = R))
    print.rsqbetamc(hc3)
    summary.rsqbetamc(hc3)
    coef.rsqbetamc(hc3)
    vcov.rsqbetamc(hc3)
    confint.rsqbetamc(hc3)
    hc4 <- RSqBetaMC(BetaMC(object, type = "hc4", R = R))
    print.rsqbetamc(hc4)
    summary.rsqbetamc(hc4)
    coef.rsqbetamc(hc4)
    vcov.rsqbetamc(hc4)
    confint.rsqbetamc(hc4)
    hc4m <- RSqBetaMC(BetaMC(object, type = "hc4m", R = R))
    print.rsqbetamc(hc4m)
    summary.rsqbetamc(hc4m)
    coef.rsqbetamc(hc4m)
    vcov.rsqbetamc(hc4m)
    confint.rsqbetamc(hc4m)
    hc5 <- RSqBetaMC(BetaMC(object, type = "hc5", R = R))
    print.rsqbetamc(hc5)
    summary.rsqbetamc(hc5)
    coef.rsqbetamc(hc5)
    vcov.rsqbetamc(hc5)
    confint.rsqbetamc(hc5)
  },
  text = "test-betaMC-r-sq-beta-mc-methods",
  R = 10L
)
