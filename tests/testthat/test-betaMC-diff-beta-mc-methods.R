## ---- test-betaSandwich-diff-beta-sandwich-methods
lapply(
  X = 1,
  FUN = function(i,
                 text) {
    message(text)
    if (!exists("nas1982")) {
      try(
        data(
          "nas1982",
          package = "betaSandwich"
        ),
        silent = TRUE
      )
    }
    df <- nas1982
    object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = df)
    mvn <- dif(BetaMC(object, type = "mvn"))
    print.difbetamc(mvn)
    summary.difbetamc(mvn)
    coef.difbetamc(mvn)
    vcov.difbetamc(mvn)
    confint.difbetamc(mvn)
    adf <- dif(BetaMC(object, type = "adf"))
    print.difbetamc(adf)
    summary.difbetamc(adf)
    coef.difbetamc(adf)
    vcov.difbetamc(adf)
    confint.difbetamc(adf)
    hc0 <- dif(BetaMC(object, type = "hc0"))
    print.difbetamc(hc0)
    summary.difbetamc(hc0)
    coef.difbetamc(hc0)
    vcov.difbetamc(hc0)
    confint.difbetamc(hc0)
    hc1 <- dif(BetaMC(object, type = "hc1"))
    print.difbetamc(hc1)
    summary.difbetamc(hc1)
    coef.difbetamc(hc1)
    vcov.difbetamc(hc1)
    confint.difbetamc(hc1)
    hc2 <- dif(BetaMC(object, type = "hc2"))
    print.difbetamc(hc2)
    summary.difbetamc(hc2)
    coef.difbetamc(hc2)
    vcov.difbetamc(hc2)
    confint.difbetamc(hc2)
    hc3 <- dif(BetaMC(object, type = "hc3"))
    print.difbetamc(hc3)
    summary.difbetamc(hc3)
    coef.difbetamc(hc3)
    vcov.difbetamc(hc3)
    confint.difbetamc(hc3)
    hc4 <- dif(BetaMC(object, type = "hc4"))
    print.difbetamc(hc4)
    summary.difbetamc(hc4)
    coef.difbetamc(hc4)
    vcov.difbetamc(hc4)
    confint.difbetamc(hc4)
    hc4m <- dif(BetaMC(object, type = "hc4m"))
    print.difbetamc(hc4m)
    summary.difbetamc(hc4m)
    coef.difbetamc(hc4m)
    vcov.difbetamc(hc4m)
    confint.difbetamc(hc4m)
    hc5 <- dif(BetaMC(object, type = "hc5"))
    print.difbetamc(hc5)
    summary.difbetamc(hc5)
    coef.difbetamc(hc5)
    vcov.difbetamc(hc5)
    confint.difbetamc(hc5)
  },
  text = "test-betaSandwich-diff-beta-sandwich-methods"
)
