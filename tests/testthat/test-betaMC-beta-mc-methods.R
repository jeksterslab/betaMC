## ---- test-betaMC-beta-mc-methods
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
    out <- BetaMC(object, R = R)
    print.betamc(out)
    summary.betamc(out)
    coef.betamc(out)
    vcov.betamc(out)
    confint.betamc(out)
    object <- lm(QUALITY ~ NARTIC, data = df)
    out <- BetaMC(object, R = R)
    print.betamc(out)
    summary.betamc(out)
    coef.betamc(out)
    vcov.betamc(out)
    confint.betamc(out)
  },
  text = "test-betaMC-beta-mc-methods",
  R = 10L
)
