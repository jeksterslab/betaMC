# betaMC: Example Using the PCorMC Function

Confidence intervals for squared partial correlation coefficients are
generated using the
[`PCorMC()`](https://jeksterslab.github.io/betaMC/reference/PCorMC.md)
function from the `betaMC` package. In this example, we use the data set
and the model used in [betaMC: Example Using the BetaMC
Function](https://jeksterslab.github.io/betaMC/articles/example-beta-mc.md).

\
[`library`](https://rdrr.io/r/base/library.html)`(`[`betaMC`](https://github.com/jeksterslab/betaMC)`)`

\
`df`` ``<-`` ``betaMC``::`[`nas1982`](https://jeksterslab.github.io/betaMC/reference/nas1982.md)

### Regression

Fit the regression model using the
[`lm()`](https://rdrr.io/r/stats/lm.html) function.

\
`object`` ``<-`` `[`lm`](https://rdrr.io/r/stats/lm.html)`(``QUALITY`` ``~`` ``NARTIC`` ``+`` ``PCTGRT`` ``+`` ``PCTSUPP``, data ``=`` ``df``)`

### Monte Carlo Sampling Distribution of Parameters

#### Normal-Theory Approach

\
`mvn`` ``<-`` `[`MC`](https://jeksterslab.github.io/betaMC/reference/MC.md)`(``object``, type ``=`` ``"mvn"``)`

#### Asymptotic distribution-free Approach

\
`adf`` ``<-`` `[`MC`](https://jeksterslab.github.io/betaMC/reference/MC.md)`(``object``, type ``=`` ``"adf"``)`

#### Heteroskedasticity Consistent Approach (HC3)

\
`hc3`` ``<-`` `[`MC`](https://jeksterslab.github.io/betaMC/reference/MC.md)`(``object``, type ``=`` ``"hc3"``)`

### Squared Partial Correlation Coefficients

#### Normal-Theory Approach

\
`mvn`` ``<-`` `[`PCorMC`](https://jeksterslab.github.io/betaMC/reference/PCorMC.md)`(``mvn``)`

#### Asymptotic distribution-free Approach

\
`adf`` ``<-`` `[`PCorMC`](https://jeksterslab.github.io/betaMC/reference/PCorMC.md)`(``adf``)`

#### Heteroskedasticity Consistent Approach (HC3)

\
`hc3`` ``<-`` `[`PCorMC`](https://jeksterslab.github.io/betaMC/reference/PCorMC.md)`(``hc3``)`

## Methods

### summary

Summary of the results of
[`PCorMC()`](https://jeksterslab.github.io/betaMC/reference/PCorMC.md).

\
[`summary`](https://rdrr.io/r/base/summary.html)`(``mvn``)`\
`#> Call:`\
`#> PCorMC(object = mvn)`\
`#> `\
`#> Squared partial correlations`\
`#> type = "mvn"`\
`#>            est     se     R  0.05%   0.5%   2.5%  97.5%  99.5% 99.95%`\
`#> NARTIC  0.4874 0.1056 20000 0.1215 0.1809 0.2439 0.6516 0.7070 0.7729`\
`#> PCTGRT  0.3757 0.1069 20000 0.0502 0.0972 0.1474 0.5596 0.6227 0.6930`\
`#> PCTSUPP 0.2254 0.0992 20000 0.0041 0.0208 0.0467 0.4290 0.5044 0.5855`\
[`summary`](https://rdrr.io/r/base/summary.html)`(``adf``)`\
`#> Call:`\
`#> PCorMC(object = adf)`\
`#> `\
`#> Squared partial correlations`\
`#> type = "adf"`\
`#>            est     se     R  0.05%   0.5%   2.5%  97.5%  99.5% 99.95%`\
`#> NARTIC  0.4874 0.1001 20000 0.0531 0.1551 0.2387 0.6328 0.6854 0.7525`\
`#> PCTGRT  0.3757 0.1009 20000 0.0252 0.0899 0.1450 0.5357 0.5911 0.6601`\
`#> PCTSUPP 0.2254 0.1033 20000 0.0004 0.0101 0.0364 0.4335 0.5023 0.5826`\
[`summary`](https://rdrr.io/r/base/summary.html)`(``hc3``)`\
`#> Call:`\
`#> PCorMC(object = hc3)`\
`#> `\
`#> Squared partial correlations`\
`#> type = "hc3"`\
`#>            est     se     R  0.05%   0.5%   2.5%  97.5%  99.5% 99.95%`\
`#> NARTIC  0.4874 0.1185 20000 0.0101 0.0747 0.1813 0.6485 0.7086 0.7726`\
`#> PCTGRT  0.3757 0.1150 20000 0.0068 0.0479 0.1073 0.5532 0.6143 0.6846`\
`#> PCTSUPP 0.2254 0.1139 20000 0.0001 0.0034 0.0250 0.4581 0.5461 0.6468`

### coef

Return the vector of estimates.

\
[`coef`](https://rdrr.io/r/stats/coef.html)`(``mvn``)`\
`#>    NARTIC    PCTGRT   PCTSUPP `\
`#> 0.4874382 0.3757383 0.2253739`\
[`coef`](https://rdrr.io/r/stats/coef.html)`(``adf``)`\
`#>    NARTIC    PCTGRT   PCTSUPP `\
`#> 0.4874382 0.3757383 0.2253739`\
[`coef`](https://rdrr.io/r/stats/coef.html)`(``hc3``)`\
`#>    NARTIC    PCTGRT   PCTSUPP `\
`#> 0.4874382 0.3757383 0.2253739`

### vcov

Return the sampling covariance matrix.

\
[`vcov`](https://rdrr.io/r/stats/vcov.html)`(``mvn``)`\
`#>               NARTIC       PCTGRT      PCTSUPP`\
`#> NARTIC  0.0111416715 0.0006771445 0.0004585966`\
`#> PCTGRT  0.0006771445 0.0114229744 0.0001186545`\
`#> PCTSUPP 0.0004585966 0.0001186545 0.0098435770`\
[`vcov`](https://rdrr.io/r/stats/vcov.html)`(``adf``)`\
`#>              NARTIC       PCTGRT      PCTSUPP`\
`#> NARTIC  0.010020154 0.0025967551 0.0021447945`\
`#> PCTGRT  0.002596755 0.0101857725 0.0008667582`\
`#> PCTSUPP 0.002144795 0.0008667582 0.0106690364`\
[`vcov`](https://rdrr.io/r/stats/vcov.html)`(``hc3``)`\
`#>              NARTIC      PCTGRT     PCTSUPP`\
`#> NARTIC  0.014034713 0.003686483 0.002925480`\
`#> PCTGRT  0.003686483 0.013228458 0.001002455`\
`#> PCTSUPP 0.002925480 0.001002455 0.012964028`

### confint

Return confidence intervals.

\
[`confint`](https://rdrr.io/r/stats/confint.html)`(``mvn``, level ``=`` ``0.95``)`\
`#>              2.5 %    97.5 %`\
`#> NARTIC  0.24389871 0.6515824`\
`#> PCTGRT  0.14737337 0.5596427`\
`#> PCTSUPP 0.04672414 0.4290453`\
[`confint`](https://rdrr.io/r/stats/confint.html)`(``adf``, level ``=`` ``0.95``)`\
`#>              2.5 %    97.5 %`\
`#> NARTIC  0.23873119 0.6328010`\
`#> PCTGRT  0.14495782 0.5356783`\
`#> PCTSUPP 0.03640219 0.4334605`\
[`confint`](https://rdrr.io/r/stats/confint.html)`(``hc3``, level ``=`` ``0.95``)`\
`#>              2.5 %    97.5 %`\
`#> NARTIC  0.18130711 0.6485450`\
`#> PCTGRT  0.10733295 0.5532402`\
`#> PCTSUPP 0.02497765 0.4580861`

## References

Dudgeon, P. (2017). Some improvements in confidence intervals for
standardized regression coefficients. *Psychometrika*, *82*(4), 928–951.
<https://doi.org/10.1007/s11336-017-9563-z>
