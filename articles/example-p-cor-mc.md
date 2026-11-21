# betaMC: Example Using the PCorMC Function

Confidence intervals for squared partial correlation coefficients are
generated using the
[`PCorMC()`](https://jeksterslab.github.io/betaMC/reference/PCorMC.md)
function from the `betaMC` package. In this example, we use the data set
and the model used in [betaMC: Example Using the BetaMC
Function](https://jeksterslab.github.io/betaMC/articles/example-beta-mc.md).

\
[`library`](https://rdrr.io/r/base/library.html)`(``package ``=`` ``betaMC``)`

\
`df`` ``<-`` ``betaMC``::`[`nas1982`](https://jeksterslab.github.io/betaMC/reference/nas1982.md)

### Regression

Fit the regression model using the
[`lm()`](https://rdrr.io/r/stats/lm.html) function.

\
`object`` ``<-`` ``stats``::`[`lm`](https://rdrr.io/r/stats/lm.html)`(``formula ``=`` ``QUALITY`` ``~`` ``NARTIC`` ``+`` ``PCTGRT`` ``+`` ``PCTSUPP``, data ``=`` ``df``)`

### Monte Carlo Sampling Distribution of Parameters

#### Normal-Theory Approach

\
`mvn`` ``<-`` `[`MC`](https://jeksterslab.github.io/betaMC/reference/MC.md)`(``object ``=`` ``object``, type ``=`` ``"mvn"``)`

#### Asymptotic distribution-free Approach

\
`adf`` ``<-`` `[`MC`](https://jeksterslab.github.io/betaMC/reference/MC.md)`(``object ``=`` ``object``, type ``=`` ``"adf"``)`

#### Heteroskedasticity Consistent Approach (HC3)

\
`hc3`` ``<-`` `[`MC`](https://jeksterslab.github.io/betaMC/reference/MC.md)`(``object ``=`` ``object``, type ``=`` ``"hc3"``)`

### Squared Partial Correlation Coefficients

#### Normal-Theory Approach

\
`mvn`` ``<-`` `[`PCorMC`](https://jeksterslab.github.io/betaMC/reference/PCorMC.md)`(``object ``=`` ``mvn``)`

#### Asymptotic distribution-free Approach

\
`adf`` ``<-`` `[`PCorMC`](https://jeksterslab.github.io/betaMC/reference/PCorMC.md)`(``object ``=`` ``adf``)`

#### Heteroskedasticity Consistent Approach (HC3)

\
`hc3`` ``<-`` `[`PCorMC`](https://jeksterslab.github.io/betaMC/reference/PCorMC.md)`(``object ``=`` ``hc3``)`

## Methods

### summary

Summary of the results of
[`PCorMC()`](https://jeksterslab.github.io/betaMC/reference/PCorMC.md).

\
[`summary`](https://rdrr.io/r/base/summary.html)`(``object ``=`` ``mvn``)`\
`#> Call:`\
`#> PCorMC(object = mvn)`\
`#> `\
`#> Squared partial correlations`\
`#> type = "mvn"`\
`#>            est     se     R  0.05%   0.5%   2.5%  97.5%  99.5% 99.95%`\
`#> NARTIC  0.4874 0.1049 20000 0.1187 0.1847 0.2440 0.6547 0.7079 0.7669`\
`#> PCTGRT  0.3757 0.1066 20000 0.0556 0.0970 0.1490 0.5602 0.6247 0.6820`\
`#> PCTSUPP 0.2254 0.0989 20000 0.0053 0.0188 0.0486 0.4258 0.4988 0.5942`\
[`summary`](https://rdrr.io/r/base/summary.html)`(``object ``=`` ``adf``)`\
`#> Call:`\
`#> PCorMC(object = adf)`\
`#> `\
`#> Squared partial correlations`\
`#> type = "adf"`\
`#>            est     se     R  0.05%   0.5%   2.5%  97.5%  99.5% 99.95%`\
`#> NARTIC  0.4874 0.1000 20000 0.0531 0.1566 0.2389 0.6329 0.6854 0.7525`\
`#> PCTGRT  0.3757 0.1009 20000 0.0252 0.0899 0.1450 0.5355 0.5905 0.6601`\
`#> PCTSUPP 0.2254 0.1034 20000 0.0004 0.0101 0.0364 0.4339 0.5023 0.5826`\
[`summary`](https://rdrr.io/r/base/summary.html)`(``object ``=`` ``hc3``)`\
`#> Call:`\
`#> PCorMC(object = hc3)`\
`#> `\
`#> Squared partial correlations`\
`#> type = "hc3"`\
`#>            est     se     R  0.05%   0.5%   2.5%  97.5%  99.5% 99.95%`\
`#> NARTIC  0.4874 0.1184 20000 0.0101 0.0747 0.1814 0.6482 0.7078 0.7726`\
`#> PCTGRT  0.3757 0.1150 20000 0.0068 0.0479 0.1073 0.5532 0.6161 0.6846`\
`#> PCTSUPP 0.2254 0.1139 20000 0.0001 0.0034 0.0248 0.4580 0.5452 0.6468`

### coef

Return the vector of estimates.

\
`stats``::`[`coef`](https://rdrr.io/r/stats/coef.html)`(``object ``=`` ``mvn``)`\
`#>    NARTIC    PCTGRT   PCTSUPP `\
`#> 0.4874382 0.3757383 0.2253739`\
`stats``::`[`coef`](https://rdrr.io/r/stats/coef.html)`(``object ``=`` ``adf``)`\
`#>    NARTIC    PCTGRT   PCTSUPP `\
`#> 0.4874382 0.3757383 0.2253739`\
`stats``::`[`coef`](https://rdrr.io/r/stats/coef.html)`(``object ``=`` ``hc3``)`\
`#>    NARTIC    PCTGRT   PCTSUPP `\
`#> 0.4874382 0.3757383 0.2253739`

### vcov

Return the sampling covariance matrix.

\
`stats``::`[`vcov`](https://rdrr.io/r/stats/vcov.html)`(``object ``=`` ``mvn``)`\
`#>               NARTIC       PCTGRT      PCTSUPP`\
`#> NARTIC  0.0110141845 6.580146e-04 3.645558e-04`\
`#> PCTGRT  0.0006580146 1.136777e-02 3.109028e-06`\
`#> PCTSUPP 0.0003645558 3.109028e-06 9.783594e-03`\
`stats``::`[`vcov`](https://rdrr.io/r/stats/vcov.html)`(``object ``=`` ``adf``)`\
`#>              NARTIC       PCTGRT      PCTSUPP`\
`#> NARTIC  0.010008319 0.0025897215 0.0021423294`\
`#> PCTGRT  0.002589721 0.0101780631 0.0008563851`\
`#> PCTSUPP 0.002142329 0.0008563851 0.0106838539`\
`stats``::`[`vcov`](https://rdrr.io/r/stats/vcov.html)`(``object ``=`` ``hc3``)`\
`#>              NARTIC       PCTGRT      PCTSUPP`\
`#> NARTIC  0.014020632 0.0036807534 0.0029199035`\
`#> PCTGRT  0.003680753 0.0132272021 0.0009992982`\
`#> PCTSUPP 0.002919904 0.0009992982 0.0129621009`

### confint

Return confidence intervals.

\
`stats``::`[`confint`](https://rdrr.io/r/stats/confint.html)`(``object ``=`` ``mvn``, level ``=`` ``0.95``)`\
`#>              2.5 %    97.5 %`\
`#> NARTIC  0.24404271 0.6546911`\
`#> PCTGRT  0.14898177 0.5601978`\
`#> PCTSUPP 0.04855312 0.4257944`\
`stats``::`[`confint`](https://rdrr.io/r/stats/confint.html)`(``object ``=`` ``adf``, level ``=`` ``0.95``)`\
`#>             2.5 %    97.5 %`\
`#> NARTIC  0.2389386 0.6329057`\
`#> PCTGRT  0.1449664 0.5355016`\
`#> PCTSUPP 0.0363954 0.4339313`\
`stats``::`[`confint`](https://rdrr.io/r/stats/confint.html)`(``object ``=`` ``hc3``, level ``=`` ``0.95``)`\
`#>              2.5 %    97.5 %`\
`#> NARTIC  0.18140198 0.6482142`\
`#> PCTGRT  0.10730870 0.5532402`\
`#> PCTSUPP 0.02483994 0.4579576`

## References

Dudgeon, P. (2017). Some improvements in confidence intervals for
standardized regression coefficients. *Psychometrika*, *82*(4), 928–951.
<https://doi.org/10.1007/s11336-017-9563-z>

Pesigan, I. J. A., & Cheung, S. F. (2024). Monte Carlo confidence
intervals for the indirect effect with missing data. *Behavior Research
Methods*, *56*(3), 1678–1696.
<https://doi.org/10.3758/s13428-023-02114-4>
