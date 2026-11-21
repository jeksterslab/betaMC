# betaMC: Example Using the DeltaRSqMC Function

Confidence intervals for improvement in R-squared are generated using
the
[`DeltaRSqMC()`](https://jeksterslab.github.io/betaMC/reference/DeltaRSqMC.md)
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

### Improvement in R-squared

#### Normal-Theory Approach

\
`mvn`` ``<-`` `[`DeltaRSqMC`](https://jeksterslab.github.io/betaMC/reference/DeltaRSqMC.md)`(``object ``=`` ``mvn``)`

#### Asymptotic distribution-free Approach

\
`adf`` ``<-`` `[`DeltaRSqMC`](https://jeksterslab.github.io/betaMC/reference/DeltaRSqMC.md)`(``object ``=`` ``adf``)`

#### Heteroskedasticity Consistent Approach (HC3)

\
`hc3`` ``<-`` `[`DeltaRSqMC`](https://jeksterslab.github.io/betaMC/reference/DeltaRSqMC.md)`(``object ``=`` ``hc3``)`

## Methods

### summary

Summary of the results of
[`DeltaRSqMC()`](https://jeksterslab.github.io/betaMC/reference/DeltaRSqMC.md).

\
[`summary`](https://rdrr.io/r/base/summary.html)`(``object ``=`` ``mvn``)`\
`#> Call:`\
`#> DeltaRSqMC(object = mvn)`\
`#> `\
`#> Improvement in R-squared`\
`#> type = "mvn"`\
`#>            est     se     R  0.05%   0.5%   2.5%  97.5%  99.5% 99.95%`\
`#> NARTIC  0.1859 0.0656 20000 0.0299 0.0489 0.0720 0.3290 0.3856 0.4507`\
`#> PCTGRT  0.1177 0.0508 20000 0.0116 0.0251 0.0384 0.2340 0.2872 0.3467`\
`#> PCTSUPP 0.0569 0.0339 20000 0.0003 0.0046 0.0107 0.1396 0.1800 0.2489`\
[`summary`](https://rdrr.io/r/base/summary.html)`(``object ``=`` ``adf``)`\
`#> Call:`\
`#> DeltaRSqMC(object = adf)`\
`#> `\
`#> Improvement in R-squared`\
`#> type = "adf"`\
`#>            est     se     R  0.05%   0.5%   2.5%  97.5%  99.5% 99.95%`\
`#> NARTIC  0.1859 0.0583 20000 0.0137 0.0443 0.0729 0.3028 0.3533 0.4149`\
`#> PCTGRT  0.1177 0.0472 20000 0.0096 0.0231 0.0368 0.2212 0.2638 0.3186`\
`#> PCTSUPP 0.0569 0.0334 20000 0.0001 0.0027 0.0088 0.1367 0.1778 0.2239`\
[`summary`](https://rdrr.io/r/base/summary.html)`(``object ``=`` ``hc3``)`\
`#> Call:`\
`#> DeltaRSqMC(object = hc3)`\
`#> `\
`#> Improvement in R-squared`\
`#> type = "hc3"`\
`#>            est     se     R  0.05%   0.5%   2.5%  97.5%  99.5% 99.95%`\
`#> NARTIC  0.1859 0.0681 20000 0.0020 0.0170 0.0495 0.3177 0.3767 0.4311`\
`#> PCTGRT  0.1177 0.0540 20000 0.0013 0.0107 0.0265 0.2338 0.2901 0.3692`\
`#> PCTSUPP 0.0569 0.0373 20000 0.0000 0.0011 0.0064 0.1481 0.1925 0.2652`

### coef

Return the vector of estimates.

\
`stats``::`[`coef`](https://rdrr.io/r/stats/coef.html)`(``object ``=`` ``mvn``)`\
`#>    NARTIC    PCTGRT   PCTSUPP `\
`#> 0.1858925 0.1176542 0.0568722`\
`stats``::`[`coef`](https://rdrr.io/r/stats/coef.html)`(``object ``=`` ``adf``)`\
`#>    NARTIC    PCTGRT   PCTSUPP `\
`#> 0.1858925 0.1176542 0.0568722`\
`stats``::`[`coef`](https://rdrr.io/r/stats/coef.html)`(``object ``=`` ``hc3``)`\
`#>    NARTIC    PCTGRT   PCTSUPP `\
`#> 0.1858925 0.1176542 0.0568722`

### vcov

Return the sampling covariance matrix.

\
`stats``::`[`vcov`](https://rdrr.io/r/stats/vcov.html)`(``object ``=`` ``mvn``)`\
`#>                NARTIC        PCTGRT       PCTSUPP`\
`#> NARTIC   0.0043074664 -0.0006946688 -0.0003606126`\
`#> PCTGRT  -0.0006946688  0.0025758293 -0.0002362047`\
`#> PCTSUPP -0.0003606126 -0.0002362047  0.0011524040`\
`stats``::`[`vcov`](https://rdrr.io/r/stats/vcov.html)`(``object ``=`` ``adf``)`\
`#>                NARTIC        PCTGRT       PCTSUPP`\
`#> NARTIC   3.404617e-03  4.612882e-05 -0.0001580908`\
`#> PCTGRT   4.612882e-05  2.226522e-03 -0.0001956553`\
`#> PCTSUPP -1.580908e-04 -1.956553e-04  0.0011161151`\
`stats``::`[`vcov`](https://rdrr.io/r/stats/vcov.html)`(``object ``=`` ``hc3``)`\
`#>                NARTIC        PCTGRT       PCTSUPP`\
`#> NARTIC   4.640667e-03  0.0001959007 -2.952555e-05`\
`#> PCTGRT   1.959007e-04  0.0029156679 -1.953027e-04`\
`#> PCTSUPP -2.952555e-05 -0.0001953027  1.389998e-03`

### confint

Return confidence intervals.

\
`stats``::`[`confint`](https://rdrr.io/r/stats/confint.html)`(``object ``=`` ``mvn``, level ``=`` ``0.95``)`\
`#>              2.5 %    97.5 %`\
`#> NARTIC  0.07203962 0.3289978`\
`#> PCTGRT  0.03837774 0.2340088`\
`#> PCTSUPP 0.01072673 0.1395875`\
`stats``::`[`confint`](https://rdrr.io/r/stats/confint.html)`(``object ``=`` ``adf``, level ``=`` ``0.95``)`\
`#>               2.5 %    97.5 %`\
`#> NARTIC  0.072861942 0.3027791`\
`#> PCTGRT  0.036796841 0.2212350`\
`#> PCTSUPP 0.008828175 0.1367070`\
`stats``::`[`confint`](https://rdrr.io/r/stats/confint.html)`(``object ``=`` ``hc3``, level ``=`` ``0.95``)`\
`#>               2.5 %    97.5 %`\
`#> NARTIC  0.049518793 0.3177486`\
`#> PCTGRT  0.026522453 0.2338050`\
`#> PCTSUPP 0.006403343 0.1480521`

## References

Dudgeon, P. (2017). Some improvements in confidence intervals for
standardized regression coefficients. *Psychometrika*, *82*(4), 928–951.
<https://doi.org/10.1007/s11336-017-9563-z>

Pesigan, I. J. A., & Cheung, S. F. (2024). Monte Carlo confidence
intervals for the indirect effect with missing data. *Behavior Research
Methods*, *56*(3), 1678–1696.
<https://doi.org/10.3758/s13428-023-02114-4>
