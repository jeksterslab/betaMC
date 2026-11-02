# betaMC: Example Using the DeltaRSqMC Function

Confidence intervals for improvement in R-squared are generated using
the
[`DeltaRSqMC()`](https://github.com/jeksterslab/betaMC/reference/DeltaRSqMC.md)
function from the `betaMC` package. In this example, we use the data set
and the model used in [betaMC: Example Using the BetaMC
Function](https://github.com/jeksterslab/betaMC/articles/example-beta-mc.md).

\
[`library`](https://rdrr.io/r/base/library.html)`(`[`betaMC`](https://github.com/jeksterslab/betaMC)`)`

\
`df`` ``<-`` ``betaMC``::`[`nas1982`](https://github.com/jeksterslab/betaMC/reference/nas1982.md)

### Regression

Fit the regression model using the
[`lm()`](https://rdrr.io/r/stats/lm.html) function.

\
`object`` ``<-`` `[`lm`](https://rdrr.io/r/stats/lm.html)`(``QUALITY`` ``~`` ``NARTIC`` ``+`` ``PCTGRT`` ``+`` ``PCTSUPP``, data ``=`` ``df``)`

### Monte Carlo Sampling Distribution of Parameters

#### Normal-Theory Approach

\
`mvn`` ``<-`` `[`MC`](https://github.com/jeksterslab/betaMC/reference/MC.md)`(``object``, type ``=`` ``"mvn"``)`

#### Asymptotic distribution-free Approach

\
`adf`` ``<-`` `[`MC`](https://github.com/jeksterslab/betaMC/reference/MC.md)`(``object``, type ``=`` ``"adf"``)`

#### Heteroskedasticity Consistent Approach (HC3)

\
`hc3`` ``<-`` `[`MC`](https://github.com/jeksterslab/betaMC/reference/MC.md)`(``object``, type ``=`` ``"hc3"``)`

### Improvement in R-squared

#### Normal-Theory Approach

\
`mvn`` ``<-`` `[`DeltaRSqMC`](https://github.com/jeksterslab/betaMC/reference/DeltaRSqMC.md)`(``mvn``)`

#### Asymptotic distribution-free Approach

\
`adf`` ``<-`` `[`DeltaRSqMC`](https://github.com/jeksterslab/betaMC/reference/DeltaRSqMC.md)`(``adf``)`

#### Heteroskedasticity Consistent Approach (HC3)

\
`hc3`` ``<-`` `[`DeltaRSqMC`](https://github.com/jeksterslab/betaMC/reference/DeltaRSqMC.md)`(``hc3``)`

## Methods

### summary

Summary of the results of
[`DeltaRSqMC()`](https://github.com/jeksterslab/betaMC/reference/DeltaRSqMC.md).

\
[`summary`](https://rdrr.io/r/base/summary.html)`(``mvn``)`\
`#> Call:`\
`#> DeltaRSqMC(object = mvn)`\
`#> `\
`#> Improvement in R-squared`\
`#> type = "mvn"`\
`#>            est     se     R  0.05%   0.5%   2.5%  97.5%  99.5% 99.95%`\
`#> NARTIC  0.1859 0.0663 20000 0.0324 0.0510 0.0726 0.3282 0.3908 0.4682`\
`#> PCTGRT  0.1177 0.0511 20000 0.0141 0.0244 0.0382 0.2365 0.2914 0.3703`\
`#> PCTSUPP 0.0569 0.0338 20000 0.0007 0.0049 0.0103 0.1403 0.1786 0.2342`\
[`summary`](https://rdrr.io/r/base/summary.html)`(``adf``)`\
`#> Call:`\
`#> DeltaRSqMC(object = adf)`\
`#> `\
`#> Improvement in R-squared`\
`#> type = "adf"`\
`#>            est     se     R  0.05%   0.5%   2.5%  97.5%  99.5% 99.95%`\
`#> NARTIC  0.1859 0.0582 20000 0.0077 0.0450 0.0745 0.3032 0.3512 0.4044`\
`#> PCTGRT  0.1177 0.0470 20000 0.0057 0.0219 0.0368 0.2198 0.2638 0.3255`\
`#> PCTSUPP 0.0569 0.0330 20000 0.0001 0.0028 0.0094 0.1354 0.1748 0.2182`\
[`summary`](https://rdrr.io/r/base/summary.html)`(``hc3``)`\
`#> Call:`\
`#> DeltaRSqMC(object = hc3)`\
`#> `\
`#> Improvement in R-squared`\
`#> type = "hc3"`\
`#>            est     se     R  0.05%   0.5%   2.5%  97.5%  99.5% 99.95%`\
`#> NARTIC  0.1859 0.0691 20000 0.0024 0.0185 0.0517 0.3245 0.3791 0.4458`\
`#> PCTGRT  0.1177 0.0546 20000 0.0016 0.0109 0.0254 0.2334 0.2951 0.3749`\
`#> PCTSUPP 0.0569 0.0372 20000 0.0000 0.0010 0.0064 0.1483 0.1923 0.2512`

### coef

Return the vector of estimates.

\
[`coef`](https://rdrr.io/r/stats/coef.html)`(``mvn``)`\
`#>    NARTIC    PCTGRT   PCTSUPP `\
`#> 0.1858925 0.1176542 0.0568722`\
[`coef`](https://rdrr.io/r/stats/coef.html)`(``adf``)`\
`#>    NARTIC    PCTGRT   PCTSUPP `\
`#> 0.1858925 0.1176542 0.0568722`\
[`coef`](https://rdrr.io/r/stats/coef.html)`(``hc3``)`\
`#>    NARTIC    PCTGRT   PCTSUPP `\
`#> 0.1858925 0.1176542 0.0568722`

### vcov

Return the sampling covariance matrix.

\
[`vcov`](https://rdrr.io/r/stats/vcov.html)`(``mvn``)`\
`#>                NARTIC        PCTGRT       PCTSUPP`\
`#> NARTIC   0.0043927066 -0.0007420947 -0.0003420446`\
`#> PCTGRT  -0.0007420947  0.0026064133 -0.0002431884`\
`#> PCTSUPP -0.0003420446 -0.0002431884  0.0011422639`\
[`vcov`](https://rdrr.io/r/stats/vcov.html)`(``adf``)`\
`#>                NARTIC        PCTGRT       PCTSUPP`\
`#> NARTIC   3.391031e-03  4.874407e-05 -0.0001580216`\
`#> PCTGRT   4.874407e-05  2.207210e-03 -0.0002018811`\
`#> PCTSUPP -1.580216e-04 -2.018811e-04  0.0010870440`\
[`vcov`](https://rdrr.io/r/stats/vcov.html)`(``hc3``)`\
`#>                NARTIC        PCTGRT       PCTSUPP`\
`#> NARTIC   4.770940e-03  0.0001563451 -4.443315e-05`\
`#> PCTGRT   1.563451e-04  0.0029767221 -1.524671e-04`\
`#> PCTSUPP -4.443315e-05 -0.0001524671  1.381807e-03`

### confint

Return confidence intervals.

\
[`confint`](https://rdrr.io/r/stats/confint.html)`(``mvn``, level ``=`` ``0.95``)`\
`#>              2.5 %    97.5 %`\
`#> NARTIC  0.07256185 0.3281640`\
`#> PCTGRT  0.03821714 0.2365150`\
`#> PCTSUPP 0.01031463 0.1402773`\
[`confint`](https://rdrr.io/r/stats/confint.html)`(``adf``, level ``=`` ``0.95``)`\
`#>               2.5 %    97.5 %`\
`#> NARTIC  0.074519672 0.3031958`\
`#> PCTGRT  0.036764139 0.2197601`\
`#> PCTSUPP 0.009403787 0.1353896`\
[`confint`](https://rdrr.io/r/stats/confint.html)`(``hc3``, level ``=`` ``0.95``)`\
`#>               2.5 %    97.5 %`\
`#> NARTIC  0.051720568 0.3244929`\
`#> PCTGRT  0.025360626 0.2333554`\
`#> PCTSUPP 0.006386026 0.1483131`

## References

Dudgeon, P. (2017). Some improvements in confidence intervals for
standardized regression coefficients. *Psychometrika*, *82*(4), 928–951.
<https://doi.org/10.1007/s11336-017-9563-z>
