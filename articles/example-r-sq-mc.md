# betaMC: Example Using the RSqMC Function

Confidence intervals for multiple correlation coefficients (R-squared
and adjusted R-squared) are generated using the
[`RSqMC()`](https://jeksterslab.github.io/betaMC/reference/RSqMC.md)
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

### Multiple Correlation Coefficients

#### Normal-Theory Approach

\
`mvn`` ``<-`` `[`RSqMC`](https://jeksterslab.github.io/betaMC/reference/RSqMC.md)`(``mvn``)`

#### Asymptotic distribution-free Approach

\
`adf`` ``<-`` `[`RSqMC`](https://jeksterslab.github.io/betaMC/reference/RSqMC.md)`(``adf``)`

#### Heteroskedasticity Consistent Approach (HC3)

\
`hc3`` ``<-`` `[`RSqMC`](https://jeksterslab.github.io/betaMC/reference/RSqMC.md)`(``hc3``)`

## Methods

### summary

Summary of the results of
[`RSqMC()`](https://jeksterslab.github.io/betaMC/reference/RSqMC.md).

\
[`summary`](https://rdrr.io/r/base/summary.html)`(``mvn``)`\
`#> Call:`\
`#> RSqMC(object = mvn)`\
`#> `\
`#> R-squared and adjusted R-squared`\
`#> type = "mvn"`\
`#>        est     se     R  0.05%   0.5%   2.5%  97.5%  99.5% 99.95%`\
`#> rsq 0.8045 0.0557 20000 0.5187 0.6003 0.6633 0.8803 0.9013 0.9258`\
`#> adj 0.7906 0.0597 20000 0.4843 0.5717 0.6392 0.8718 0.8942 0.9205`\
[`summary`](https://rdrr.io/r/base/summary.html)`(``adf``)`\
`#> Call:`\
`#> RSqMC(object = adf)`\
`#> `\
`#> R-squared and adjusted R-squared`\
`#> type = "adf"`\
`#>        est     se     R  0.05%   0.5%   2.5%  97.5%  99.5% 99.95%`\
`#> rsq 0.8045 0.0547 20000 0.5427 0.6122 0.6660 0.8797 0.9026 0.9212`\
`#> adj 0.7906 0.0586 20000 0.5100 0.5845 0.6421 0.8711 0.8956 0.9156`\
[`summary`](https://rdrr.io/r/base/summary.html)`(``hc3``)`\
`#> Call:`\
`#> RSqMC(object = hc3)`\
`#> `\
`#> R-squared and adjusted R-squared`\
`#> type = "hc3"`\
`#>        est     se     R  0.05% 0.5%   2.5% 97.5%  99.5% 99.95%`\
`#> rsq 0.8045 0.0618 20000 0.4685 0.58 0.6481 0.888 0.9111 0.9382`\
`#> adj 0.7906 0.0662 20000 0.4306 0.55 0.6230 0.880 0.9048 0.9338`

### coef

Return the vector of estimates.

\
[`coef`](https://rdrr.io/r/stats/coef.html)`(``mvn``)`\
`#>       rsq       adj `\
`#> 0.8045263 0.7905638`\
[`coef`](https://rdrr.io/r/stats/coef.html)`(``adf``)`\
`#>       rsq       adj `\
`#> 0.8045263 0.7905638`\
[`coef`](https://rdrr.io/r/stats/coef.html)`(``hc3``)`\
`#>       rsq       adj `\
`#> 0.8045263 0.7905638`

### vcov

Return the sampling covariance matrix.

\
[`vcov`](https://rdrr.io/r/stats/vcov.html)`(``mvn``)`\
`#>             rsq         adj`\
`#> rsq 0.003103084 0.003324733`\
`#> adj 0.003324733 0.003562213`\
[`vcov`](https://rdrr.io/r/stats/vcov.html)`(``adf``)`\
`#>             rsq         adj`\
`#> rsq 0.002992561 0.003206315`\
`#> adj 0.003206315 0.003435338`\
[`vcov`](https://rdrr.io/r/stats/vcov.html)`(``hc3``)`\
`#>             rsq         adj`\
`#> rsq 0.003815207 0.004087721`\
`#> adj 0.004087721 0.004379701`

### confint

Return confidence intervals.

\
[`confint`](https://rdrr.io/r/stats/confint.html)`(``mvn``, level ``=`` ``0.95``)`\
`#>         2.5 %    97.5 %`\
`#> rsq 0.6632574 0.8803444`\
`#> adj 0.6392043 0.8717976`\
[`confint`](https://rdrr.io/r/stats/confint.html)`(``adf``, level ``=`` ``0.95``)`\
`#>         2.5 %    97.5 %`\
`#> rsq 0.6659786 0.8796907`\
`#> adj 0.6421200 0.8710972`\
[`confint`](https://rdrr.io/r/stats/confint.html)`(``hc3``, level ``=`` ``0.95``)`\
`#>         2.5 %    97.5 %`\
`#> rsq 0.6480992 0.8880097`\
`#> adj 0.6229635 0.8800104`

## References

Dudgeon, P. (2017). Some improvements in confidence intervals for
standardized regression coefficients. *Psychometrika*, *82*(4), 928–951.
<https://doi.org/10.1007/s11336-017-9563-z>
