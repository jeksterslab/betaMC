# betaMC: Example Using the DiffBetaMC Function

Confidence intervals for differences of standardized regression slopes
are generated using the
[`DiffBetaMC()`](https://jeksterslab.github.io/betaMC/reference/DiffBetaMC.md)
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

### Differences of Standardized Regression Slopes

#### Normal-Theory Approach

\
`mvn`` ``<-`` `[`DiffBetaMC`](https://jeksterslab.github.io/betaMC/reference/DiffBetaMC.md)`(``mvn``)`

#### Asymptotic distribution-free Approach

\
`adf`` ``<-`` `[`DiffBetaMC`](https://jeksterslab.github.io/betaMC/reference/DiffBetaMC.md)`(``adf``)`

#### Heteroskedasticity Consistent Approach (HC3)

\
`hc3`` ``<-`` `[`DiffBetaMC`](https://jeksterslab.github.io/betaMC/reference/DiffBetaMC.md)`(``hc3``)`

## Methods

### summary

Summary of the results of
[`DiffBetaMC()`](https://jeksterslab.github.io/betaMC/reference/DiffBetaMC.md).

\
[`summary`](https://rdrr.io/r/base/summary.html)`(``mvn``)`\
`#> Call:`\
`#> DiffBetaMC(object = mvn)`\
`#> `\
`#> Differences of standardized regression slopes`\
`#> type = "mvn"`\
`#>                   est     se     R   0.05%    0.5%    2.5%  97.5%  99.5% 99.95%`\
`#> NARTIC-PCTGRT  0.1037 0.1357 20000 -0.3469 -0.2507 -0.1651 0.3647 0.4450 0.5439`\
`#> NARTIC-PCTSUPP 0.2319 0.1251 20000 -0.2136 -0.0942 -0.0219 0.4701 0.5354 0.6025`\
`#> PCTGRT-PCTSUPP 0.1282 0.1227 20000 -0.2852 -0.1901 -0.1155 0.3663 0.4363 0.5280`\
[`summary`](https://rdrr.io/r/base/summary.html)`(``adf``)`\
`#> Call:`\
`#> DiffBetaMC(object = adf)`\
`#> `\
`#> Differences of standardized regression slopes`\
`#> type = "adf"`\
`#>                   est     se     R   0.05%    0.5%    2.5%  97.5%  99.5% 99.95%`\
`#> NARTIC-PCTGRT  0.1037 0.1214 20000 -0.3196 -0.2124 -0.1349 0.3384 0.4081 0.5024`\
`#> NARTIC-PCTSUPP 0.2319 0.1183 20000 -0.1530 -0.0676 -0.0039 0.4560 0.5355 0.6304`\
`#> PCTGRT-PCTSUPP 0.1282 0.1223 20000 -0.2746 -0.1931 -0.1166 0.3633 0.4418 0.5174`\
[`summary`](https://rdrr.io/r/base/summary.html)`(``hc3``)`\
`#> Call:`\
`#> DiffBetaMC(object = hc3)`\
`#> `\
`#> Differences of standardized regression slopes`\
`#> type = "hc3"`\
`#>                   est     se     R   0.05%    0.5%    2.5%  97.5%  99.5% 99.95%`\
`#> NARTIC-PCTGRT  0.1037 0.1425 20000 -0.3745 -0.2619 -0.1781 0.3811 0.4737 0.5792`\
`#> NARTIC-PCTSUPP 0.2319 0.1327 20000 -0.2316 -0.1198 -0.0370 0.4800 0.5717 0.6545`\
`#> PCTGRT-PCTSUPP 0.1282 0.1374 20000 -0.3573 -0.2464 -0.1519 0.3928 0.4746 0.5486`

### coef

Return the vector of estimates.

\
[`coef`](https://rdrr.io/r/stats/coef.html)`(``mvn``)`\
`#>  NARTIC-PCTGRT NARTIC-PCTSUPP PCTGRT-PCTSUPP `\
`#>      0.1036564      0.2318974      0.1282410`\
[`coef`](https://rdrr.io/r/stats/coef.html)`(``adf``)`\
`#>  NARTIC-PCTGRT NARTIC-PCTSUPP PCTGRT-PCTSUPP `\
`#>      0.1036564      0.2318974      0.1282410`\
[`coef`](https://rdrr.io/r/stats/coef.html)`(``hc3``)`\
`#>  NARTIC-PCTGRT NARTIC-PCTSUPP PCTGRT-PCTSUPP `\
`#>      0.1036564      0.2318974      0.1282410`

### vcov

Return the sampling covariance matrix.

\
[`vcov`](https://rdrr.io/r/stats/vcov.html)`(``mvn``)`\
`#>                NARTIC-PCTGRT NARTIC-PCTSUPP PCTGRT-PCTSUPP`\
`#> NARTIC-PCTGRT    0.018415625    0.009501368   -0.008914258`\
`#> NARTIC-PCTSUPP   0.009501368    0.015638911    0.006137543`\
`#> PCTGRT-PCTSUPP  -0.008914258    0.006137543    0.015051801`\
[`vcov`](https://rdrr.io/r/stats/vcov.html)`(``adf``)`\
`#>                NARTIC-PCTGRT NARTIC-PCTSUPP PCTGRT-PCTSUPP`\
`#> NARTIC-PCTGRT    0.014738725    0.006896875   -0.007841851`\
`#> NARTIC-PCTSUPP   0.006896875    0.014004385    0.007107510`\
`#> PCTGRT-PCTSUPP  -0.007841851    0.007107510    0.014949361`\
[`vcov`](https://rdrr.io/r/stats/vcov.html)`(``hc3``)`\
`#>                NARTIC-PCTGRT NARTIC-PCTSUPP PCTGRT-PCTSUPP`\
`#> NARTIC-PCTGRT     0.02030562    0.009523790   -0.010781831`\
`#> NARTIC-PCTSUPP    0.00952379    0.017609955    0.008086164`\
`#> PCTGRT-PCTSUPP   -0.01078183    0.008086164    0.018867995`

### confint

Return confidence intervals.

\
[`confint`](https://rdrr.io/r/stats/confint.html)`(``mvn``, level ``=`` ``0.95``)`\
`#>                      2.5 %    97.5 %`\
`#> NARTIC-PCTGRT  -0.16512972 0.3647204`\
`#> NARTIC-PCTSUPP -0.02193225 0.4701166`\
`#> PCTGRT-PCTSUPP -0.11548008 0.3662706`\
[`confint`](https://rdrr.io/r/stats/confint.html)`(``adf``, level ``=`` ``0.95``)`\
`#>                       2.5 %    97.5 %`\
`#> NARTIC-PCTGRT  -0.134903338 0.3383689`\
`#> NARTIC-PCTSUPP -0.003920906 0.4559979`\
`#> PCTGRT-PCTSUPP -0.116565175 0.3632955`\
[`confint`](https://rdrr.io/r/stats/confint.html)`(``hc3``, level ``=`` ``0.95``)`\
`#>                      2.5 %    97.5 %`\
`#> NARTIC-PCTGRT  -0.17807592 0.3810693`\
`#> NARTIC-PCTSUPP -0.03696402 0.4799981`\
`#> PCTGRT-PCTSUPP -0.15191434 0.3928330`

## References

Dudgeon, P. (2017). Some improvements in confidence intervals for
standardized regression coefficients. *Psychometrika*, *82*(4), 928–951.
<https://doi.org/10.1007/s11336-017-9563-z>
