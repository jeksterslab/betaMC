# betaMC: Example Using the BetaMC Function

In this example, a multiple regression model is fitted using program
quality ratings (`QUALITY`) as the regressand/outcome variable and
number of published articles attributed to the program faculty members
(`NARTIC`), percent of faculty members holding research grants
(`PCTGRT`), and percentage of program graduates who received support
(`PCTSUPP`) as regressor/predictor variables using a data set from 1982
ratings of 46 doctoral programs in psychology in the USA (National
Research Council, 1982). Confidence intervals for the standardized
regression coefficients are generated using the
[`BetaMC()`](https://jeksterslab.github.io/betaMC/reference/BetaMC.md)
function from the `betaMC` package.

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

### Standardized Regression Slopes

#### Normal-Theory Approach

\
`mvn`` ``<-`` `[`BetaMC`](https://jeksterslab.github.io/betaMC/reference/BetaMC.md)`(``mvn``)`

#### Asymptotic distribution-free Approach

\
`adf`` ``<-`` `[`BetaMC`](https://jeksterslab.github.io/betaMC/reference/BetaMC.md)`(``adf``)`

#### Heteroskedasticity Consistent Approach (HC3)

\
`hc3`` ``<-`` `[`BetaMC`](https://jeksterslab.github.io/betaMC/reference/BetaMC.md)`(``hc3``)`

## Methods

### summary

Summary of the results of
[`BetaMC()`](https://jeksterslab.github.io/betaMC/reference/BetaMC.md).

\
[`summary`](https://rdrr.io/r/base/summary.html)`(``mvn``)`\
`#> Call:`\
`#> BetaMC(object = mvn)`\
`#> `\
`#> Standardized regression slopes`\
`#> type = "mvn"`\
`#>            est     se     R  0.05%   0.5%   2.5%  97.5%  99.5% 99.95%`\
`#> NARTIC  0.4951 0.0762 20000 0.2324 0.2920 0.3391 0.6377 0.6796 0.7263`\
`#> PCTGRT  0.3915 0.0773 20000 0.1424 0.1922 0.2371 0.5393 0.5919 0.6462`\
`#> PCTSUPP 0.2632 0.0747 20000 0.0274 0.0758 0.1181 0.4077 0.4615 0.5244`\
[`summary`](https://rdrr.io/r/base/summary.html)`(``adf``)`\
`#> Call:`\
`#> BetaMC(object = adf)`\
`#> `\
`#> Standardized regression slopes`\
`#> type = "adf"`\
`#>            est     se     R  0.05%   0.5%   2.5%  97.5%  99.5% 99.95%`\
`#> NARTIC  0.4951 0.0678 20000 0.2591 0.3088 0.3494 0.6152 0.6559 0.6942`\
`#> PCTGRT  0.3915 0.0711 20000 0.1449 0.1955 0.2413 0.5215 0.5625 0.6120`\
`#> PCTSUPP 0.2632 0.0767 20000 0.0134 0.0582 0.1079 0.4080 0.4490 0.4999`\
[`summary`](https://rdrr.io/r/base/summary.html)`(``hc3``)`\
`#> Call:`\
`#> BetaMC(object = hc3)`\
`#> `\
`#> Standardized regression slopes`\
`#> type = "hc3"`\
`#>            est     se     R   0.05%   0.5%   2.5%  97.5%  99.5% 99.95%`\
`#> NARTIC  0.4951 0.0794 20000  0.2178 0.2690 0.3228 0.6339 0.6745 0.7254`\
`#> PCTGRT  0.3915 0.0824 20000  0.0960 0.1575 0.2169 0.5406 0.5856 0.6371`\
`#> PCTSUPP 0.2632 0.0856 20000 -0.0318 0.0278 0.0889 0.4283 0.4782 0.5403`

### coef

Return the vector of estimates.

\
[`coef`](https://rdrr.io/r/stats/coef.html)`(``mvn``)`\
`#>    NARTIC    PCTGRT   PCTSUPP `\
`#> 0.4951451 0.3914887 0.2632477`\
[`coef`](https://rdrr.io/r/stats/coef.html)`(``adf``)`\
`#>    NARTIC    PCTGRT   PCTSUPP `\
`#> 0.4951451 0.3914887 0.2632477`\
[`coef`](https://rdrr.io/r/stats/coef.html)`(``hc3``)`\
`#>    NARTIC    PCTGRT   PCTSUPP `\
`#> 0.4951451 0.3914887 0.2632477`

### vcov

Return the sampling covariance matrix.

\
[`vcov`](https://rdrr.io/r/stats/vcov.html)`(``mvn``)`\
`#>               NARTIC       PCTGRT      PCTSUPP`\
`#> NARTIC   0.005811264 -0.003377116 -0.002133562`\
`#> PCTGRT  -0.003377116  0.005977965 -0.001726979`\
`#> PCTSUPP -0.002133562 -0.001726979  0.005577850`\
[`vcov`](https://rdrr.io/r/stats/vcov.html)`(``adf``)`\
`#>               NARTIC       PCTGRT      PCTSUPP`\
`#> NARTIC   0.004600246 -0.002562396 -0.001689232`\
`#> PCTGRT  -0.002562396  0.005051016 -0.001860037`\
`#> PCTSUPP -0.001689232 -0.001860037  0.005887343`\
[`vcov`](https://rdrr.io/r/stats/vcov.html)`(``hc3``)`\
`#>               NARTIC       PCTGRT      PCTSUPP`\
`#> NARTIC   0.006311116 -0.003622202 -0.001964332`\
`#> PCTGRT  -0.003622202  0.006797582 -0.002339048`\
`#> PCTSUPP -0.001964332 -0.002339048  0.007325528`

### confint

Return confidence intervals.

\
[`confint`](https://rdrr.io/r/stats/confint.html)`(``mvn``, level ``=`` ``0.95``)`\
`#>             2.5 %    97.5 %`\
`#> NARTIC  0.3391226 0.6377052`\
`#> PCTGRT  0.2370561 0.5393107`\
`#> PCTSUPP 0.1180515 0.4077326`\
[`confint`](https://rdrr.io/r/stats/confint.html)`(``adf``, level ``=`` ``0.95``)`\
`#>             2.5 %    97.5 %`\
`#> NARTIC  0.3493922 0.6151987`\
`#> PCTGRT  0.2413328 0.5215218`\
`#> PCTSUPP 0.1078522 0.4079892`\
[`confint`](https://rdrr.io/r/stats/confint.html)`(``hc3``, level ``=`` ``0.95``)`\
`#>              2.5 %    97.5 %`\
`#> NARTIC  0.32277942 0.6338670`\
`#> PCTGRT  0.21693842 0.5406124`\
`#> PCTSUPP 0.08894597 0.4283310`

## References

Dudgeon, P. (2017). Some improvements in confidence intervals for
standardized regression coefficients. *Psychometrika*, *82*(4), 928–951.
<https://doi.org/10.1007/s11336-017-9563-z>

National Research Council. (1982). *An assessment of research-doctorate
programs in the United States: Social and behavioral sciences*. National
Academies Press. <https://doi.org/10.17226/9781>
