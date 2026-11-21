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

### Standardized Regression Slopes

#### Normal-Theory Approach

\
`mvn`` ``<-`` `[`BetaMC`](https://jeksterslab.github.io/betaMC/reference/BetaMC.md)`(``object ``=`` ``mvn``)`

#### Asymptotic distribution-free Approach

\
`adf`` ``<-`` `[`BetaMC`](https://jeksterslab.github.io/betaMC/reference/BetaMC.md)`(``object ``=`` ``adf``)`

#### Heteroskedasticity Consistent Approach (HC3)

\
`hc3`` ``<-`` `[`BetaMC`](https://jeksterslab.github.io/betaMC/reference/BetaMC.md)`(``object ``=`` ``hc3``)`

## Methods

### summary

Summary of the results of
[`BetaMC()`](https://jeksterslab.github.io/betaMC/reference/BetaMC.md).

\
[`summary`](https://rdrr.io/r/base/summary.html)`(``object ``=`` ``mvn``)`\
`#> Call:`\
`#> BetaMC(object = mvn)`\
`#> `\
`#> Standardized regression slopes`\
`#> type = "mvn"`\
`#>            est     se     R  0.05%   0.5%   2.5%  97.5%  99.5% 99.95%`\
`#> NARTIC  0.4951 0.0765 20000 0.2315 0.2897 0.3369 0.6354 0.6808 0.7300`\
`#> PCTGRT  0.3915 0.0762 20000 0.1424 0.1934 0.2379 0.5362 0.5821 0.6351`\
`#> PCTSUPP 0.2632 0.0746 20000 0.0290 0.0773 0.1187 0.4092 0.4618 0.5168`\
[`summary`](https://rdrr.io/r/base/summary.html)`(``object ``=`` ``adf``)`\
`#> Call:`\
`#> BetaMC(object = adf)`\
`#> `\
`#> Standardized regression slopes`\
`#> type = "adf"`\
`#>            est     se     R  0.05%   0.5%   2.5%  97.5%  99.5% 99.95%`\
`#> NARTIC  0.4951 0.0677 20000 0.2642 0.3133 0.3516 0.6166 0.6570 0.6989`\
`#> PCTGRT  0.3915 0.0708 20000 0.1339 0.1932 0.2420 0.5195 0.5633 0.6099`\
`#> PCTSUPP 0.2632 0.0761 20000 0.0020 0.0574 0.1047 0.4057 0.4515 0.5036`\
[`summary`](https://rdrr.io/r/base/summary.html)`(``object ``=`` ``hc3``)`\
`#> Call:`\
`#> BetaMC(object = hc3)`\
`#> `\
`#> Standardized regression slopes`\
`#> type = "hc3"`\
`#>            est     se     R   0.05%   0.5%   2.5%  97.5%  99.5% 99.95%`\
`#> NARTIC  0.4951 0.0804 20000  0.2058 0.2713 0.3217 0.6369 0.6830 0.7389`\
`#> PCTGRT  0.3915 0.0830 20000  0.0907 0.1594 0.2170 0.5416 0.5858 0.6455`\
`#> PCTSUPP 0.2632 0.0855 20000 -0.0305 0.0299 0.0876 0.4258 0.4790 0.5494`

### coef

Return the vector of estimates.

\
`stats``::`[`coef`](https://rdrr.io/r/stats/coef.html)`(``object ``=`` ``mvn``)`\
`#>    NARTIC    PCTGRT   PCTSUPP `\
`#> 0.4951451 0.3914887 0.2632477`\
`stats``::`[`coef`](https://rdrr.io/r/stats/coef.html)`(``object ``=`` ``adf``)`\
`#>    NARTIC    PCTGRT   PCTSUPP `\
`#> 0.4951451 0.3914887 0.2632477`\
`stats``::`[`coef`](https://rdrr.io/r/stats/coef.html)`(``object ``=`` ``hc3``)`\
`#>    NARTIC    PCTGRT   PCTSUPP `\
`#> 0.4951451 0.3914887 0.2632477`

### vcov

Return the sampling covariance matrix.

\
`stats``::`[`vcov`](https://rdrr.io/r/stats/vcov.html)`(``object ``=`` ``mvn``)`\
`#>               NARTIC       PCTGRT      PCTSUPP`\
`#> NARTIC   0.005856019 -0.003306578 -0.002193270`\
`#> PCTGRT  -0.003306578  0.005798830 -0.001654611`\
`#> PCTSUPP -0.002193270 -0.001654611  0.005559314`\
`stats``::`[`vcov`](https://rdrr.io/r/stats/vcov.html)`(``object ``=`` ``adf``)`\
`#>               NARTIC       PCTGRT      PCTSUPP`\
`#> NARTIC   0.004586158 -0.002538012 -0.001693544`\
`#> PCTGRT  -0.002538012  0.005011604 -0.001795002`\
`#> PCTSUPP -0.001693544 -0.001795002  0.005792288`\
`stats``::`[`vcov`](https://rdrr.io/r/stats/vcov.html)`(``object ``=`` ``hc3``)`\
`#>               NARTIC       PCTGRT      PCTSUPP`\
`#> NARTIC   0.006459017 -0.003669376 -0.002022395`\
`#> PCTGRT  -0.003669376  0.006883886 -0.002367254`\
`#> PCTSUPP -0.002022395 -0.002367254  0.007304423`

### confint

Return confidence intervals.

\
`stats``::`[`confint`](https://rdrr.io/r/stats/confint.html)`(``object ``=`` ``mvn``, level ``=`` ``0.95``)`\
`#>             2.5 %    97.5 %`\
`#> NARTIC  0.3368786 0.6353753`\
`#> PCTGRT  0.2379438 0.5361703`\
`#> PCTSUPP 0.1187428 0.4092017`\
`stats``::`[`confint`](https://rdrr.io/r/stats/confint.html)`(``object ``=`` ``adf``, level ``=`` ``0.95``)`\
`#>             2.5 %    97.5 %`\
`#> NARTIC  0.3516068 0.6165669`\
`#> PCTGRT  0.2420371 0.5195099`\
`#> PCTSUPP 0.1047297 0.4057334`\
`stats``::`[`confint`](https://rdrr.io/r/stats/confint.html)`(``object ``=`` ``hc3``, level ``=`` ``0.95``)`\
`#>              2.5 %    97.5 %`\
`#> NARTIC  0.32170117 0.6369219`\
`#> PCTGRT  0.21702340 0.5416418`\
`#> PCTSUPP 0.08758336 0.4258405`

## References

Dudgeon, P. (2017). Some improvements in confidence intervals for
standardized regression coefficients. *Psychometrika*, *82*(4), 928–951.
<https://doi.org/10.1007/s11336-017-9563-z>

National Research Council. (1982). *An assessment of research-doctorate
programs in the United States: Social and behavioral sciences*. National
Academies Press. <https://doi.org/10.17226/9781>

Pesigan, I. J. A., & Cheung, S. F. (2024). Monte Carlo confidence
intervals for the indirect effect with missing data. *Behavior Research
Methods*, *56*(3), 1678–1696.
<https://doi.org/10.3758/s13428-023-02114-4>
