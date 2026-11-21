# betaMC: Example Using the SCorMC Function

Confidence intervals for semipartial correlation coefficients are
generated using the
[`SCorMC()`](https://jeksterslab.github.io/betaMC/reference/SCorMC.md)
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

### Semipartial Correlation Coefficients

#### Normal-Theory Approach

\
`mvn`` ``<-`` `[`SCorMC`](https://jeksterslab.github.io/betaMC/reference/SCorMC.md)`(``object ``=`` ``mvn``)`

#### Asymptotic distribution-free Approach

\
`adf`` ``<-`` `[`SCorMC`](https://jeksterslab.github.io/betaMC/reference/SCorMC.md)`(``object ``=`` ``adf``)`

#### Heteroskedasticity Consistent Approach (HC3)

\
`hc3`` ``<-`` `[`SCorMC`](https://jeksterslab.github.io/betaMC/reference/SCorMC.md)`(``object ``=`` ``hc3``)`

## Methods

### summary

Summary of the results of
[`SCorMC()`](https://jeksterslab.github.io/betaMC/reference/SCorMC.md).

\
[`summary`](https://rdrr.io/r/base/summary.html)`(``object ``=`` ``mvn``)`\
`#> Call:`\
`#> SCorMC(object = mvn)`\
`#> `\
`#> Semipartial correlations`\
`#> type = "mvn"`\
`#>            est     se     R  0.05%   0.5%   2.5%  97.5%  99.5% 99.95%`\
`#> NARTIC  0.4312 0.0770 20000 0.1681 0.2291 0.2714 0.5706 0.6249 0.6961`\
`#> PCTGRT  0.3430 0.0740 20000 0.1211 0.1571 0.1955 0.4829 0.5364 0.6008`\
`#> PCTSUPP 0.2385 0.0704 20000 0.0279 0.0644 0.1000 0.3781 0.4271 0.4957`\
[`summary`](https://rdrr.io/r/base/summary.html)`(``object ``=`` ``adf``)`\
`#> Call:`\
`#> SCorMC(object = adf)`\
`#> `\
`#> Semipartial correlations`\
`#> type = "adf"`\
`#>            est     se     R  0.05%   0.5%   2.5%  97.5%  99.5% 99.95%`\
`#> NARTIC  0.4312 0.0708 20000 0.1157 0.2112 0.2720 0.5495 0.5933 0.6477`\
`#> PCTGRT  0.3430 0.0708 20000 0.0882 0.1499 0.1919 0.4677 0.5159 0.5807`\
`#> PCTSUPP 0.2385 0.0695 20000 0.0032 0.0511 0.0955 0.3690 0.4164 0.4732`\
[`summary`](https://rdrr.io/r/base/summary.html)`(``object ``=`` ``hc3``)`\
`#> Call:`\
`#> SCorMC(object = hc3)`\
`#> `\
`#> Semipartial correlations`\
`#> type = "hc3"`\
`#>            est     se     R   0.05%   0.5%   2.5%  97.5%  99.5% 99.95%`\
`#> NARTIC  0.4312 0.0865 20000  0.0487 0.1407 0.2215 0.5667 0.6167 0.6788`\
`#> PCTGRT  0.3430 0.0831 20000  0.0483 0.1056 0.1602 0.4879 0.5407 0.6078`\
`#> PCTSUPP 0.2385 0.0789 20000 -0.0346 0.0248 0.0774 0.3889 0.4472 0.5182`

### coef

Return the vector of estimates.

\
`stats``::`[`coef`](https://rdrr.io/r/stats/coef.html)`(``object ``=`` ``mvn``)`\
`#>    NARTIC    PCTGRT   PCTSUPP `\
`#> 0.4311525 0.3430075 0.2384789`\
`stats``::`[`coef`](https://rdrr.io/r/stats/coef.html)`(``object ``=`` ``adf``)`\
`#>    NARTIC    PCTGRT   PCTSUPP `\
`#> 0.4311525 0.3430075 0.2384789`\
`stats``::`[`coef`](https://rdrr.io/r/stats/coef.html)`(``object ``=`` ``hc3``)`\
`#>    NARTIC    PCTGRT   PCTSUPP `\
`#> 0.4311525 0.3430075 0.2384789`

### vcov

Return the sampling covariance matrix.

\
`stats``::`[`vcov`](https://rdrr.io/r/stats/vcov.html)`(``object ``=`` ``mvn``)`\
`#>                NARTIC        PCTGRT       PCTSUPP`\
`#> NARTIC   0.0059268167 -0.0012858761 -0.0008769062`\
`#> PCTGRT  -0.0012858761  0.0054816608 -0.0007984442`\
`#> PCTSUPP -0.0008769062 -0.0007984442  0.0049547509`\
`stats``::`[`vcov`](https://rdrr.io/r/stats/vcov.html)`(``object ``=`` ``adf``)`\
`#>               NARTIC        PCTGRT       PCTSUPP`\
`#> NARTIC   0.005007964  0.0002532270 -0.0003993550`\
`#> PCTGRT   0.000253227  0.0050112638 -0.0006392964`\
`#> PCTSUPP -0.000399355 -0.0006392964  0.0048346776`\
`stats``::`[`vcov`](https://rdrr.io/r/stats/vcov.html)`(``object ``=`` ``hc3``)`\
`#>                NARTIC        PCTGRT       PCTSUPP`\
`#> NARTIC   0.0074834730  0.0008353545 -0.0001597632`\
`#> PCTGRT   0.0008353545  0.0068996994 -0.0006074884`\
`#> PCTSUPP -0.0001597632 -0.0006074884  0.0062271316`

### confint

Return confidence intervals.

\
`stats``::`[`confint`](https://rdrr.io/r/stats/confint.html)`(``object ``=`` ``mvn``, level ``=`` ``0.95``)`\
`#>              2.5 %    97.5 %`\
`#> NARTIC  0.27143275 0.5705824`\
`#> PCTGRT  0.19549174 0.4829498`\
`#> PCTSUPP 0.09995168 0.3780812`\
`stats``::`[`confint`](https://rdrr.io/r/stats/confint.html)`(``object ``=`` ``adf``, level ``=`` ``0.95``)`\
`#>              2.5 %    97.5 %`\
`#> NARTIC  0.27204612 0.5494827`\
`#> PCTGRT  0.19192682 0.4676552`\
`#> PCTSUPP 0.09549401 0.3690050`\
`stats``::`[`confint`](https://rdrr.io/r/stats/confint.html)`(``object ``=`` ``hc3``, level ``=`` ``0.95``)`\
`#>              2.5 %    97.5 %`\
`#> NARTIC  0.22151770 0.5666784`\
`#> PCTGRT  0.16023810 0.4879101`\
`#> PCTSUPP 0.07737701 0.3889084`

## References

Dudgeon, P. (2017). Some improvements in confidence intervals for
standardized regression coefficients. *Psychometrika*, *82*(4), 928–951.
<https://doi.org/10.1007/s11336-017-9563-z>

Pesigan, I. J. A., & Cheung, S. F. (2024). Monte Carlo confidence
intervals for the indirect effect with missing data. *Behavior Research
Methods*, *56*(3), 1678–1696.
<https://doi.org/10.3758/s13428-023-02114-4>
