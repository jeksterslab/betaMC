# betaMC: Example Using the SCorMC Function

Confidence intervals for semipartial correlation coefficients are
generated using the
[`SCorMC()`](https://jeksterslab.github.io/betaMC/reference/SCorMC.md)
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

### Semipartial Correlation Coefficients

#### Normal-Theory Approach

\
`mvn`` ``<-`` `[`SCorMC`](https://jeksterslab.github.io/betaMC/reference/SCorMC.md)`(``mvn``)`

#### Asymptotic distribution-free Approach

\
`adf`` ``<-`` `[`SCorMC`](https://jeksterslab.github.io/betaMC/reference/SCorMC.md)`(``adf``)`

#### Heteroskedasticity Consistent Approach (HC3)

\
`hc3`` ``<-`` `[`SCorMC`](https://jeksterslab.github.io/betaMC/reference/SCorMC.md)`(``hc3``)`

## Methods

### summary

Summary of the results of
[`SCorMC()`](https://jeksterslab.github.io/betaMC/reference/SCorMC.md).

\
[`summary`](https://rdrr.io/r/base/summary.html)`(``mvn``)`\
`#> Call:`\
`#> SCorMC(object = mvn)`\
`#> `\
`#> Semipartial correlations`\
`#> type = "mvn"`\
`#>            est     se     R  0.05%   0.5%   2.5%  97.5%  99.5% 99.95%`\
`#> NARTIC  0.4312 0.0771 20000 0.1828 0.2286 0.2718 0.5720 0.6255 0.6888`\
`#> PCTGRT  0.3430 0.0733 20000 0.1112 0.1551 0.1947 0.4823 0.5328 0.5784`\
`#> PCTSUPP 0.2385 0.0700 20000 0.0262 0.0674 0.1030 0.3786 0.4365 0.4887`\
[`summary`](https://rdrr.io/r/base/summary.html)`(``adf``)`\
`#> Call:`\
`#> SCorMC(object = adf)`\
`#> `\
`#> Semipartial correlations`\
`#> type = "adf"`\
`#>            est     se     R  0.05%   0.5%   2.5%  97.5%  99.5% 99.95%`\
`#> NARTIC  0.4312 0.0708 20000 0.1157 0.2112 0.2720 0.5496 0.5935 0.6477`\
`#> PCTGRT  0.3430 0.0708 20000 0.0882 0.1499 0.1919 0.4673 0.5159 0.5807`\
`#> PCTSUPP 0.2385 0.0696 20000 0.0032 0.0511 0.0955 0.3690 0.4160 0.4732`\
[`summary`](https://rdrr.io/r/base/summary.html)`(``hc3``)`\
`#> Call:`\
`#> SCorMC(object = hc3)`\
`#> `\
`#> Semipartial correlations`\
`#> type = "hc3"`\
`#>            est     se     R   0.05%   0.5%   2.5%  97.5%  99.5% 99.95%`\
`#> NARTIC  0.4312 0.0865 20000  0.0487 0.1407 0.2215 0.5667 0.6168 0.6788`\
`#> PCTGRT  0.3430 0.0831 20000  0.0483 0.1056 0.1604 0.4879 0.5407 0.6078`\
`#> PCTSUPP 0.2385 0.0789 20000 -0.0346 0.0248 0.0775 0.3889 0.4467 0.5182`

### coef

Return the vector of estimates.

\
[`coef`](https://rdrr.io/r/stats/coef.html)`(``mvn``)`\
`#>    NARTIC    PCTGRT   PCTSUPP `\
`#> 0.4311525 0.3430075 0.2384789`\
[`coef`](https://rdrr.io/r/stats/coef.html)`(``adf``)`\
`#>    NARTIC    PCTGRT   PCTSUPP `\
`#> 0.4311525 0.3430075 0.2384789`\
[`coef`](https://rdrr.io/r/stats/coef.html)`(``hc3``)`\
`#>    NARTIC    PCTGRT   PCTSUPP `\
`#> 0.4311525 0.3430075 0.2384789`

### vcov

Return the sampling covariance matrix.

\
[`vcov`](https://rdrr.io/r/stats/vcov.html)`(``mvn``)`\
`#>                NARTIC        PCTGRT       PCTSUPP`\
`#> NARTIC   0.0059390404 -0.0012721726 -0.0008609524`\
`#> PCTGRT  -0.0012721726  0.0053658116 -0.0006949645`\
`#> PCTSUPP -0.0008609524 -0.0006949645  0.0048964555`\
[`vcov`](https://rdrr.io/r/stats/vcov.html)`(``adf``)`\
`#>                NARTIC        PCTGRT       PCTSUPP`\
`#> NARTIC   0.0050107401  0.0002460707 -0.0004010920`\
`#> PCTGRT   0.0002460707  0.0050101465 -0.0006346065`\
`#> PCTSUPP -0.0004010920 -0.0006346065  0.0048373115`\
[`vcov`](https://rdrr.io/r/stats/vcov.html)`(``hc3``)`\
`#>                NARTIC        PCTGRT       PCTSUPP`\
`#> NARTIC   0.0074841276  0.0008331550 -0.0001594435`\
`#> PCTGRT   0.0008331550  0.0069004908 -0.0006089837`\
`#> PCTSUPP -0.0001594435 -0.0006089837  0.0062239417`

### confint

Return confidence intervals.

\
[`confint`](https://rdrr.io/r/stats/confint.html)`(``mvn``, level ``=`` ``0.95``)`\
`#>             2.5 %    97.5 %`\
`#> NARTIC  0.2718302 0.5719749`\
`#> PCTGRT  0.1946928 0.4822687`\
`#> PCTSUPP 0.1029988 0.3785698`\
[`confint`](https://rdrr.io/r/stats/confint.html)`(``adf``, level ``=`` ``0.95``)`\
`#>              2.5 %    97.5 %`\
`#> NARTIC  0.27204612 0.5495748`\
`#> PCTGRT  0.19192682 0.4672548`\
`#> PCTSUPP 0.09553564 0.3690101`\
[`confint`](https://rdrr.io/r/stats/confint.html)`(``hc3``, level ``=`` ``0.95``)`\
`#>              2.5 %    97.5 %`\
`#> NARTIC  0.22151770 0.5666839`\
`#> PCTGRT  0.16039190 0.4879101`\
`#> PCTSUPP 0.07746136 0.3889084`

## References

Dudgeon, P. (2017). Some improvements in confidence intervals for
standardized regression coefficients. *Psychometrika*, *82*(4), 928–951.
<https://doi.org/10.1007/s11336-017-9563-z>
