# betaMC: Example Using the RSqMC Function

Confidence intervals for multiple correlation coefficients (R-squared
and adjusted R-squared) are generated using the
[`RSqMC()`](https://jeksterslab.github.io/betaMC/reference/RSqMC.md)
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

### Multiple Correlation Coefficients

#### Normal-Theory Approach

\
`mvn`` ``<-`` `[`RSqMC`](https://jeksterslab.github.io/betaMC/reference/RSqMC.md)`(``object ``=`` ``mvn``)`

#### Asymptotic distribution-free Approach

\
`adf`` ``<-`` `[`RSqMC`](https://jeksterslab.github.io/betaMC/reference/RSqMC.md)`(``object ``=`` ``adf``)`

#### Heteroskedasticity Consistent Approach (HC3)

\
`hc3`` ``<-`` `[`RSqMC`](https://jeksterslab.github.io/betaMC/reference/RSqMC.md)`(``object ``=`` ``hc3``)`

## Methods

### summary

Summary of the results of
[`RSqMC()`](https://jeksterslab.github.io/betaMC/reference/RSqMC.md).

\
[`summary`](https://rdrr.io/r/base/summary.html)`(``object ``=`` ``mvn``)`\
`#> Call:`\
`#> RSqMC(object = mvn)`\
`#> `\
`#> R-squared and adjusted R-squared`\
`#> type = "mvn"`\
`#>        est     se     R  0.05%   0.5%   2.5%  97.5%  99.5% 99.95%`\
`#> rsq 0.8045 0.0561 20000 0.5067 0.6038 0.6605 0.8809 0.9025 0.9283`\
`#> adj 0.7906 0.0601 20000 0.4715 0.5755 0.6363 0.8724 0.8955 0.9232`\
[`summary`](https://rdrr.io/r/base/summary.html)`(``object ``=`` ``adf``)`\
`#> Call:`\
`#> RSqMC(object = adf)`\
`#> `\
`#> R-squared and adjusted R-squared`\
`#> type = "adf"`\
`#>        est     se     R  0.05%   0.5%   2.5%  97.5%  99.5% 99.95%`\
`#> rsq 0.8045 0.0547 20000 0.5427 0.6122 0.6661 0.8796 0.9025 0.9212`\
`#> adj 0.7906 0.0586 20000 0.5100 0.5845 0.6423 0.8710 0.8956 0.9156`\
[`summary`](https://rdrr.io/r/base/summary.html)`(``object ``=`` ``hc3``)`\
`#> Call:`\
`#> RSqMC(object = hc3)`\
`#> `\
`#> R-squared and adjusted R-squared`\
`#> type = "hc3"`\
`#>        est     se     R  0.05% 0.5%   2.5% 97.5%  99.5% 99.95%`\
`#> rsq 0.8045 0.0617 20000 0.4685 0.58 0.6481 0.888 0.9111 0.9382`\
`#> adj 0.7906 0.0662 20000 0.4306 0.55 0.6230 0.880 0.9048 0.9338`

### coef

Return the vector of estimates.

\
`stats``::`[`coef`](https://rdrr.io/r/stats/coef.html)`(``object ``=`` ``mvn``)`\
`#>       rsq       adj `\
`#> 0.8045263 0.7905638`\
`stats``::`[`coef`](https://rdrr.io/r/stats/coef.html)`(``object ``=`` ``adf``)`\
`#>       rsq       adj `\
`#> 0.8045263 0.7905638`\
`stats``::`[`coef`](https://rdrr.io/r/stats/coef.html)`(``object ``=`` ``hc3``)`\
`#>       rsq       adj `\
`#> 0.8045263 0.7905638`

### vcov

Return the sampling covariance matrix.

\
`stats``::`[`vcov`](https://rdrr.io/r/stats/vcov.html)`(``object ``=`` ``mvn``)`\
`#>             rsq         adj`\
`#> rsq 0.003146481 0.003371229`\
`#> adj 0.003371229 0.003612031`\
`stats``::`[`vcov`](https://rdrr.io/r/stats/vcov.html)`(``object ``=`` ``adf``)`\
`#>             rsq         adj`\
`#> rsq 0.002990290 0.003203882`\
`#> adj 0.003203882 0.003432730`\
`stats``::`[`vcov`](https://rdrr.io/r/stats/vcov.html)`(``object ``=`` ``hc3``)`\
`#>             rsq         adj`\
`#> rsq 0.003812644 0.004084976`\
`#> adj 0.004084976 0.004376760`

### confint

Return confidence intervals.

\
`stats``::`[`confint`](https://rdrr.io/r/stats/confint.html)`(``object ``=`` ``mvn``, level ``=`` ``0.95``)`\
`#>         2.5 %    97.5 %`\
`#> rsq 0.6605064 0.8808726`\
`#> adj 0.6362568 0.8723635`\
`stats``::`[`confint`](https://rdrr.io/r/stats/confint.html)`(``object ``=`` ``adf``, level ``=`` ``0.95``)`\
`#>         2.5 %    97.5 %`\
`#> rsq 0.6661370 0.8796453`\
`#> adj 0.6422896 0.8710486`\
`stats``::`[`confint`](https://rdrr.io/r/stats/confint.html)`(``object ``=`` ``hc3``, level ``=`` ``0.95``)`\
`#>         2.5 %    97.5 %`\
`#> rsq 0.6480992 0.8879830`\
`#> adj 0.6229635 0.8799818`

## References

Dudgeon, P. (2017). Some improvements in confidence intervals for
standardized regression coefficients. *Psychometrika*, *82*(4), 928–951.
<https://doi.org/10.1007/s11336-017-9563-z>

Pesigan, I. J. A., & Cheung, S. F. (2024). Monte Carlo confidence
intervals for the indirect effect with missing data. *Behavior Research
Methods*, *56*(3), 1678–1696.
<https://doi.org/10.3758/s13428-023-02114-4>
