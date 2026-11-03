# betaMC

Ivan Jacob Agaloos Pesigan 2026-11-03

## Description

Generates Monte Carlo confidence intervals for standardized regression
coefficients (beta) and other effect sizes, including multiple
correlation, semipartial correlations, improvement in R-squared, squared
partial correlations, and differences in standardized regression
coefficients, for models fitted by
[`lm()`](https://rdrr.io/r/stats/lm.html). `betaMC` combines ideas from
Monte Carlo confidence intervals for the indirect effect (Pesigan and
Cheung, 2024: <http://doi.org/10.3758/s13428-023-02114-4>) and the
sampling covariance matrix of regression coefficients (Dudgeon, 2017:
<http://doi.org/10.1007/s11336-017-9563-z>) to generate confidence
intervals effect sizes in regression.

## Installation

You can install the CRAN release of `betaMC` with:

\
[`install.packages`](https://rdrr.io/r/utils/install.packages.html)`(``"betaMC"``)`

You can install the development version of `betaMC` from
[GitHub](https://github.com/jeksterslab/betaMC) with:

\
`if`` ``(``!`[`require`](https://rdrr.io/r/base/library.html)`(`[`"pak"`](https://pak.r-lib.org/)`)``)`` `[`install.packages`](https://rdrr.io/r/utils/install.packages.html)`(``"pak"``)`\
`pak``::`[`pkg_install`](https://pak.r-lib.org/reference/pkg_install.html)`(``"jeksterslab/betaMC"``)`

## Example

In this example, a multiple regression model is fitted using program
quality ratings (`QUALITY`) as the regressand/outcome variable and
number of published articles attributed to the program faculty members
(`NARTIC`), percent of faculty members holding research grants
(`PCTGRT`), and percentage of program graduates who received support
(`PCTSUPP`) as regressor/predictor variables using a data set from 1982
ratings of 46 doctoral programs in psychology in the USA (National
Research Council, 1982). Confidence intervals for the standardized
regression coefficients are generated using the
[`BetaMC()`](https://github.com/jeksterslab/betaMC/reference/BetaMC.md)
function from the `betaMC` package.

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

### Standardized Regression Slopes

#### Normal-Theory Approach

\
[`BetaMC`](https://github.com/jeksterslab/betaMC/reference/BetaMC.md)`(``mvn``, alpha ``=`` ``0.05``)`\
`#> Call:`\
`#> BetaMC(object = mvn, alpha = 0.05)`\
`#> `\
`#> Standardized regression slopes`\
`#> type = "mvn"`\
`#>            est     se     R   2.5%  97.5%`\
`#> NARTIC  0.4951 0.0751 20000 0.3416 0.6331`\
`#> PCTGRT  0.3915 0.0765 20000 0.2388 0.5385`\
`#> PCTSUPP 0.2632 0.0743 20000 0.1191 0.4102`

#### Asymptotic distribution-free Approach

\
[`BetaMC`](https://github.com/jeksterslab/betaMC/reference/BetaMC.md)`(``adf``, alpha ``=`` ``0.05``)`\
`#> Call:`\
`#> BetaMC(object = adf, alpha = 0.05)`\
`#> `\
`#> Standardized regression slopes`\
`#> type = "adf"`\
`#>            est     se     R   2.5%  97.5%`\
`#> NARTIC  0.4951 0.0678 20000 0.3523 0.6147`\
`#> PCTGRT  0.3915 0.0704 20000 0.2451 0.5197`\
`#> PCTSUPP 0.2632 0.0773 20000 0.1062 0.4103`

#### Heteroskedasticity Consistent Approach (HC3)

\
[`BetaMC`](https://github.com/jeksterslab/betaMC/reference/BetaMC.md)`(``hc3``, alpha ``=`` ``0.05``)`\
`#> Call:`\
`#> BetaMC(object = hc3, alpha = 0.05)`\
`#> `\
`#> Standardized regression slopes`\
`#> type = "hc3"`\
`#>            est     se     R   2.5%  97.5%`\
`#> NARTIC  0.4951 0.0796 20000 0.3227 0.6324`\
`#> PCTGRT  0.3915 0.0822 20000 0.2209 0.5437`\
`#> PCTSUPP 0.2632 0.0859 20000 0.0890 0.4256`

### Other Effect Sizes

The `betaMC` package also has functions to generate Monte Carlo
confidence intervals for other effect sizes such as
[`RSqMC()`](https://github.com/jeksterslab/betaMC/reference/RSqMC.md)
for multiple correlation coefficients (R-squared and adjusted
R-squared),
[`DeltaRSqMC()`](https://github.com/jeksterslab/betaMC/reference/DeltaRSqMC.md)
for improvement in R-squared,
[`SCorMC()`](https://github.com/jeksterslab/betaMC/reference/SCorMC.md)
for semipartial correlation coefficients,
[`PCorMC()`](https://github.com/jeksterslab/betaMC/reference/PCorMC.md)
for squared partial correlation coefficients, and
[`DiffBetaMC()`](https://github.com/jeksterslab/betaMC/reference/DiffBetaMC.md)
for differences of standardized regression coefficients.

#### Multiple Correlation Coefficients (R-squared and adjusted R-squared)

\
[`RSqMC`](https://github.com/jeksterslab/betaMC/reference/RSqMC.md)`(``hc3``, alpha ``=`` ``0.05``)`\
`#> Call:`\
`#> RSqMC(object = hc3, alpha = 0.05)`\
`#> `\
`#> R-squared and adjusted R-squared`\
`#> type = "hc3"`\
`#>        est     se     R   2.5%  97.5%`\
`#> rsq 0.8045 0.0623 20000 0.6451 0.8874`\
`#> adj 0.7906 0.0667 20000 0.6198 0.8794`

#### Improvement in R-squared

\
[`DeltaRSqMC`](https://github.com/jeksterslab/betaMC/reference/DeltaRSqMC.md)`(``hc3``, alpha ``=`` ``0.05``)`\
`#> Call:`\
`#> DeltaRSqMC(object = hc3, alpha = 0.05)`\
`#> `\
`#> Improvement in R-squared`\
`#> type = "hc3"`\
`#>            est     se     R   2.5%  97.5%`\
`#> NARTIC  0.1859 0.0689 20000 0.0492 0.3220`\
`#> PCTGRT  0.1177 0.0550 20000 0.0261 0.2400`\
`#> PCTSUPP 0.0569 0.0377 20000 0.0060 0.1495`

#### Semipartial Correlation Coefficients

\
[`SCorMC`](https://github.com/jeksterslab/betaMC/reference/SCorMC.md)`(``hc3``, alpha ``=`` ``0.05``)`\
`#> Call:`\
`#> SCorMC(object = hc3, alpha = 0.05)`\
`#> `\
`#> Semipartial correlations`\
`#> type = "hc3"`\
`#>            est     se     R   2.5%  97.5%`\
`#> NARTIC  0.4312 0.0867 20000 0.2218 0.5675`\
`#> PCTGRT  0.3430 0.0831 20000 0.1615 0.4899`\
`#> PCTSUPP 0.2385 0.0784 20000 0.0776 0.3866`

#### Squared Partial Correlation Coefficients

\
[`PCorMC`](https://github.com/jeksterslab/betaMC/reference/PCorMC.md)`(``hc3``, alpha ``=`` ``0.05``)`\
`#> Call:`\
`#> PCorMC(object = hc3, alpha = 0.05)`\
`#> `\
`#> Squared partial correlations`\
`#> type = "hc3"`\
`#>            est     se     R   2.5%  97.5%`\
`#> NARTIC  0.4874 0.1194 20000 0.1764 0.6495`\
`#> PCTGRT  0.3757 0.1159 20000 0.1059 0.5584`\
`#> PCTSUPP 0.2254 0.1131 20000 0.0245 0.4584`

#### Differences of Standardized Regression Coefficients

\
[`DiffBetaMC`](https://github.com/jeksterslab/betaMC/reference/DiffBetaMC.md)`(``hc3``, alpha ``=`` ``0.05``)`\
`#> Call:`\
`#> DiffBetaMC(object = hc3, alpha = 0.05)`\
`#> `\
`#> Differences of standardized regression slopes`\
`#> type = "hc3"`\
`#>                   est     se     R    2.5%  97.5%`\
`#> NARTIC-PCTGRT  0.1037 0.1419 20000 -0.1783 0.3725`\
`#> NARTIC-PCTSUPP 0.2319 0.1332 20000 -0.0418 0.4813`\
`#> PCTGRT-PCTSUPP 0.1282 0.1377 20000 -0.1459 0.3931`

## Documentation

See [GitHub Pages](https://jeksterslab.github.io/betaMC/index.html) for
package documentation.

## Citation

To cite `betaMC` in publications, please cite Pesigan & Cheung (2024).

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
