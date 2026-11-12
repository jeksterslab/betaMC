# betaMC: Monte Carlo for Regression Effect Sizes

[![DOI](https://zenodo.org/badge/DOI/10.3758/s13428-023-02114-4.svg)](https://doi.org/10.3758/s13428-023-02114-4)
[![CRAN
Status](https://www.r-pkg.org/badges/version/betaMC)](https://cran.r-project.org/package=betaMC)
[![R-Universe
Status](https://jeksterslab.r-universe.dev/badges/betaMC)](https://jeksterslab.r-universe.dev)
[![Make
Project](https://github.com/jeksterslab/betaMC/actions/workflows/make.yml/badge.svg)](https://github.com/jeksterslab/betaMC/actions/workflows/make.yml)
[![R-CMD-check-standard](https://github.com/jeksterslab/betaMC/actions/workflows/check-standard.yml/badge.svg)](https://github.com/jeksterslab/betaMC/actions/workflows/check-standard.yml)
[![R-CMD-check-oldrel](https://github.com/jeksterslab/betaMC/actions/workflows/check-oldrel.yml/badge.svg)](https://github.com/jeksterslab/betaMC/actions/workflows/check-oldrel.yml)
[![R Package Test
Coverage](https://github.com/jeksterslab/betaMC/actions/workflows/test-coverage.yml/badge.svg)](https://github.com/jeksterslab/betaMC/actions/workflows/test-coverage.yml)
[![Lint R
Package](https://github.com/jeksterslab/betaMC/actions/workflows/lint.yml/badge.svg)](https://github.com/jeksterslab/betaMC/actions/workflows/lint.yml)
[![Package Website (GitHub
Pages)](https://github.com/jeksterslab/betaMC/actions/workflows/pkgdown-gh-pages.yml/badge.svg)](https://github.com/jeksterslab/betaMC/actions/workflows/pkgdown-gh-pages.yml)
[![Compile
LaTeX](https://github.com/jeksterslab/betaMC/actions/workflows/latex.yml/badge.svg)](https://github.com/jeksterslab/betaMC/actions/workflows/latex.yml)
[![Shell
Check](https://github.com/jeksterslab/betaMC/actions/workflows/shellcheck.yml/badge.svg)](https://github.com/jeksterslab/betaMC/actions/workflows/shellcheck.yml)
[![pages-build-deployment](https://github.com/jeksterslab/betaMC/actions/workflows/pages/pages-build-deployment/badge.svg)](https://github.com/jeksterslab/betaMC/actions/workflows/pages/pages-build-deployment)
[![codecov](https://codecov.io/gh/jeksterslab/betaMC/branch/main/graph/badge.svg?token=KVLUET3DJ6)](https://codecov.io/gh/jeksterslab/betaMC)

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
[`BetaMC`](https://jeksterslab.github.io/betaMC/reference/BetaMC.md)`(``mvn``, alpha ``=`` ``0.05``)`\
`#> Call:`\
`#> BetaMC(object = mvn, alpha = 0.05)`\
`#> `\
`#> Standardized regression slopes`\
`#> type = "mvn"`\
`#>            est     se     R   2.5%  97.5%`\
`#> NARTIC  0.4951 0.0758 20000 0.3404 0.6341`\
`#> PCTGRT  0.3915 0.0766 20000 0.2366 0.5365`\
`#> PCTSUPP 0.2632 0.0749 20000 0.1188 0.4103`

#### Asymptotic distribution-free Approach

\
[`BetaMC`](https://jeksterslab.github.io/betaMC/reference/BetaMC.md)`(``adf``, alpha ``=`` ``0.05``)`\
`#> Call:`\
`#> BetaMC(object = adf, alpha = 0.05)`\
`#> `\
`#> Standardized regression slopes`\
`#> type = "adf"`\
`#>            est     se     R   2.5%  97.5%`\
`#> NARTIC  0.4951 0.0676 20000 0.3501 0.6157`\
`#> PCTGRT  0.3915 0.0707 20000 0.2438 0.5217`\
`#> PCTSUPP 0.2632 0.0771 20000 0.1033 0.4086`

#### Heteroskedasticity Consistent Approach (HC3)

\
[`BetaMC`](https://jeksterslab.github.io/betaMC/reference/BetaMC.md)`(``hc3``, alpha ``=`` ``0.05``)`\
`#> Call:`\
`#> BetaMC(object = hc3, alpha = 0.05)`\
`#> `\
`#> Standardized regression slopes`\
`#> type = "hc3"`\
`#>            est     se     R   2.5%  97.5%`\
`#> NARTIC  0.4951 0.0796 20000 0.3252 0.6375`\
`#> PCTGRT  0.3915 0.0820 20000 0.2192 0.5387`\
`#> PCTSUPP 0.2632 0.0856 20000 0.0899 0.4277`

### Other Effect Sizes

The `betaMC` package also has functions to generate Monte Carlo
confidence intervals for other effect sizes such as
[`RSqMC()`](https://jeksterslab.github.io/betaMC/reference/RSqMC.md) for
multiple correlation coefficients (R-squared and adjusted R-squared),
[`DeltaRSqMC()`](https://jeksterslab.github.io/betaMC/reference/DeltaRSqMC.md)
for improvement in R-squared,
[`SCorMC()`](https://jeksterslab.github.io/betaMC/reference/SCorMC.md)
for semipartial correlation coefficients,
[`PCorMC()`](https://jeksterslab.github.io/betaMC/reference/PCorMC.md)
for squared partial correlation coefficients, and
[`DiffBetaMC()`](https://jeksterslab.github.io/betaMC/reference/DiffBetaMC.md)
for differences of standardized regression coefficients.

#### Multiple Correlation Coefficients (R-squared and adjusted R-squared)

\
[`RSqMC`](https://jeksterslab.github.io/betaMC/reference/RSqMC.md)`(``hc3``, alpha ``=`` ``0.05``)`\
`#> Call:`\
`#> RSqMC(object = hc3, alpha = 0.05)`\
`#> `\
`#> R-squared and adjusted R-squared`\
`#> type = "hc3"`\
`#>        est     se     R   2.5%  97.5%`\
`#> rsq 0.8045 0.0620 20000 0.6465 0.8883`\
`#> adj 0.7906 0.0665 20000 0.6213 0.8803`

#### Improvement in R-squared

\
[`DeltaRSqMC`](https://jeksterslab.github.io/betaMC/reference/DeltaRSqMC.md)`(``hc3``, alpha ``=`` ``0.05``)`\
`#> Call:`\
`#> DeltaRSqMC(object = hc3, alpha = 0.05)`\
`#> `\
`#> Improvement in R-squared`\
`#> type = "hc3"`\
`#>            est     se     R   2.5%  97.5%`\
`#> NARTIC  0.1859 0.0694 20000 0.0499 0.3236`\
`#> PCTGRT  0.1177 0.0543 20000 0.0251 0.2349`\
`#> PCTSUPP 0.0569 0.0378 20000 0.0061 0.1508`

#### Semipartial Correlation Coefficients

\
[`SCorMC`](https://jeksterslab.github.io/betaMC/reference/SCorMC.md)`(``hc3``, alpha ``=`` ``0.05``)`\
`#> Call:`\
`#> SCorMC(object = hc3, alpha = 0.05)`\
`#> `\
`#> Semipartial correlations`\
`#> type = "hc3"`\
`#>            est     se     R   2.5%  97.5%`\
`#> NARTIC  0.4312 0.0872 20000 0.2235 0.5689`\
`#> PCTGRT  0.3430 0.0830 20000 0.1584 0.4846`\
`#> PCTSUPP 0.2385 0.0784 20000 0.0784 0.3884`

#### Squared Partial Correlation Coefficients

\
[`PCorMC`](https://jeksterslab.github.io/betaMC/reference/PCorMC.md)`(``hc3``, alpha ``=`` ``0.05``)`\
`#> Call:`\
`#> PCorMC(object = hc3, alpha = 0.05)`\
`#> `\
`#> Squared partial correlations`\
`#> type = "hc3"`\
`#>            est     se     R   2.5%  97.5%`\
`#> NARTIC  0.4874 0.1196 20000 0.1751 0.6492`\
`#> PCTGRT  0.3757 0.1155 20000 0.1045 0.5530`\
`#> PCTSUPP 0.2254 0.1126 20000 0.0254 0.4568`

#### Differences of Standardized Regression Coefficients

\
[`DiffBetaMC`](https://jeksterslab.github.io/betaMC/reference/DiffBetaMC.md)`(``hc3``, alpha ``=`` ``0.05``)`\
`#> Call:`\
`#> DiffBetaMC(object = hc3, alpha = 0.05)`\
`#> `\
`#> Differences of standardized regression slopes`\
`#> type = "hc3"`\
`#>                   est     se     R    2.5%  97.5%`\
`#> NARTIC-PCTGRT  0.1037 0.1417 20000 -0.1736 0.3789`\
`#> NARTIC-PCTSUPP 0.2319 0.1331 20000 -0.0378 0.4826`\
`#> PCTGRT-PCTSUPP 0.1282 0.1372 20000 -0.1464 0.3871`

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
