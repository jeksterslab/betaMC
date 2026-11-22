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
Cheung, 2024,
[doi:10.3758/s13428-023-02114-4](https://doi.org/10.3758/s13428-023-02114-4))
and the sampling covariance matrix of regression coefficients (Dudgeon,
2017,
[doi:10.1007/s11336-017-9563-z](https://doi.org/10.1007/s11336-017-9563-z))
to generate confidence intervals for effect sizes in regression.

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
[`library`](https://rdrr.io/r/base/library.html)`(``package ``=`` ``betaMC``)`

\
`df`` ``<-`` ``betaMC``::`[`nas1982`](https://jeksterslab.github.io/betaMC/reference/nas1982.md)

### Fit the regression model using the `lm()` function.

\
`object`` ``<-`` ``stats``::`[`lm`](https://rdrr.io/r/stats/lm.html)`(``formula ``=`` ``QUALITY`` ``~`` ``NARTIC`` ``+`` ``PCTGRT`` ``+`` ``PCTSUPP``, data ``=`` ``df``)`

### Generate the sampling distribution of the regression parameters using the Monte Carlo method.

\
`mc`` ``<-`` `[`MC`](https://jeksterslab.github.io/betaMC/reference/MC.md)`(``object ``=`` ``object``, type ``=`` ``"mvn"``, R ``=`` ``20000L``, seed ``=`` ``0508``)`

### Estimate the standardized regression slopes and the corresponding confidence intervals.

\
[`BetaMC`](https://jeksterslab.github.io/betaMC/reference/BetaMC.md)`(``object ``=`` ``mc``, alpha ``=`` ``0.05``)`\
`#> Call:`\
`#> BetaMC(object = mc, alpha = 0.05)`\
`#> `\
`#> Standardized regression slopes`\
`#> type = "mvn"`\
`#>            est     se     R   2.5%  97.5%`\
`#> NARTIC  0.4951 0.0755 20000 0.3393 0.6334`\
`#> PCTGRT  0.3915 0.0765 20000 0.2376 0.5374`\
`#> PCTSUPP 0.2632 0.0744 20000 0.1182 0.4087`

## Other Features

The package provides the following functions.

| Function | Purpose |
|----|----|
| [`MC()`](https://jeksterslab.github.io/betaMC/reference/MC.md) | Monte Carlo sampling distribution of the regression parameters (`type = "mvn"`, `"adf"`, or HC0-HC5) |
| [`MCMI()`](https://jeksterslab.github.io/betaMC/reference/MCMI.md) | [`MC()`](https://jeksterslab.github.io/betaMC/reference/MC.md) for multiply imputed data |
| [`BetaMC()`](https://jeksterslab.github.io/betaMC/reference/BetaMC.md) | Standardized regression coefficients |
| [`RSqMC()`](https://jeksterslab.github.io/betaMC/reference/RSqMC.md) | R-squared and adjusted R-squared |
| [`DiffBetaMC()`](https://jeksterslab.github.io/betaMC/reference/DiffBetaMC.md) | Differences of standardized regression coefficients |
| [`SCorMC()`](https://jeksterslab.github.io/betaMC/reference/SCorMC.md) | Semipartial correlations |
| [`SCorSqMC()`](https://jeksterslab.github.io/betaMC/reference/SCorSqMC.md) | Squared semipartial correlations (squared signed interval) |
| [`PCorMC()`](https://jeksterslab.github.io/betaMC/reference/PCorMC.md) | Squared partial correlations |
| [`DeltaRSqMC()`](https://jeksterslab.github.io/betaMC/reference/DeltaRSqMC.md) | Improvement in R-squared |
| [`FSqMC()`](https://jeksterslab.github.io/betaMC/reference/FSqMC.md) | Cohen’s f-squared and local f-squared |

The [`coef()`](https://rdrr.io/r/stats/coef.html),
[`vcov()`](https://rdrr.io/r/stats/vcov.html),
[`confint()`](https://rdrr.io/r/stats/confint.html),
[`summary()`](https://rdrr.io/r/base/summary.html), and
[`print()`](https://rdrr.io/r/base/print.html) methods are available for
the returned objects. The example above uses the normal-theory sampling
covariance matrix; the asymptotically distribution-free (`type = "adf"`)
and heteroskedasticity-consistent (`type = "hc3"` and others) matrices
are chosen with the `type` argument of
[`MC()`](https://jeksterslab.github.io/betaMC/reference/MC.md). Examples
for each function are in the package vignettes.

## Effect Sizes

The definitions, formulas, and notes on the intervals are in the help
pages of the functions and in the [example
vignettes](https://jeksterslab.github.io/betaMC/articles/index.md).

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

R Core Team. (2026). *R: A language and environment for statistical
computing*. R Foundation for Statistical Computing.
<https://www.R-project.org/>
