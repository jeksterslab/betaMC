betaMC
================
Ivan Jacob Agaloos Pesigan
2026-11-21

<!-- README.md is generated from .setup/readme/README.Rmd. Please edit that file -->

<!-- badges: start -->
[![DOI](https://zenodo.org/badge/DOI/10.3758/s13428-023-02114-4.svg)](https://doi.org/10.3758/s13428-023-02114-4)
[![CRAN Status](https://www.r-pkg.org/badges/version/betaMC)](https://cran.r-project.org/package=betaMC)
[![R-Universe Status](https://jeksterslab.r-universe.dev/badges/betaMC)](https://jeksterslab.r-universe.dev/betaMC)
[![pages-build-deployment](https://github.com/jeksterslab/betaMC/actions/workflows/pages/pages-build-deployment/badge.svg)](https://github.com/jeksterslab/betaMC/actions/workflows/pages/pages-build-deployment)
[![codecov](https://codecov.io/gh/jeksterslab/betaMC/branch/main/graph/badge.svg?token=KVLUET3DJ6)](https://codecov.io/gh/jeksterslab/betaMC)
[![Make Project (make all)](https://github.com/jeksterslab/betaMC/actions/workflows/make-all.yml/badge.svg)](https://github.com/jeksterslab/betaMC/actions/workflows/make-all.yml)
[![Make Project](https://github.com/jeksterslab/betaMC/actions/workflows/make.yml/badge.svg)](https://github.com/jeksterslab/betaMC/actions/workflows/make.yml)
[![R-CMD-check-full](https://github.com/jeksterslab/betaMC/actions/workflows/check-full.yml/badge.svg)](https://github.com/jeksterslab/betaMC/actions/workflows/check-full.yml)
[![R-CMD-check-oldrel](https://github.com/jeksterslab/betaMC/actions/workflows/check-oldrel.yml/badge.svg)](https://github.com/jeksterslab/betaMC/actions/workflows/check-oldrel.yml)
[![R-CMD-check-standard](https://github.com/jeksterslab/betaMC/actions/workflows/check-standard.yml/badge.svg)](https://github.com/jeksterslab/betaMC/actions/workflows/check-standard.yml)
[![Compile LaTeX](https://github.com/jeksterslab/betaMC/actions/workflows/latex.yml/badge.svg)](https://github.com/jeksterslab/betaMC/actions/workflows/latex.yml)
[![Lint R Package](https://github.com/jeksterslab/betaMC/actions/workflows/lint.yml/badge.svg)](https://github.com/jeksterslab/betaMC/actions/workflows/lint.yml)
[![Non-ASCII Check](https://github.com/jeksterslab/betaMC/actions/workflows/non-ascii-check.yml/badge.svg)](https://github.com/jeksterslab/betaMC/actions/workflows/non-ascii-check.yml)
[![Package Website (GitHub Pages)](https://github.com/jeksterslab/betaMC/actions/workflows/pkgdown-gh-pages.yml/badge.svg)](https://github.com/jeksterslab/betaMC/actions/workflows/pkgdown-gh-pages.yml)
[![rhub 1 (essential)](https://github.com/jeksterslab/betaMC/actions/workflows/rhub-1-essential.yml/badge.svg)](https://github.com/jeksterslab/betaMC/actions/workflows/rhub-1-essential.yml)
[![rhub 2 (memory)](https://github.com/jeksterslab/betaMC/actions/workflows/rhub-2-memory.yml/badge.svg)](https://github.com/jeksterslab/betaMC/actions/workflows/rhub-2-memory.yml)
[![rhub 3 (numerics), review only](https://github.com/jeksterslab/betaMC/actions/workflows/rhub-3-numerics.yml/badge.svg)](https://github.com/jeksterslab/betaMC/actions/workflows/rhub-3-numerics.yml)
[![rhub 4 (compilers), review only](https://github.com/jeksterslab/betaMC/actions/workflows/rhub-4-compilers.yml/badge.svg)](https://github.com/jeksterslab/betaMC/actions/workflows/rhub-4-compilers.yml)
[![Shell Check](https://github.com/jeksterslab/betaMC/actions/workflows/shellcheck.yml/badge.svg)](https://github.com/jeksterslab/betaMC/actions/workflows/shellcheck.yml)
[![Source Code](https://github.com/jeksterslab/betaMC/actions/workflows/source.yml/badge.svg)](https://github.com/jeksterslab/betaMC/actions/workflows/source.yml)
[![R Package Test Coverage](https://github.com/jeksterslab/betaMC/actions/workflows/test-coverage.yml/badge.svg)](https://github.com/jeksterslab/betaMC/actions/workflows/test-coverage.yml)
<!-- badges: end -->

## Description

Generates Monte Carlo confidence intervals for standardized regression
coefficients (beta) and other effect sizes, including multiple
correlation, semipartial correlations, improvement in R-squared, squared
partial correlations, and differences in standardized regression
coefficients, for models fitted by `lm()`. `betaMC` combines ideas from
Monte Carlo confidence intervals for the indirect effect (Pesigan and
Cheung, 2024: <https://doi.org/10.3758/s13428-023-02114-4>) and the
sampling covariance matrix of regression coefficients (Dudgeon, 2017:
<https://doi.org/10.1007/s11336-017-9563-z>) to generate confidence
intervals effect sizes in regression.

## Installation

You can install the CRAN release of `betaMC` with:

``` r
install.packages("betaMC")
```

You can install the development version of `betaMC` from
[GitHub](https://github.com/jeksterslab/betaMC) with:

``` r
if (!require("pak")) install.packages("pak")
pak::pkg_install("jeksterslab/betaMC")
```

## Example

In this example, a multiple regression model is fitted using program
quality ratings (`QUALITY`) as the regressand/outcome variable and
number of published articles attributed to the program faculty members
(`NARTIC`), percent of faculty members holding research grants
(`PCTGRT`), and percentage of program graduates who received support
(`PCTSUPP`) as regressor/predictor variables using a data set from 1982
ratings of 46 doctoral programs in psychology in the USA (National
Research Council, 1982). Confidence intervals for the standardized
regression coefficients are generated using the `BetaMC()` function from
the `betaMC` package.

``` r
library(package = betaMC)
```

``` r
df <- betaMC::nas1982
```

### Regression

Fit the regression model using the `lm()` function.

``` r
object <- stats::lm(formula = QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = df)
```

### Monte Carlo Sampling Distribution of Parameters

#### Normal-Theory Approach

``` r
mvn <- MC(object = object, type = "mvn")
```

#### Asymptotic distribution-free Approach

``` r
adf <- MC(object = object, type = "adf")
```

#### Heteroskedasticity Consistent Approach (HC3)

``` r
hc3 <- MC(object = object, type = "hc3")
```

### Standardized Regression Slopes

#### Normal-Theory Approach

``` r
BetaMC(object = mvn, alpha = 0.05)
#> Call:
#> BetaMC(object = mvn, alpha = 0.05)
#> 
#> Standardized regression slopes
#> type = "mvn"
#>            est     se     R   2.5%  97.5%
#> NARTIC  0.4951 0.0757 20000 0.3402 0.6360
#> PCTGRT  0.3915 0.0771 20000 0.2370 0.5385
#> PCTSUPP 0.2632 0.0742 20000 0.1177 0.4094
```

#### Asymptotic distribution-free Approach

``` r
BetaMC(object = adf, alpha = 0.05)
#> Call:
#> BetaMC(object = adf, alpha = 0.05)
#> 
#> Standardized regression slopes
#> type = "adf"
#>            est     se     R   2.5%  97.5%
#> NARTIC  0.4951 0.0673 20000 0.3537 0.6167
#> PCTGRT  0.3915 0.0708 20000 0.2432 0.5201
#> PCTSUPP 0.2632 0.0766 20000 0.1068 0.4074
```

#### Heteroskedasticity Consistent Approach (HC3)

``` r
BetaMC(object = hc3, alpha = 0.05)
#> Call:
#> BetaMC(object = hc3, alpha = 0.05)
#> 
#> Standardized regression slopes
#> type = "hc3"
#>            est     se     R   2.5%  97.5%
#> NARTIC  0.4951 0.0797 20000 0.3222 0.6340
#> PCTGRT  0.3915 0.0824 20000 0.2182 0.5418
#> PCTSUPP 0.2632 0.0854 20000 0.0903 0.4238
```

### Other Effect Sizes

The `betaMC` package also has functions to generate Monte Carlo
confidence intervals for other effect sizes such as `RSqMC()` for
multiple correlation coefficients (R-squared and adjusted R-squared),
`DeltaRSqMC()` for improvement in R-squared, `SCorMC()` for semipartial
correlation coefficients, `PCorMC()` for squared partial correlation
coefficients, and `DiffBetaMC()` for differences of standardized
regression coefficients.

#### Multiple Correlation Coefficients (R-squared and adjusted R-squared)

``` r
RSqMC(object = hc3, alpha = 0.05)
#> Call:
#> RSqMC(object = hc3, alpha = 0.05)
#> 
#> R-squared and adjusted R-squared
#> type = "hc3"
#>        est     se     R   2.5%  97.5%
#> rsq 0.8045 0.0627 20000 0.6451 0.8868
#> adj 0.7906 0.0672 20000 0.6197 0.8787
```

#### Improvement in R-squared

``` r
DeltaRSqMC(object = hc3, alpha = 0.05)
#> Call:
#> DeltaRSqMC(object = hc3, alpha = 0.05)
#> 
#> Improvement in R-squared
#> type = "hc3"
#>            est     se     R   2.5%  97.5%
#> NARTIC  0.1859 0.0685 20000 0.0489 0.3193
#> PCTGRT  0.1177 0.0547 20000 0.0256 0.2372
#> PCTSUPP 0.0569 0.0376 20000 0.0063 0.1488
```

#### Semipartial Correlation Coefficients

``` r
SCorMC(object = hc3, alpha = 0.05)
#> Call:
#> SCorMC(object = hc3, alpha = 0.05)
#> 
#> Semipartial correlations
#> type = "hc3"
#>            est     se     R   2.5%  97.5%
#> NARTIC  0.4312 0.0864 20000 0.2212 0.5650
#> PCTGRT  0.3430 0.0831 20000 0.1600 0.4870
#> PCTSUPP 0.2385 0.0782 20000 0.0790 0.3857
```

#### Squared Partial Correlation Coefficients

``` r
PCorMC(object = hc3, alpha = 0.05)
#> Call:
#> PCorMC(object = hc3, alpha = 0.05)
#> 
#> Squared partial correlations
#> type = "hc3"
#>            est     se     R   2.5%  97.5%
#> NARTIC  0.4874 0.1194 20000 0.1792 0.6521
#> PCTGRT  0.3757 0.1150 20000 0.1063 0.5546
#> PCTSUPP 0.2254 0.1128 20000 0.0261 0.4546
```

#### Differences of Standardized Regression Coefficients

``` r
DiffBetaMC(object = hc3, alpha = 0.05)
#> Call:
#> DiffBetaMC(object = hc3, alpha = 0.05)
#> 
#> Differences of standardized regression slopes
#> type = "hc3"
#>                   est     se     R    2.5%  97.5%
#> NARTIC-PCTGRT  0.1037 0.1427 20000 -0.1794 0.3820
#> NARTIC-PCTSUPP 0.2319 0.1326 20000 -0.0367 0.4813
#> PCTGRT-PCTSUPP 0.1282 0.1371 20000 -0.1476 0.3899
```

## Documentation

See [GitHub Pages](https://jeksterslab.github.io/betaMC/index.html) for
package documentation.

## Citation

To cite `betaMC` in publications, please cite Pesigan & Cheung (2024).

## References

<div id="refs" class="references csl-bib-body hanging-indent"
data-entry-spacing="0" data-line-spacing="2">

<div id="ref-Dudgeon-2017" class="csl-entry">

Dudgeon, P. (2017). Some improvements in confidence intervals for
standardized regression coefficients. *Psychometrika*, *82*(4), 928–951.
<https://doi.org/10.1007/s11336-017-9563-z>

</div>

<div id="ref-NationalResearchCouncil-1982" class="csl-entry">

National Research Council. (1982). *An assessment of research-doctorate
programs in the United States: Social and behavioral sciences*. National
Academies Press. <https://doi.org/10.17226/9781>

</div>

<div id="ref-Pesigan-Cheung-2024" class="csl-entry">

Pesigan, I. J. A., & Cheung, S. F. (2024). Monte Carlo confidence
intervals for the indirect effect with missing data. *Behavior Research
Methods*, *56*(3), 1678–1696.
<https://doi.org/10.3758/s13428-023-02114-4>

</div>

</div>
