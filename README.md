betaMC
================
Ivan Jacob Agaloos Pesigan
2026-11-12

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
Cheung, 2024: <http://doi.org/10.3758/s13428-023-02114-4>) and the
sampling covariance matrix of regression coefficients (Dudgeon, 2017:
<http://doi.org/10.1007/s11336-017-9563-z>) to generate confidence
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
library(betaMC)
```

``` r
df <- betaMC::nas1982
```

### Regression

Fit the regression model using the `lm()` function.

``` r
object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = df)
```

### Monte Carlo Sampling Distribution of Parameters

#### Normal-Theory Approach

``` r
mvn <- MC(object, type = "mvn")
```

#### Asymptotic distribution-free Approach

``` r
adf <- MC(object, type = "adf")
```

#### Heteroskedasticity Consistent Approach (HC3)

``` r
hc3 <- MC(object, type = "hc3")
```

### Standardized Regression Slopes

#### Normal-Theory Approach

``` r
BetaMC(mvn, alpha = 0.05)
#> Call:
#> BetaMC(object = mvn, alpha = 0.05)
#> 
#> Standardized regression slopes
#> type = "mvn"
#>            est     se     R   2.5%  97.5%
#> NARTIC  0.4951 0.0756 20000 0.3369 0.6346
#> PCTGRT  0.3915 0.0769 20000 0.2375 0.5375
#> PCTSUPP 0.2632 0.0741 20000 0.1183 0.4095
```

#### Asymptotic distribution-free Approach

``` r
BetaMC(adf, alpha = 0.05)
#> Call:
#> BetaMC(object = adf, alpha = 0.05)
#> 
#> Standardized regression slopes
#> type = "adf"
#>            est     se     R   2.5%  97.5%
#> NARTIC  0.4951 0.0680 20000 0.3513 0.6170
#> PCTGRT  0.3915 0.0712 20000 0.2422 0.5206
#> PCTSUPP 0.2632 0.0770 20000 0.1046 0.4091
```

#### Heteroskedasticity Consistent Approach (HC3)

``` r
BetaMC(hc3, alpha = 0.05)
#> Call:
#> BetaMC(object = hc3, alpha = 0.05)
#> 
#> Standardized regression slopes
#> type = "hc3"
#>            est     se     R   2.5%  97.5%
#> NARTIC  0.4951 0.0800 20000 0.3249 0.6346
#> PCTGRT  0.3915 0.0826 20000 0.2161 0.5426
#> PCTSUPP 0.2632 0.0849 20000 0.0911 0.4228
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
RSqMC(hc3, alpha = 0.05)
#> Call:
#> RSqMC(object = hc3, alpha = 0.05)
#> 
#> R-squared and adjusted R-squared
#> type = "hc3"
#>        est     se     R   2.5%  97.5%
#> rsq 0.8045 0.0616 20000 0.6450 0.8864
#> adj 0.7906 0.0660 20000 0.6196 0.8783
```

#### Improvement in R-squared

``` r
DeltaRSqMC(hc3, alpha = 0.05)
#> Call:
#> DeltaRSqMC(object = hc3, alpha = 0.05)
#> 
#> Improvement in R-squared
#> type = "hc3"
#>            est     se     R   2.5%  97.5%
#> NARTIC  0.1859 0.0690 20000 0.0491 0.3225
#> PCTGRT  0.1177 0.0551 20000 0.0249 0.2392
#> PCTSUPP 0.0569 0.0371 20000 0.0063 0.1492
```

#### Semipartial Correlation Coefficients

``` r
SCorMC(hc3, alpha = 0.05)
#> Call:
#> SCorMC(object = hc3, alpha = 0.05)
#> 
#> Semipartial correlations
#> type = "hc3"
#>            est     se     R   2.5%  97.5%
#> NARTIC  0.4312 0.0868 20000 0.2216 0.5679
#> PCTGRT  0.3430 0.0838 20000 0.1577 0.4891
#> PCTSUPP 0.2385 0.0776 20000 0.0793 0.3862
```

#### Squared Partial Correlation Coefficients

``` r
PCorMC(hc3, alpha = 0.05)
#> Call:
#> PCorMC(object = hc3, alpha = 0.05)
#> 
#> Squared partial correlations
#> type = "hc3"
#>            est     se     R   2.5%  97.5%
#> NARTIC  0.4874 0.1192 20000 0.1803 0.6536
#> PCTGRT  0.3757 0.1154 20000 0.1054 0.5539
#> PCTSUPP 0.2254 0.1115 20000 0.0265 0.4513
```

#### Differences of Standardized Regression Coefficients

``` r
DiffBetaMC(hc3, alpha = 0.05)
#> Call:
#> DiffBetaMC(object = hc3, alpha = 0.05)
#> 
#> Differences of standardized regression slopes
#> type = "hc3"
#>                   est     se     R    2.5%  97.5%
#> NARTIC-PCTGRT  0.1037 0.1433 20000 -0.1816 0.3788
#> NARTIC-PCTSUPP 0.2319 0.1330 20000 -0.0372 0.4824
#> PCTGRT-PCTSUPP 0.1282 0.1362 20000 -0.1458 0.3901
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
