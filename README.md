betaMC
================
Ivan Jacob Agaloos Pesigan
2024-12-29

<!-- README.md is generated from .setup/readme/README.Rmd. Please edit that file -->

<!-- badges: start -->

[![CRAN
Status](https://www.r-pkg.org/badges/version/betaMC)](https://cran.r-project.org/package=betaMC)
[![R-Universe
Status](https://jeksterslab.r-universe.dev/badges/betaMC)](https://jeksterslab.r-universe.dev/betaMC)
[![DOI](https://zenodo.org/badge/DOI/10.3758/s13428-023-02114-4.svg)](https://doi.org/10.3758/s13428-023-02114-4)
[![Make
Project](https://github.com/jeksterslab/betaMC/actions/workflows/make.yml/badge.svg)](https://github.com/jeksterslab/betaMC/actions/workflows/make.yml)
[![R-CMD-check](https://github.com/jeksterslab/betaMC/actions/workflows/check-full.yml/badge.svg)](https://github.com/jeksterslab/betaMC/actions/workflows/check-full.yml)
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
<!-- badges: end -->

## Description

Generates Monte Carlo confidence intervals for standardized regression
coefficients (beta) and other effect sizes, including multiple
correlation, semipartial correlations, improvement in R-squared, squared
partial correlations, and differences in standardized regression
coefficients, for models fitted by `lm()`. `betaMC` combines ideas from
Monte Carlo confidence intervals for the indirect effect (Pesigan and
Cheung, 2023: <http://doi.org/10.3758/s13428-023-02114-4>) and the
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
if (!require("remotes")) install.packages("remotes")
remotes::install_github("jeksterslab/betaMC")
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
#> NARTIC  0.4951 0.0755 20000 0.3402 0.6372
#> PCTGRT  0.3915 0.0771 20000 0.2366 0.5372
#> PCTSUPP 0.2632 0.0748 20000 0.1180 0.4104
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
#> NARTIC  0.4951 0.0677 20000 0.3527 0.6162
#> PCTGRT  0.3915 0.0717 20000 0.2395 0.5200
#> PCTSUPP 0.2632 0.0768 20000 0.1080 0.4091
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
#> NARTIC  0.4951 0.0794 20000 0.3243 0.6341
#> PCTGRT  0.3915 0.0818 20000 0.2200 0.5410
#> PCTSUPP 0.2632 0.0855 20000 0.0878 0.4256
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
#> rsq 0.8045 0.0623 20000 0.6433 0.8875
#> adj 0.7906 0.0667 20000 0.6178 0.8794
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
#> NARTIC  0.1859 0.0684 20000 0.0511 0.3200
#> PCTGRT  0.1177 0.0547 20000 0.0261 0.2386
#> PCTSUPP 0.0569 0.0374 20000 0.0059 0.1502
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
#> NARTIC  0.4312 0.0859 20000 0.2261 0.5657
#> PCTGRT  0.3430 0.0828 20000 0.1615 0.4884
#> PCTSUPP 0.2385 0.0781 20000 0.0766 0.3875
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
#> NARTIC  0.4874 0.1181 20000 0.1822 0.6477
#> PCTGRT  0.3757 0.1150 20000 0.1074 0.5544
#> PCTSUPP 0.2254 0.1131 20000 0.0243 0.4540
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
#> NARTIC-PCTGRT  0.1037 0.1417 20000 -0.1820 0.3777
#> NARTIC-PCTSUPP 0.2319 0.1330 20000 -0.0407 0.4819
#> PCTGRT-PCTSUPP 0.1282 0.1361 20000 -0.1438 0.3880
```

## Documentation

See [GitHub Pages](https://jeksterslab.github.io/betaMC/index.html) for
package documentation.

## Citation

To cite `betaMC` in publications, please cite Pesigan & Cheung (2023).

## References

<div id="refs" class="references csl-bib-body hanging-indent"
entry-spacing="0" line-spacing="2">

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

<div id="ref-Pesigan-Cheung-2023" class="csl-entry">

Pesigan, I. J. A., & Cheung, S. F. (2023). Monte Carlo confidence
intervals for the indirect effect with missing data. *Behavior Research
Methods*, *56*(3), 1678–1696.
<https://doi.org/10.3758/s13428-023-02114-4>

</div>

</div>
