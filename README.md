betaMC
================
Ivan Jacob Agaloos Pesigan
2024-06-10

<!-- README.md is generated from .setup/readme/README.Rmd. Please edit that file -->
<!-- badges: start -->

[![CRAN
Status](https://www.r-pkg.org/badges/version/betaMC)](https://cran.r-project.org/package=betaMC)
[![R-Universe
Status](https://jeksterslab.r-universe.dev/badges/betaMC)](https://jeksterslab.r-universe.dev)
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
#> NARTIC  0.4951 0.0760 20000 0.3374 0.6355
#> PCTGRT  0.3915 0.0763 20000 0.2390 0.5383
#> PCTSUPP 0.2632 0.0748 20000 0.1177 0.4097
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
#> NARTIC  0.4951 0.0678 20000 0.3522 0.6160
#> PCTGRT  0.3915 0.0710 20000 0.2444 0.5223
#> PCTSUPP 0.2632 0.0770 20000 0.1058 0.4077
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
#> NARTIC  0.4951 0.0794 20000 0.3243 0.6348
#> PCTGRT  0.3915 0.0824 20000 0.2172 0.5394
#> PCTSUPP 0.2632 0.0856 20000 0.0882 0.4259
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
#> rsq 0.8045 0.0613 20000 0.6489 0.8875
#> adj 0.7906 0.0657 20000 0.6239 0.8795
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
#> NARTIC  0.1859 0.0686 20000 0.0510 0.3189
#> PCTGRT  0.1177 0.0544 20000 0.0253 0.2374
#> PCTSUPP 0.0569 0.0376 20000 0.0060 0.1491
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
#> NARTIC  0.4312 0.0864 20000 0.2258 0.5647
#> PCTGRT  0.3430 0.0829 20000 0.1591 0.4873
#> PCTSUPP 0.2385 0.0783 20000 0.0773 0.3862
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
#> NARTIC  0.4874 0.1188 20000 0.1773 0.6477
#> PCTGRT  0.3757 0.1154 20000 0.1065 0.5539
#> PCTSUPP 0.2254 0.1125 20000 0.0260 0.4533
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
#> NARTIC-PCTGRT  0.1037 0.1425 20000 -0.1786 0.3817
#> NARTIC-PCTSUPP 0.2319 0.1326 20000 -0.0398 0.4762
#> PCTGRT-PCTSUPP 0.1282 0.1369 20000 -0.1488 0.3868
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
Methods*. <https://doi.org/10.3758/s13428-023-02114-4>

</div>

</div>
