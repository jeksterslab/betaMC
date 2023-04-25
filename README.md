betaMC
================
Ivan Jacob Agaloos Pesigan
2023-04-25

<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![CRAN
Status](https://www.r-pkg.org/badges/version/betaMC)](https://cran.r-project.org/package=betaMC)
[![R-Universe
Status](https://jeksterslab.r-universe.dev/badges/betaMC)](https://jeksterslab.r-universe.dev)
[![R-CMD-check](https://github.com/jeksterslab/betaMC/workflows/R-CMD-check/badge.svg)](https://github.com/jeksterslab/betaMC/actions)
[![test-coverage](https://github.com/jeksterslab/betaMC/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/jeksterslab/betaMC/actions/workflows/test-coverage.yaml)
[![lint](https://github.com/jeksterslab/betaMC/actions/workflows/lint.yaml/badge.svg)](https://github.com/jeksterslab/betaMC/actions/workflows/lint.yaml)
[![codecov](https://codecov.io/gh/jeksterslab/betaMC/branch/main/graph/badge.svg?token=KVLUET3DJ6)](https://codecov.io/gh/jeksterslab/betaMC)
<!-- badges: end -->

## Description

Generates Monte Carlo confidence intervals for standardized regression
coefficients (beta) and other effect sizes, including multiple
correlation, semipartial correlations, improvement in R-squared, squared
partial correlations, and differences in standardized regression
coefficients, for models fitted by `lm()`. `betaMC` combines ideas from
Monte Carlo confidence intervals for the indirect effect (Preacher and
Selig, 2012: <http://doi.org/10.1080/19312458.2012.679848>) and the
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
BetaMC(mvn)
#> Call:
#> BetaMC(object = mvn)
#> 
#> Standardized regression slopes
#> type = "mvn"
#>            est     se     R  0.05%   0.5%   2.5%  97.5%  99.5% 99.95%
#> NARTIC  0.4951 0.0755 20000 0.2479 0.2947 0.3396 0.6338 0.6796 0.7200
#> PCTGRT  0.3915 0.0771 20000 0.1348 0.1894 0.2376 0.5405 0.5853 0.6374
#> PCTSUPP 0.2632 0.0747 20000 0.0296 0.0777 0.1193 0.4110 0.4619 0.5146
```

#### Asymptotic distribution-free Approach

``` r
BetaMC(adf)
#> Call:
#> BetaMC(object = adf)
#> 
#> Standardized regression slopes
#> type = "adf"
#>            est     se     R  0.05%   0.5%   2.5%  97.5%  99.5% 99.95%
#> NARTIC  0.4951 0.0679 20000 0.2652 0.3106 0.3526 0.6163 0.6562 0.7018
#> PCTGRT  0.3915 0.0715 20000 0.1399 0.1942 0.2412 0.5196 0.5654 0.6200
#> PCTSUPP 0.2632 0.0775 20000 0.0031 0.0574 0.1068 0.4107 0.4588 0.5157
```

#### Heteroskedasticity Consistent Approach (HC3)

``` r
BetaMC(hc3)
#> Call:
#> BetaMC(object = hc3)
#> 
#> Standardized regression slopes
#> type = "hc3"
#>            est     se     R   0.05%   0.5%   2.5%  97.5%  99.5% 99.95%
#> NARTIC  0.4951 0.0796 20000  0.2102 0.2697 0.3216 0.6345 0.6777 0.7198
#> PCTGRT  0.3915 0.0824 20000  0.0921 0.1598 0.2169 0.5388 0.5861 0.6352
#> PCTSUPP 0.2632 0.0858 20000 -0.0467 0.0300 0.0889 0.4246 0.4807 0.5429
```

### Other Effect Sizes

The `betaMC` package also has functions to generate Monte Carlo
confidence intervals for other effect sizes such as `RSqMC()` for
multiple correlation coefficients (R-squared and adjusted R-squared),
`DeltaRSqMC()` for improvement in R-squared, `SCorMC()` for semipartial
correlation coefficients, `PCorMC()` for squared partial correlation
coefficients, and `DiffBetaMC()` for differences of standardized
regression coefficients.

### References

Dudgeon, P. (2017). Some improvements in confidence intervals for
standardized regression coefficients. *Psychometrika*, *82*(4), 928–951.
<https://doi.org/10.1007/s11336-017-9563-z>

National Research Council. (1982). *An assessment of research-doctorate
programs in the United States: Social and behavioral sciences*.
<https://doi.org/10.17226/9781>. Reproduced with permission from the
National Academy of Sciences, Courtesy of the National Academies Press,
Washington, D.C.

Pesigan, I. J. A. (2022). Confidence intervals for standardized
coefficients: Applied to regression coefficients in primary studies and
indirect effects in meta-analytic structural equation modeling
\[Unpublished doctoral dissertation\]. University of Macau.

Preacher, K. J., & Selig, J. P. (2012). Advantages of Monte Carlo
confidence intervals for indirect effects. *Communication Methods and
Measures*, *6*(2), 77-98. <https://doi.org/10.1080/19312458.2012.679848>

## Documentation

See [GitHub Pages](https://jeksterslab.github.io/betaMC/index.html) for
package documentation.
