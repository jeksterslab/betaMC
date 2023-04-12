betaMC
================
Ivan Jacob Agaloos Pesigan
2023-04-12

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
coefficients and other effect sizes for models fitted by `lm()`.
`betaMC` combines ideas from Monte Carlo confidence intervals for the
indirect effect (Preacher and Selig, 2012:
<http://doi.org/10.1080/19312458.2012.679848>) and the sampling
covariance matrix of regression coefficients (Dudgeon, 2017:
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
#> NARTIC  0.4951 0.0751 20000 0.2417 0.2953 0.3387 0.6324 0.6749 0.7137
#> PCTGRT  0.3915 0.0762 20000 0.1556 0.1952 0.2392 0.5381 0.5815 0.6346
#> PCTSUPP 0.2632 0.0749 20000 0.0275 0.0742 0.1183 0.4104 0.4577 0.5302
```

#### Asymptotic distribution-free Approach

``` r
BetaMC(adf)
#> Call:
#> BetaMC(object = adf)
#> 
#> Standardized regression slopes
#> type = "adf"
#>            est     se     R   0.05%   0.5%   2.5%  97.5%  99.5% 99.95%
#> NARTIC  0.4951 0.0678 20000  0.2490 0.3101 0.3515 0.6185 0.6570 0.6967
#> PCTGRT  0.3915 0.0712 20000  0.1507 0.2002 0.2418 0.5211 0.5619 0.6095
#> PCTSUPP 0.2632 0.0763 20000 -0.0086 0.0561 0.1074 0.4071 0.4512 0.5025
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
#> NARTIC  0.4951 0.0794 20000  0.1878 0.2725 0.3253 0.6348 0.6766 0.7311
#> PCTGRT  0.3915 0.0823 20000  0.1005 0.1612 0.2198 0.5429 0.5877 0.6311
#> PCTSUPP 0.2632 0.0856 20000 -0.0261 0.0277 0.0900 0.4264 0.4787 0.5337
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
