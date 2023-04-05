betaMC
================
Ivan Jacob Agaloos Pesigan
2023-04-05

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
install.packages("remotes")
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
#> NARTIC  0.4951 0.0756 20000 0.2424 0.2930 0.3389 0.6350 0.6784 0.7230
#> PCTGRT  0.3915 0.0769 20000 0.1460 0.1890 0.2371 0.5384 0.5847 0.6440
#> PCTSUPP 0.2632 0.0738 20000 0.0296 0.0743 0.1191 0.4073 0.4559 0.5185
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
#> NARTIC  0.4951 0.0677 20000  0.2635 0.3094 0.3524 0.6174 0.6559 0.7055
#> PCTGRT  0.3915 0.0712 20000  0.1460 0.1967 0.2429 0.5224 0.5628 0.6032
#> PCTSUPP 0.2632 0.0767 20000 -0.0035 0.0555 0.1074 0.4090 0.4521 0.4970
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
#> NARTIC  0.4951 0.0799 20000  0.2101 0.2660 0.3224 0.6358 0.6832 0.7365
#> PCTGRT  0.3915 0.0825 20000  0.0944 0.1655 0.2165 0.5395 0.5870 0.6543
#> PCTSUPP 0.2632 0.0854 20000 -0.0242 0.0359 0.0910 0.4277 0.4772 0.5524
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
