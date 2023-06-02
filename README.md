betaMC
================
Ivan Jacob Agaloos Pesigan
2023-06-02

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
BetaMC(mvn, alpha = 0.05)
#> Call:
#> BetaMC(object = mvn, alpha = 0.05)
#> 
#> Standardized regression slopes
#> type = ""
#>            est     se     R   2.5%  97.5%
#> NARTIC  0.4951 0.0753 20000 0.3399 0.6344
#> PCTGRT  0.3915 0.0766 20000 0.2371 0.5354
#> PCTSUPP 0.2632 0.0746 20000 0.1167 0.4105
```

#### Asymptotic distribution-free Approach

``` r
BetaMC(adf, alpha = 0.05)
#> Call:
#> BetaMC(object = adf, alpha = 0.05)
#> 
#> Standardized regression slopes
#> type = ""
#>            est     se     R   2.5%  97.5%
#> NARTIC  0.4951 0.0676 20000 0.3536 0.6168
#> PCTGRT  0.3915 0.0711 20000 0.2419 0.5211
#> PCTSUPP 0.2632 0.0764 20000 0.1083 0.4087
```

#### Heteroskedasticity Consistent Approach (HC3)

``` r
BetaMC(hc3, alpha = 0.05)
#> Call:
#> BetaMC(object = hc3, alpha = 0.05)
#> 
#> Standardized regression slopes
#> type = ""
#>            est     se     R   2.5%  97.5%
#> NARTIC  0.4951 0.0798 20000 0.3253 0.6348
#> PCTGRT  0.3915 0.0825 20000 0.2188 0.5417
#> PCTSUPP 0.2632 0.0852 20000 0.0916 0.4236
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
#> type = ""
#>        est     se     R   2.5%  97.5%
#> rsq 0.8045 0.0617 20000 0.6468 0.8881
#> adj 0.7906 0.0662 20000 0.6216 0.8801
```

#### Improvement in R-squared

``` r
DeltaRSqMC(hc3, alpha = 0.05)
#> Call:
#> DeltaRSqMC(object = hc3, alpha = 0.05)
#> 
#> Improvement in R-squared
#> type = ""
#>            est     se     R   2.5%  97.5%
#> NARTIC  0.1859 0.0695 20000 0.0489 0.3230
#> PCTGRT  0.1177 0.0548 20000 0.0253 0.2383
#> PCTSUPP 0.0569 0.0373 20000 0.0064 0.1491
```

#### Semipartial Correlation Coefficients

``` r
SCorMC(hc3, alpha = 0.05)
#> Call:
#> SCorMC(object = hc3, alpha = 0.05)
#> 
#> Semipartial correlations
#> type = ""
#>            est     se     R   2.5%  97.5%
#> NARTIC  0.4312 0.0874 20000 0.2210 0.5683
#> PCTGRT  0.3430 0.0833 20000 0.1592 0.4881
#> PCTSUPP 0.2385 0.0780 20000 0.0798 0.3861
```

#### Squared Partial Correlation Coefficients

``` r
PCorMC(hc3, alpha = 0.05)
#> Call:
#> PCorMC(object = hc3, alpha = 0.05)
#> 
#> Squared partial correlations
#> type = ""
#>            est     se     R   2.5%  97.5%
#> NARTIC  0.4874 0.1195 20000 0.1794 0.6507
#> PCTGRT  0.3757 0.1152 20000 0.1063 0.5549
#> PCTSUPP 0.2254 0.1128 20000 0.0275 0.4543
```

#### Differences of Standardized Regression Coefficients

``` r
DiffBetaMC(hc3, alpha = 0.05)
#> Call:
#> DiffBetaMC(object = hc3, alpha = 0.05)
#> 
#> Differences of standardized regression slopes
#> type = ""
#>                   est     se     R    2.5%  97.5%
#> NARTIC-PCTGRT  0.1037 0.1428 20000 -0.1788 0.3773
#> NARTIC-PCTSUPP 0.2319 0.1321 20000 -0.0381 0.4817
#> PCTGRT-PCTSUPP 0.1282 0.1374 20000 -0.1463 0.3883
```

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
