# betaMC

Ivan Jacob Agaloos Pesigan 2026-06-10

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
regression coefficients are generated using the
[`BetaMC()`](https://github.com/jeksterslab/betaMC/reference/BetaMC.md)
function from the `betaMC` package.

``` r

library(betaMC)
```

``` r

df <- betaMC::nas1982
```

### Regression

Fit the regression model using the
[`lm()`](https://rdrr.io/r/stats/lm.html) function.

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
#> NARTIC  0.4951 0.0760 20000 0.3397 0.6359
#> PCTGRT  0.3915 0.0769 20000 0.2356 0.5390
#> PCTSUPP 0.2632 0.0741 20000 0.1196 0.4086
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
#> NARTIC  0.4951 0.0678 20000 0.3493 0.6151
#> PCTGRT  0.3915 0.0711 20000 0.2431 0.5226
#> PCTSUPP 0.2632 0.0767 20000 0.1055 0.4083
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
#> NARTIC  0.4951 0.0792 20000 0.3242 0.6337
#> PCTGRT  0.3915 0.0818 20000 0.2184 0.5401
#> PCTSUPP 0.2632 0.0855 20000 0.0887 0.4270
```

### Other Effect Sizes

The `betaMC` package also has functions to generate Monte Carlo
confidence intervals for other effect sizes such as
[`RSqMC()`](https://github.com/jeksterslab/betaMC/reference/RSqMC.md)
for multiple correlation coefficients (R-squared and adjusted
R-squared),
[`DeltaRSqMC()`](https://github.com/jeksterslab/betaMC/reference/DeltaRSqMC.md)
for improvement in R-squared,
[`SCorMC()`](https://github.com/jeksterslab/betaMC/reference/SCorMC.md)
for semipartial correlation coefficients,
[`PCorMC()`](https://github.com/jeksterslab/betaMC/reference/PCorMC.md)
for squared partial correlation coefficients, and
[`DiffBetaMC()`](https://github.com/jeksterslab/betaMC/reference/DiffBetaMC.md)
for differences of standardized regression coefficients.

#### Multiple Correlation Coefficients (R-squared and adjusted R-squared)

``` r

RSqMC(hc3, alpha = 0.05)
#> Call:
#> RSqMC(object = hc3, alpha = 0.05)
#> 
#> R-squared and adjusted R-squared
#> type = "hc3"
#>        est     se     R   2.5%  97.5%
#> rsq 0.8045 0.0617 20000 0.6470 0.8866
#> adj 0.7906 0.0661 20000 0.6218 0.8785
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
#> NARTIC  0.1859 0.0678 20000 0.0507 0.3180
#> PCTGRT  0.1177 0.0544 20000 0.0268 0.2374
#> PCTSUPP 0.0569 0.0374 20000 0.0062 0.1489
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
#> NARTIC  0.4312 0.0854 20000 0.2251 0.5639
#> PCTGRT  0.3430 0.0824 20000 0.1636 0.4872
#> PCTSUPP 0.2385 0.0781 20000 0.0786 0.3859
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
#> NARTIC  0.4874 0.1187 20000 0.1812 0.6502
#> PCTGRT  0.3757 0.1138 20000 0.1103 0.5512
#> PCTSUPP 0.2254 0.1125 20000 0.0256 0.4540
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
#> NARTIC-PCTGRT  0.1037 0.1417 20000 -0.1768 0.3761
#> NARTIC-PCTSUPP 0.2319 0.1322 20000 -0.0368 0.4813
#> PCTGRT-PCTSUPP 0.1282 0.1367 20000 -0.1513 0.3896
```

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
