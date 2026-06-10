# betaMC: Example Using the RSqMC Function

Confidence intervals for multiple correlation coefficients (R-squared
and adjusted R-squared) are generated using the
[`RSqMC()`](https://github.com/jeksterslab/betaMC/reference/RSqMC.md)
function from the `betaMC` package. In this example, we use the data set
and the model used in [betaMC: Example Using the BetaMC
Function](https://github.com/jeksterslab/betaMC/articles/example-beta-mc.md).

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

### Multiple Correlation Coefficients

#### Normal-Theory Approach

``` r

mvn <- RSqMC(mvn)
```

#### Asymptotic distribution-free Approach

``` r

adf <- RSqMC(adf)
```

#### Heteroskedasticity Consistent Approach (HC3)

``` r

hc3 <- RSqMC(hc3)
```

## Methods

### summary

Summary of the results of
[`RSqMC()`](https://github.com/jeksterslab/betaMC/reference/RSqMC.md).

``` r

summary(mvn)
#> Call:
#> RSqMC(object = mvn)
#> 
#> R-squared and adjusted R-squared
#> type = "mvn"
#>        est     se     R  0.05%   0.5%   2.5%  97.5%  99.5% 99.95%
#> rsq 0.8045 0.0557 20000 0.5087 0.5987 0.6625 0.8806 0.9012 0.9233
#> adj 0.7906 0.0597 20000 0.4736 0.5700 0.6384 0.8721 0.8941 0.9179
summary(adf)
#> Call:
#> RSqMC(object = adf)
#> 
#> R-squared and adjusted R-squared
#> type = "adf"
#>        est     se     R  0.05%   0.5%   2.5%  97.5% 99.5% 99.95%
#> rsq 0.8045 0.0546 20000 0.5430 0.6174 0.6661 0.8797 0.902 0.9217
#> adj 0.7906 0.0585 20000 0.5104 0.5901 0.6423 0.8711 0.895 0.9161
summary(hc3)
#> Call:
#> RSqMC(object = hc3)
#> 
#> R-squared and adjusted R-squared
#> type = "hc3"
#>        est     se     R  0.05%   0.5%   2.5%  97.5%  99.5% 99.95%
#> rsq 0.8045 0.0616 20000 0.4742 0.5806 0.6477 0.8872 0.9111 0.9384
#> adj 0.7906 0.0660 20000 0.4367 0.5506 0.6226 0.8792 0.9048 0.9339
```

### coef

Return the vector of estimates.

``` r

coef(mvn)
#>       rsq       adj 
#> 0.8045263 0.7905638
coef(adf)
#>       rsq       adj 
#> 0.8045263 0.7905638
coef(hc3)
#>       rsq       adj 
#> 0.8045263 0.7905638
```

### vcov

Return the sampling covariance matrix.

``` r

vcov(mvn)
#>             rsq         adj
#> rsq 0.003102177 0.003323761
#> adj 0.003323761 0.003561172
vcov(adf)
#>             rsq         adj
#> rsq 0.002978179 0.003190906
#> adj 0.003190906 0.003418828
vcov(hc3)
#>             rsq         adj
#> rsq 0.003797717 0.004068983
#> adj 0.004068983 0.004359624
```

### confint

Return confidence intervals.

``` r

confint(mvn, level = 0.95)
#>         2.5 %    97.5 %
#> rsq 0.6624855 0.8806143
#> adj 0.6383773 0.8720868
confint(adf, level = 0.95)
#>         2.5 %    97.5 %
#> rsq 0.6661151 0.8796947
#> adj 0.6422662 0.8711015
confint(hc3, level = 0.95)
#>         2.5 %    97.5 %
#> rsq 0.6477215 0.8872304
#> adj 0.6225588 0.8791754
```

## References

Dudgeon, P. (2017). Some improvements in confidence intervals for
standardized regression coefficients. *Psychometrika*, *82*(4), 928–951.
<https://doi.org/10.1007/s11336-017-9563-z>
