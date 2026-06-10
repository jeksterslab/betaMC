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
#>        est     se     R  0.05%   0.5%   2.5%  97.5% 99.5% 99.95%
#> rsq 0.8045 0.0557 20000 0.5176 0.6001 0.6620 0.8801 0.903 0.9277
#> adj 0.7906 0.0597 20000 0.4831 0.5716 0.6378 0.8715 0.896 0.9226
summary(adf)
#> Call:
#> RSqMC(object = adf)
#> 
#> R-squared and adjusted R-squared
#> type = "adf"
#>        est     se     R  0.05%   0.5%   2.5%  97.5%  99.5% 99.95%
#> rsq 0.8045 0.0546 20000 0.5398 0.6140 0.6669 0.8796 0.9022 0.9228
#> adj 0.7906 0.0585 20000 0.5069 0.5865 0.6431 0.8710 0.8952 0.9173
summary(hc3)
#> Call:
#> RSqMC(object = hc3)
#> 
#> R-squared and adjusted R-squared
#> type = "hc3"
#>        est     se     R  0.05%   0.5%   2.5%  97.5%  99.5% 99.95%
#> rsq 0.8045 0.0619 20000 0.4772 0.5781 0.6465 0.8876 0.9115 0.9374
#> adj 0.7906 0.0663 20000 0.4399 0.5480 0.6212 0.8795 0.9052 0.9330
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
#> rsq 0.003103479 0.003325156
#> adj 0.003325156 0.003562667
vcov(adf)
#>             rsq         adj
#> rsq 0.002982790 0.003195847
#> adj 0.003195847 0.003424122
vcov(hc3)
#>             rsq         adj
#> rsq 0.003827733 0.004101142
#> adj 0.004101142 0.004394081
```

### confint

Return confidence intervals.

``` r

confint(mvn, level = 0.95)
#>         2.5 %    97.5 %
#> rsq 0.6619727 0.8800877
#> adj 0.6378279 0.8715226
confint(adf, level = 0.95)
#>         2.5 %    97.5 %
#> rsq 0.6668676 0.8796026
#> adj 0.6430725 0.8710028
confint(hc3, level = 0.95)
#>         2.5 %    97.5 %
#> rsq 0.6464862 0.8875665
#> adj 0.6212352 0.8795355
```

## References

Dudgeon, P. (2017). Some improvements in confidence intervals for
standardized regression coefficients. *Psychometrika*, *82*(4), 928–951.
<https://doi.org/10.1007/s11336-017-9563-z>
