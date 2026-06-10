# betaMC: Example Using the DeltaRSqMC Function

Confidence intervals for improvement in R-squared are generated using
the
[`DeltaRSqMC()`](https://github.com/jeksterslab/betaMC/reference/DeltaRSqMC.md)
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

### Improvement in R-squared

#### Normal-Theory Approach

``` r

mvn <- DeltaRSqMC(mvn)
```

#### Asymptotic distribution-free Approach

``` r

adf <- DeltaRSqMC(adf)
```

#### Heteroskedasticity Consistent Approach (HC3)

``` r

hc3 <- DeltaRSqMC(hc3)
```

## Methods

### summary

Summary of the results of
[`DeltaRSqMC()`](https://github.com/jeksterslab/betaMC/reference/DeltaRSqMC.md).

``` r

summary(mvn)
#> Call:
#> DeltaRSqMC(object = mvn)
#> 
#> Improvement in R-squared
#> type = "mvn"
#>            est     se     R  0.05%   0.5%   2.5%  97.5%  99.5% 99.95%
#> NARTIC  0.1859 0.0652 20000 0.0347 0.0521 0.0722 0.3260 0.3853 0.4545
#> PCTGRT  0.1177 0.0514 20000 0.0155 0.0257 0.0377 0.2356 0.2904 0.3541
#> PCTSUPP 0.0569 0.0340 20000 0.0005 0.0042 0.0105 0.1396 0.1830 0.2373
summary(adf)
#> Call:
#> DeltaRSqMC(object = adf)
#> 
#> Improvement in R-squared
#> type = "adf"
#>            est     se     R  0.05%   0.5%   2.5%  97.5%  99.5% 99.95%
#> NARTIC  0.1859 0.0583 20000 0.0121 0.0425 0.0740 0.3036 0.3522 0.4098
#> PCTGRT  0.1177 0.0472 20000 0.0088 0.0225 0.0368 0.2190 0.2659 0.3316
#> PCTSUPP 0.0569 0.0331 20000 0.0000 0.0023 0.0089 0.1371 0.1729 0.2185
summary(hc3)
#> Call:
#> DeltaRSqMC(object = hc3)
#> 
#> Improvement in R-squared
#> type = "hc3"
#>            est     se     R  0.05%   0.5%   2.5%  97.5%  99.5% 99.95%
#> NARTIC  0.1859 0.0695 20000 0.0027 0.0182 0.0503 0.3234 0.3828 0.4560
#> PCTGRT  0.1177 0.0551 20000 0.0018 0.0107 0.0248 0.2385 0.2945 0.3626
#> PCTSUPP 0.0569 0.0378 20000 0.0001 0.0009 0.0061 0.1507 0.2015 0.2634
```

### coef

Return the vector of estimates.

``` r

coef(mvn)
#>    NARTIC    PCTGRT   PCTSUPP 
#> 0.1858925 0.1176542 0.0568722
coef(adf)
#>    NARTIC    PCTGRT   PCTSUPP 
#> 0.1858925 0.1176542 0.0568722
coef(hc3)
#>    NARTIC    PCTGRT   PCTSUPP 
#> 0.1858925 0.1176542 0.0568722
```

### vcov

Return the sampling covariance matrix.

``` r

vcov(mvn)
#>                NARTIC        PCTGRT       PCTSUPP
#> NARTIC   0.0042494392 -0.0007100277 -0.0003211197
#> PCTGRT  -0.0007100277  0.0026460471 -0.0002467873
#> PCTSUPP -0.0003211197 -0.0002467873  0.0011581213
vcov(adf)
#>                NARTIC        PCTGRT       PCTSUPP
#> NARTIC   3.399950e-03  6.109865e-05 -0.0001278611
#> PCTGRT   6.109865e-05  2.229678e-03 -0.0002112998
#> PCTSUPP -1.278611e-04 -2.112998e-04  0.0010964669
vcov(hc3)
#>                NARTIC        PCTGRT       PCTSUPP
#> NARTIC   4.836289e-03  0.0001833828 -5.771679e-05
#> PCTGRT   1.833828e-04  0.0030328839 -1.947042e-04
#> PCTSUPP -5.771679e-05 -0.0001947042  1.425686e-03
```

### confint

Return confidence intervals.

``` r

confint(mvn, level = 0.95)
#>              2.5 %    97.5 %
#> NARTIC  0.07223743 0.3259954
#> PCTGRT  0.03768865 0.2356369
#> PCTSUPP 0.01050429 0.1396394
confint(adf, level = 0.95)
#>               2.5 %    97.5 %
#> NARTIC  0.074037907 0.3035840
#> PCTGRT  0.036790648 0.2190039
#> PCTSUPP 0.008939227 0.1370532
confint(hc3, level = 0.95)
#>               2.5 %    97.5 %
#> NARTIC  0.050323375 0.3234248
#> PCTGRT  0.024846680 0.2384951
#> PCTSUPP 0.006059358 0.1506964
```

## References

Dudgeon, P. (2017). Some improvements in confidence intervals for
standardized regression coefficients. *Psychometrika*, *82*(4), 928–951.
<https://doi.org/10.1007/s11336-017-9563-z>
