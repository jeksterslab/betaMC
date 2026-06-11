# betaMC: Example Using the DiffBetaMC Function

Confidence intervals for differences of standardized regression slopes
are generated using the
[`DiffBetaMC()`](https://github.com/jeksterslab/betaMC/reference/DiffBetaMC.md)
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

### Differences of Standardized Regression Slopes

#### Normal-Theory Approach

``` r

mvn <- DiffBetaMC(mvn)
```

#### Asymptotic distribution-free Approach

``` r

adf <- DiffBetaMC(adf)
```

#### Heteroskedasticity Consistent Approach (HC3)

``` r

hc3 <- DiffBetaMC(hc3)
```

## Methods

### summary

Summary of the results of
[`DiffBetaMC()`](https://github.com/jeksterslab/betaMC/reference/DiffBetaMC.md).

``` r

summary(mvn)
#> Call:
#> DiffBetaMC(object = mvn)
#> 
#> Differences of standardized regression slopes
#> type = "mvn"
#>                   est     se     R   0.05%    0.5%    2.5%  97.5%  99.5% 99.95%
#> NARTIC-PCTGRT  0.1037 0.1356 20000 -0.3587 -0.2541 -0.1661 0.3639 0.4424 0.5236
#> NARTIC-PCTSUPP 0.2319 0.1251 20000 -0.1881 -0.1008 -0.0222 0.4679 0.5373 0.6123
#> PCTGRT-PCTSUPP 0.1282 0.1217 20000 -0.2956 -0.1929 -0.1144 0.3610 0.4325 0.5253
summary(adf)
#> Call:
#> DiffBetaMC(object = adf)
#> 
#> Differences of standardized regression slopes
#> type = "adf"
#>                   est     se     R   0.05%    0.5%    2.5%  97.5%  99.5% 99.95%
#> NARTIC-PCTGRT  0.1037 0.1211 20000 -0.2957 -0.2068 -0.1341 0.3371 0.4182 0.5103
#> NARTIC-PCTSUPP 0.2319 0.1186 20000 -0.1496 -0.0779 -0.0038 0.4590 0.5363 0.6084
#> PCTGRT-PCTSUPP 0.1282 0.1202 20000 -0.2789 -0.1861 -0.1112 0.3580 0.4315 0.5218
summary(hc3)
#> Call:
#> DiffBetaMC(object = hc3)
#> 
#> Differences of standardized regression slopes
#> type = "hc3"
#>                   est     se     R   0.05%    0.5%    2.5%  97.5%  99.5% 99.95%
#> NARTIC-PCTGRT  0.1037 0.1420 20000 -0.3424 -0.2655 -0.1793 0.3778 0.4682 0.5478
#> NARTIC-PCTSUPP 0.2319 0.1324 20000 -0.2302 -0.1258 -0.0387 0.4785 0.5537 0.6537
#> PCTGRT-PCTSUPP 0.1282 0.1367 20000 -0.3542 -0.2389 -0.1490 0.3872 0.4731 0.5633
```

### coef

Return the vector of estimates.

``` r

coef(mvn)
#>  NARTIC-PCTGRT NARTIC-PCTSUPP PCTGRT-PCTSUPP 
#>      0.1036564      0.2318974      0.1282410
coef(adf)
#>  NARTIC-PCTGRT NARTIC-PCTSUPP PCTGRT-PCTSUPP 
#>      0.1036564      0.2318974      0.1282410
coef(hc3)
#>  NARTIC-PCTGRT NARTIC-PCTSUPP PCTGRT-PCTSUPP 
#>      0.1036564      0.2318974      0.1282410
```

### vcov

Return the sampling covariance matrix.

``` r

vcov(mvn)
#>                NARTIC-PCTGRT NARTIC-PCTSUPP PCTGRT-PCTSUPP
#> NARTIC-PCTGRT    0.018385788    0.009611932   -0.008773856
#> NARTIC-PCTSUPP   0.009611932    0.015661129    0.006049196
#> PCTGRT-PCTSUPP  -0.008773856    0.006049196    0.014823052
vcov(adf)
#>                NARTIC-PCTGRT NARTIC-PCTSUPP PCTGRT-PCTSUPP
#> NARTIC-PCTGRT    0.014664868    0.007132245   -0.007532622
#> NARTIC-PCTSUPP   0.007132245    0.014056262    0.006924016
#> PCTGRT-PCTSUPP  -0.007532622    0.006924016    0.014456638
vcov(hc3)
#>                NARTIC-PCTGRT NARTIC-PCTSUPP PCTGRT-PCTSUPP
#> NARTIC-PCTGRT     0.02015545    0.009495700   -0.010659749
#> NARTIC-PCTSUPP    0.00949570    0.017530941    0.008035241
#> PCTGRT-PCTSUPP   -0.01065975    0.008035241    0.018694990
```

### confint

Return confidence intervals.

``` r

confint(mvn, level = 0.95)
#>                      2.5 %    97.5 %
#> NARTIC-PCTGRT  -0.16608844 0.3638731
#> NARTIC-PCTSUPP -0.02220833 0.4679121
#> PCTGRT-PCTSUPP -0.11438475 0.3610355
confint(adf, level = 0.95)
#>                       2.5 %    97.5 %
#> NARTIC-PCTGRT  -0.134072151 0.3371149
#> NARTIC-PCTSUPP -0.003837543 0.4590238
#> PCTGRT-PCTSUPP -0.111152006 0.3579665
confint(hc3, level = 0.95)
#>                      2.5 %    97.5 %
#> NARTIC-PCTGRT  -0.17932002 0.3777787
#> NARTIC-PCTSUPP -0.03867004 0.4785117
#> PCTGRT-PCTSUPP -0.14903956 0.3872077
```

## References

Dudgeon, P. (2017). Some improvements in confidence intervals for
standardized regression coefficients. *Psychometrika*, *82*(4), 928–951.
<https://doi.org/10.1007/s11336-017-9563-z>
