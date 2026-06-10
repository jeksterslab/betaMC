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
#> NARTIC-PCTGRT  0.1037 0.1341 20000 -0.3485 -0.2403 -0.1605 0.3624 0.4417 0.5299
#> NARTIC-PCTSUPP 0.2319 0.1250 20000 -0.1935 -0.1009 -0.0191 0.4703 0.5404 0.6231
#> PCTGRT-PCTSUPP 0.1282 0.1221 20000 -0.2808 -0.1869 -0.1141 0.3648 0.4279 0.5078
summary(adf)
#> Call:
#> DiffBetaMC(object = adf)
#> 
#> Differences of standardized regression slopes
#> type = "adf"
#>                   est     se     R   0.05%    0.5%    2.5%  97.5%  99.5% 99.95%
#> NARTIC-PCTGRT  0.1037 0.1207 20000 -0.2777 -0.2058 -0.1350 0.3393 0.4152 0.5071
#> NARTIC-PCTSUPP 0.2319 0.1180 20000 -0.1358 -0.0706 -0.0047 0.4551 0.5328 0.6085
#> PCTGRT-PCTSUPP 0.1282 0.1205 20000 -0.2628 -0.1901 -0.1152 0.3583 0.4309 0.5127
summary(hc3)
#> Call:
#> DiffBetaMC(object = hc3)
#> 
#> Differences of standardized regression slopes
#> type = "hc3"
#>                   est     se     R   0.05%    0.5%    2.5%  97.5%  99.5% 99.95%
#> NARTIC-PCTGRT  0.1037 0.1407 20000 -0.3646 -0.2604 -0.1748 0.3762 0.4665 0.5576
#> NARTIC-PCTSUPP 0.2319 0.1327 20000 -0.2358 -0.1192 -0.0398 0.4830 0.5624 0.6533
#> PCTGRT-PCTSUPP 0.1282 0.1364 20000 -0.3271 -0.2307 -0.1479 0.3935 0.4659 0.5480
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
#> NARTIC-PCTGRT    0.017975594    0.009347022   -0.008628571
#> NARTIC-PCTSUPP   0.009347022    0.015631881    0.006284859
#> PCTGRT-PCTSUPP  -0.008628571    0.006284859    0.014913430
vcov(adf)
#>                NARTIC-PCTGRT NARTIC-PCTSUPP PCTGRT-PCTSUPP
#> NARTIC-PCTGRT    0.014574533    0.006984203   -0.007590330
#> NARTIC-PCTSUPP   0.006984203    0.013918032    0.006933829
#> PCTGRT-PCTSUPP  -0.007590330    0.006933829    0.014524159
vcov(hc3)
#>                NARTIC-PCTGRT NARTIC-PCTSUPP PCTGRT-PCTSUPP
#> NARTIC-PCTGRT    0.019793356    0.009392838   -0.010400518
#> NARTIC-PCTSUPP   0.009392838    0.017604247    0.008211409
#> PCTGRT-PCTSUPP  -0.010400518    0.008211409    0.018611927
```

### confint

Return confidence intervals.

``` r

confint(mvn, level = 0.95)
#>                      2.5 %    97.5 %
#> NARTIC-PCTGRT  -0.16054041 0.3623650
#> NARTIC-PCTSUPP -0.01912096 0.4702712
#> PCTGRT-PCTSUPP -0.11408019 0.3648405
confint(adf, level = 0.95)
#>                      2.5 %    97.5 %
#> NARTIC-PCTGRT  -0.13504158 0.3392922
#> NARTIC-PCTSUPP -0.00471495 0.4551428
#> PCTGRT-PCTSUPP -0.11516167 0.3583482
confint(hc3, level = 0.95)
#>                      2.5 %    97.5 %
#> NARTIC-PCTGRT  -0.17480706 0.3762299
#> NARTIC-PCTSUPP -0.03975073 0.4830382
#> PCTGRT-PCTSUPP -0.14789040 0.3935448
```

## References

Dudgeon, P. (2017). Some improvements in confidence intervals for
standardized regression coefficients. *Psychometrika*, *82*(4), 928–951.
<https://doi.org/10.1007/s11336-017-9563-z>
