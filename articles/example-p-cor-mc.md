# betaMC: Example Using the PCorMC Function

Confidence intervals for squared partial correlation coefficients are
generated using the
[`PCorMC()`](https://github.com/jeksterslab/betaMC/reference/PCorMC.md)
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

### Squared Partial Correlation Coefficients

#### Normal-Theory Approach

``` r

mvn <- PCorMC(mvn)
```

#### Asymptotic distribution-free Approach

``` r

adf <- PCorMC(adf)
```

#### Heteroskedasticity Consistent Approach (HC3)

``` r

hc3 <- PCorMC(hc3)
```

## Methods

### summary

Summary of the results of
[`PCorMC()`](https://github.com/jeksterslab/betaMC/reference/PCorMC.md).

``` r

summary(mvn)
#> Call:
#> PCorMC(object = mvn)
#> 
#> Squared partial correlations
#> type = "mvn"
#>            est     se     R  0.05%   0.5%   2.5%  97.5%  99.5% 99.95%
#> NARTIC  0.4874 0.1054 20000 0.1160 0.1838 0.2441 0.6524 0.7095 0.7696
#> PCTGRT  0.3757 0.1077 20000 0.0536 0.1026 0.1483 0.5638 0.6291 0.7029
#> PCTSUPP 0.2254 0.0988 20000 0.0040 0.0207 0.0462 0.4278 0.5023 0.5901
summary(adf)
#> Call:
#> PCorMC(object = adf)
#> 
#> Squared partial correlations
#> type = "adf"
#>            est     se     R  0.05%   0.5%   2.5%  97.5%  99.5% 99.95%
#> NARTIC  0.4874 0.1000 20000 0.0333 0.1464 0.2410 0.6339 0.6890 0.7524
#> PCTGRT  0.3757 0.1009 20000 0.0236 0.0857 0.1447 0.5355 0.5912 0.6506
#> PCTSUPP 0.2254 0.1030 20000 0.0005 0.0099 0.0368 0.4314 0.5037 0.5885
summary(hc3)
#> Call:
#> PCorMC(object = hc3)
#> 
#> Squared partial correlations
#> type = "hc3"
#>            est     se     R  0.05%   0.5%   2.5%  97.5%  99.5% 99.95%
#> NARTIC  0.4874 0.1192 20000 0.0068 0.0689 0.1762 0.6510 0.7054 0.7691
#> PCTGRT  0.3757 0.1159 20000 0.0053 0.0436 0.1031 0.5554 0.6131 0.6835
#> PCTSUPP 0.2254 0.1137 20000 0.0000 0.0032 0.0239 0.4593 0.5379 0.6452
```

### coef

Return the vector of estimates.

``` r

coef(mvn)
#>    NARTIC    PCTGRT   PCTSUPP 
#> 0.4874382 0.3757383 0.2253739
coef(adf)
#>    NARTIC    PCTGRT   PCTSUPP 
#> 0.4874382 0.3757383 0.2253739
coef(hc3)
#>    NARTIC    PCTGRT   PCTSUPP 
#> 0.4874382 0.3757383 0.2253739
```

### vcov

Return the sampling covariance matrix.

``` r

vcov(mvn)
#>               NARTIC       PCTGRT      PCTSUPP
#> NARTIC  0.0111144612 0.0006625784 0.0005003605
#> PCTGRT  0.0006625784 0.0116038584 0.0001917074
#> PCTSUPP 0.0005003605 0.0001917074 0.0097534203
vcov(adf)
#>              NARTIC       PCTGRT      PCTSUPP
#> NARTIC  0.009996850 0.0025516955 0.0020388827
#> PCTGRT  0.002551695 0.0101874732 0.0008048103
#> PCTSUPP 0.002038883 0.0008048103 0.0105993612
vcov(hc3)
#>              NARTIC      PCTGRT     PCTSUPP
#> NARTIC  0.014200146 0.003883779 0.002982934
#> PCTGRT  0.003883779 0.013428109 0.001075290
#> PCTSUPP 0.002982934 0.001075290 0.012933597
```

### confint

Return confidence intervals.

``` r

confint(mvn, level = 0.95)
#>              2.5 %    97.5 %
#> NARTIC  0.24409473 0.6524227
#> PCTGRT  0.14832354 0.5637740
#> PCTSUPP 0.04616793 0.4278365
confint(adf, level = 0.95)
#>              2.5 %    97.5 %
#> NARTIC  0.24096240 0.6339433
#> PCTGRT  0.14473941 0.5355184
#> PCTSUPP 0.03677742 0.4314395
confint(hc3, level = 0.95)
#>              2.5 %    97.5 %
#> NARTIC  0.17621806 0.6510287
#> PCTGRT  0.10310770 0.5553741
#> PCTSUPP 0.02385487 0.4593324
```

## References

Dudgeon, P. (2017). Some improvements in confidence intervals for
standardized regression coefficients. *Psychometrika*, *82*(4), 928–951.
<https://doi.org/10.1007/s11336-017-9563-z>
