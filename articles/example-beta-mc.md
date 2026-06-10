# betaMC: Example Using the BetaMC Function

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

mvn <- BetaMC(mvn)
```

#### Asymptotic distribution-free Approach

``` r

adf <- BetaMC(adf)
```

#### Heteroskedasticity Consistent Approach (HC3)

``` r

hc3 <- BetaMC(hc3)
```

## Methods

### summary

Summary of the results of
[`BetaMC()`](https://github.com/jeksterslab/betaMC/reference/BetaMC.md).

``` r

summary(mvn)
#> Call:
#> BetaMC(object = mvn)
#> 
#> Standardized regression slopes
#> type = "mvn"
#>            est     se     R  0.05%   0.5%   2.5%  97.5%  99.5% 99.95%
#> NARTIC  0.4951 0.0758 20000 0.2445 0.2926 0.3399 0.6346 0.6774 0.7230
#> PCTGRT  0.3915 0.0765 20000 0.1463 0.1953 0.2370 0.5352 0.5829 0.6368
#> PCTSUPP 0.2632 0.0748 20000 0.0228 0.0704 0.1185 0.4113 0.4591 0.5041
summary(adf)
#> Call:
#> BetaMC(object = adf)
#> 
#> Standardized regression slopes
#> type = "adf"
#>            est     se     R   0.05%   0.5%   2.5%  97.5%  99.5% 99.95%
#> NARTIC  0.4951 0.0675 20000  0.2520 0.3101 0.3517 0.6150 0.6558 0.7018
#> PCTGRT  0.3915 0.0709 20000  0.1421 0.1978 0.2431 0.5216 0.5585 0.5962
#> PCTSUPP 0.2632 0.0771 20000 -0.0014 0.0510 0.1059 0.4088 0.4541 0.5066
summary(hc3)
#> Call:
#> BetaMC(object = hc3)
#> 
#> Standardized regression slopes
#> type = "hc3"
#>            est     se     R   0.05%   0.5%   2.5%  97.5%  99.5% 99.95%
#> NARTIC  0.4951 0.0798 20000  0.2140 0.2717 0.3216 0.6337 0.6787 0.7285
#> PCTGRT  0.3915 0.0824 20000  0.0997 0.1649 0.2186 0.5426 0.5876 0.6338
#> PCTSUPP 0.2632 0.0855 20000 -0.0300 0.0349 0.0906 0.4259 0.4768 0.5493
```

### coef

Return the vector of estimates.

``` r

coef(mvn)
#>    NARTIC    PCTGRT   PCTSUPP 
#> 0.4951451 0.3914887 0.2632477
coef(adf)
#>    NARTIC    PCTGRT   PCTSUPP 
#> 0.4951451 0.3914887 0.2632477
coef(hc3)
#>    NARTIC    PCTGRT   PCTSUPP 
#> 0.4951451 0.3914887 0.2632477
```

### vcov

Return the sampling covariance matrix.

``` r

vcov(mvn)
#>               NARTIC       PCTGRT      PCTSUPP
#> NARTIC   0.005739275 -0.003259059 -0.002152491
#> PCTGRT  -0.003259059  0.005856455 -0.001737027
#> PCTSUPP -0.002152491 -0.001737027  0.005588552
vcov(adf)
#>               NARTIC       PCTGRT      PCTSUPP
#> NARTIC   0.004558001 -0.002486244 -0.001695488
#> PCTGRT  -0.002486244  0.005032713 -0.001911165
#> PCTSUPP -0.001695488 -0.001911165  0.005942077
vcov(hc3)
#>               NARTIC       PCTGRT      PCTSUPP
#> NARTIC   0.006360543 -0.003569931 -0.002077052
#> PCTGRT  -0.003569931  0.006792890 -0.002349528
#> PCTSUPP -0.002077052 -0.002349528  0.007313634
```

### confint

Return confidence intervals.

``` r

confint(mvn, level = 0.95)
#>             2.5 %    97.5 %
#> NARTIC  0.3399022 0.6346379
#> PCTGRT  0.2370137 0.5352295
#> PCTSUPP 0.1184817 0.4113184
confint(adf, level = 0.95)
#>             2.5 %    97.5 %
#> NARTIC  0.3516627 0.6150399
#> PCTGRT  0.2431319 0.5215691
#> PCTSUPP 0.1059107 0.4087849
confint(hc3, level = 0.95)
#>              2.5 %    97.5 %
#> NARTIC  0.32158794 0.6336526
#> PCTGRT  0.21858906 0.5426428
#> PCTSUPP 0.09059068 0.4258831
```

## References

Dudgeon, P. (2017). Some improvements in confidence intervals for
standardized regression coefficients. *Psychometrika*, *82*(4), 928–951.
<https://doi.org/10.1007/s11336-017-9563-z>

National Research Council. (1982). *An assessment of research-doctorate
programs in the United States: Social and behavioral sciences*. National
Academies Press. <https://doi.org/10.17226/9781>
