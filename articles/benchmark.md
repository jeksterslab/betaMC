# Benchmark: Comparing the Monte Carlo Method with Nonparametric Bootstrapping

We compare the Monte Carlo (MC) method with nonparametric bootstrapping
(NB) for standardized regression coefficients. In this example, we use
the data set and the model used in [betaMC: Example Using the BetaMC
Function](https://github.com/jeksterslab/betaMC/articles/example-beta-mc.md).

``` r

library(betaMC)
library(boot)
library(microbenchmark)
```

The
[`BetaMC()`](https://github.com/jeksterslab/betaMC/reference/BetaMC.md)
function is used to generate MC confidence intervals. The `BetaNB()`
function is used to generate NB confidence intervals.

``` r

BetaNB <- function(formula, data, B) {
  statistic <- function(formula, data, indices) {
    return(
      coef(lm(formula = formula, data = as.data.frame(scale(data[indices, ]))))[-1]
    )
  }
  return(boot.ci(boot(data = data, statistic = statistic, formula = formula, R = B)))
}
```

## Data and Model

``` r

df <- betaMC::nas1982
```

## Benchmark

### Arguments

| Variables | Values | Notes                               |
|:----------|:-------|:------------------------------------|
| R         | 5000   | Number of Monte Carlo replications. |
| B         | 5000   | Number of bootstrap samples.        |

``` r

benchmark <- microbenchmark(
  MC = {
    formula <- "QUALITY ~ NARTIC + PCTGRT + PCTSUPP"
    object <- lm(formula = formula, data = df)
    mc <- MC(object = object, R = R, type = "mvn")
    BetaMC(object = mc)
  },
  NB = {
    formula <- "QUALITY ~ NARTIC + PCTGRT + PCTSUPP"
    object <- lm(formula = formula, data = df)
    BetaNB(formula = formula, data = df, B = B)
  },
  times = 10
)
```

### Summary of Benchmark Results

``` r

summary(benchmark, unit = "ms")
#>   expr       min       lq      mean    median        uq      max neval
#> 1   MC  304.3814  309.355  330.4766  322.2439  349.3204  379.391    10
#> 2   NB 4477.1263 4532.300 4623.9722 4579.0776 4634.5294 5153.932    10
```

### Summary of Benchmark Results Relative to the Faster Method

``` r

summary(benchmark, unit = "relative")
#>   expr      min      lq     mean   median       uq      max neval
#> 1   MC  1.00000  1.0000  1.00000  1.00000  1.00000  1.00000    10
#> 2   NB 14.70894 14.6508 13.99183 14.20998 13.26728 13.58475    10
```

## Plot

![](fig-vignettes-benchmark-unnamed-chunk-15-1.png)
