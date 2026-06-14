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
#>   expr       min        lq      mean   median        uq       max neval
#> 1   MC  323.0019  332.5833  342.8465  340.074  354.2382  368.3387    10
#> 2   NB 4726.7796 4756.0072 4813.2455 4765.061 4833.1233 5169.8796    10
```

### Summary of Benchmark Results Relative to the Faster Method

``` r

summary(benchmark, unit = "relative")
#>   expr      min      lq     mean   median       uq      max neval
#> 1   MC  1.00000  1.0000  1.00000  1.00000  1.00000  1.00000    10
#> 2   NB 14.63391 14.3002 14.03907 14.01184 13.64371 14.03567    10
```

## Plot

![](fig-vignettes-benchmark-unnamed-chunk-15-1.png)
