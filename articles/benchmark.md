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
#>   expr       min        lq      mean    median        uq       max neval
#> 1   MC  527.2678  539.9609  568.1132  554.8269  585.4245  667.8665    10
#> 2   NB 7391.2478 7415.6150 7616.6614 7544.4936 7870.1001 8057.7404    10
```

### Summary of Benchmark Results Relative to the Faster Method

``` r

summary(benchmark, unit = "relative")
#>   expr      min       lq     mean   median       uq     max neval
#> 1   MC  1.00000  1.00000  1.00000  1.00000  1.00000  1.0000    10
#> 2   NB 14.01801 13.73361 13.40694 13.59792 13.44341 12.0649    10
```

## Plot

![](fig-vignettes-benchmark-unnamed-chunk-15-1.png)
