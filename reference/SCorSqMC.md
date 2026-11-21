# Squared Signed-Semipartial Confidence Intervals

Square the signed interval as a set, with lower limit zero if it
contains zero. This differs from an interval of squared draws. No draws
are generated and no endpoints are clipped to one. Failed signed
endpoints remain missing.

## Usage

``` r
SCorSqMC(object, level = 0.95)
```

## Arguments

- object:

  Context accepted by
  [`SCorMC()`](https://jeksterslab.github.io/betaMC/reference/SCorMC.md).

- level:

  Single confidence level in (0, 1).

## Value

Data frame with est, lower and upper columns and predictor row names.
The point estimate is the squared signed estimate. No standard error is
implied by this interval transformation.

## See also

Other Beta Monte Carlo Functions:
[`BetaMC()`](https://jeksterslab.github.io/betaMC/reference/BetaMC.md),
[`DeltaRSqMC()`](https://jeksterslab.github.io/betaMC/reference/DeltaRSqMC.md),
[`DiffBetaMC()`](https://jeksterslab.github.io/betaMC/reference/DiffBetaMC.md),
[`FSqMC()`](https://jeksterslab.github.io/betaMC/reference/FSqMC.md),
[`MC()`](https://jeksterslab.github.io/betaMC/reference/MC.md),
[`MCMI()`](https://jeksterslab.github.io/betaMC/reference/MCMI.md),
[`PCorMC()`](https://jeksterslab.github.io/betaMC/reference/PCorMC.md),
[`RSqMC()`](https://jeksterslab.github.io/betaMC/reference/RSqMC.md),
[`SCorMC()`](https://jeksterslab.github.io/betaMC/reference/SCorMC.md)

## Author

Ivan Jacob Agaloos Pesigan

## Examples

``` r
fit <- stats::lm(formula = mpg ~ wt + hp, data = mtcars)
out <- MC(object = fit, R = 199)
SCorSqMC(object = out)
#>           est     lower     upper
#> wt 0.22434811 0.0618421 0.4233347
#> hp 0.07395266 0.0149379 0.1643464
```
