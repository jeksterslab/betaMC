# Print Method for an Object of Class `betamc`

Print Method for an Object of Class `betamc`

## Usage

``` r
# S3 method for class 'betamc'
print(x, alpha = NULL, digits = 4, ...)
```

## Arguments

- x:

  Object of Class `betamc`, that is, the output of the
  [`BetaMC()`](https://jeksterslab.github.io/betaMC/reference/BetaMC.md),
  [`RSqMC()`](https://jeksterslab.github.io/betaMC/reference/RSqMC.md),
  [`SCorMC()`](https://jeksterslab.github.io/betaMC/reference/SCorMC.md),
  [`DeltaRSqMC()`](https://jeksterslab.github.io/betaMC/reference/DeltaRSqMC.md),
  [`PCorMC()`](https://jeksterslab.github.io/betaMC/reference/PCorMC.md),
  [`FSqMC()`](https://jeksterslab.github.io/betaMC/reference/FSqMC.md),
  or
  [`DiffBetaMC()`](https://jeksterslab.github.io/betaMC/reference/DiffBetaMC.md)
  functions.

- alpha:

  Numeric vector. Significance level \\(\alpha\\). If `alpha = NULL`,
  use the argument `alpha` used in `x`.

- digits:

  Digits to print.

- ...:

  additional arguments.

## Value

Prints a matrix of estimates, standard errors, number of Monte Carlo
replications, and confidence intervals.

## Author

Ivan Jacob Agaloos Pesigan

## Examples

``` r
# Data ---------------------------------------------------------------------
data("nas1982", package = "betaMC")

# Fit Model in lm ----------------------------------------------------------
object <- stats::lm(
  formula = QUALITY ~ NARTIC + PCTGRT + PCTSUPP,
  data = nas1982
)

# MC -----------------------------------------------------------------------
mc <- MC(
  object = object,
  R = 100, # use a large value e.g., 20000L for actual research
  seed = 0508
)

# BetaMC -------------------------------------------------------------------
out <- BetaMC(object = mc, alpha = 0.05)

## Methods -----------------------------------------------------------------
print(x = out)
#> Call:
#> BetaMC(object = mc, alpha = 0.05)
#> 
#> Standardized regression slopes
#> type = "hc3"
#>            est     se   R   2.5%  97.5%
#> NARTIC  0.4951 0.0805 100 0.3107 0.6135
#> PCTGRT  0.3915 0.0810 100 0.2495 0.5347
#> PCTSUPP 0.2632 0.0730 100 0.1307 0.4055
```
