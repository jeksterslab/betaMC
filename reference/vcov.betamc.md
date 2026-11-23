# Sampling Variance-Covariance Matrix Method for an Object of Class `betamc`

Sampling Variance-Covariance Matrix Method for an Object of Class
`betamc`

## Usage

``` r
# S3 method for class 'betamc'
vcov(object, ...)
```

## Arguments

- object:

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

- ...:

  additional arguments.

## Value

Returns the variance-covariance matrix of estimates.

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
vcov(object = out)
#>               NARTIC       PCTGRT      PCTSUPP
#> NARTIC   0.006474424 -0.003611896 -0.002023361
#> PCTGRT  -0.003611896  0.006565249 -0.001581942
#> PCTSUPP -0.002023361 -0.001581942  0.005336272
```
