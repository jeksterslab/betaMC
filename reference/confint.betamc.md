# Confidence Intervals Method for an Object of Class `betamc`

Confidence Intervals Method for an Object of Class `betamc`

## Usage

``` r
# S3 method for class 'betamc'
confint(object, parm = NULL, level = 0.95, ...)
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

- parm:

  a specification of which parameters are to be given confidence
  intervals, either a vector of numbers or a vector of names. If
  missing, all parameters are considered.

- level:

  the confidence level required.

- ...:

  additional arguments.

## Value

Returns a matrix of confidence intervals.

## Details

The confidence intervals are percentile intervals, \\(\left\[
\hat{G}^{-1} \left( \alpha / 2 \right), \hat{G}^{-1} \left( 1 - \alpha /
2 \right) \right\]\\), where \\(\hat{G}\\) is the empirical distribution
function of the Monte Carlo draws (type 7 quantiles). The standard
errors are the standard deviations of the draws.

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
confint(object = out, level = 0.95)
#>             2.5 %    97.5 %
#> NARTIC  0.3507721 0.6231028
#> PCTGRT  0.2643226 0.5224279
#> PCTSUPP 0.1013892 0.4008517
```
