# Summary Method for an Object of Class `betamc`

Summary Method for an Object of Class `betamc`

## Usage

``` r
# S3 method for class 'betamc'
summary(object, alpha = NULL, digits = 4, ...)
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

- alpha:

  Numeric vector. Significance level \\(\alpha\\). If `alpha = NULL`,
  use the argument `alpha` used in `object`.

- digits:

  Digits to print.

- ...:

  additional arguments.

## Value

Returns a matrix of estimates, standard errors, number of Monte Carlo
replications, and confidence intervals.

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
summary(object = out)
#> Call:
#> BetaMC(object = mc, alpha = 0.05)
#> 
#> Standardized regression slopes
#> type = "hc3"
#>            est     se   R   2.5%  97.5%
#> NARTIC  0.4951 0.0760 100 0.3508 0.6231
#> PCTGRT  0.3915 0.0732 100 0.2643 0.5224
#> PCTSUPP 0.2632 0.0808 100 0.1014 0.4009
```
