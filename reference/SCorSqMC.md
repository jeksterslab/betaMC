# Estimate Squared Semipartial Correlation Coefficients and the Corresponding Confidence Intervals From the Signed Intervals

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

## Details

The semipartial correlation \\(sr\_{j}\\) (see
[`SCorMC()`](https://jeksterslab.github.io/betaMC/reference/SCorMC.md))
is signed, and its confidence interval \\(\left\[ l, u \right\]\\) may
contain zero. The interval for \\(sr^{2}\_{j}\\) is the image of that
interval under squaring. The squared interval is \\(\left\[ l^{2}, u^{2}
\right\]\\) if \\(0 \< l\\), \\(\left\[ u^{2}, l^{2} \right\]\\) if \\(u
\< 0\\), and \\(\left\[ 0, \max \left( l^{2}, u^{2} \right) \right\]\\)
if the signed interval contains zero. This is not the interval of
squared draws, and it is not an interval for
[`DeltaRSqMC()`](https://jeksterslab.github.io/betaMC/reference/DeltaRSqMC.md).

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

# SCorSqMC -----------------------------------------------------------------
SCorSqMC(object = mc, level = 0.95)
#>               est      lower     upper
#> NARTIC  0.1858925 0.04415470 0.2931601
#> PCTGRT  0.1176542 0.01356955 0.2229854
#> PCTSUPP 0.0568722 0.01333903 0.1272283
```
