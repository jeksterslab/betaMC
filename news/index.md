# Changelog

## betaMC 1.3.5

### Minor

- New
  [`FSqMC()`](https://jeksterslab.github.io/betaMC/reference/FSqMC.md):
  Cohen’s f-squared and the local f-squared of each regressor.
- New
  [`SCorSqMC()`](https://jeksterslab.github.io/betaMC/reference/SCorSqMC.md):
  the image of a signed-semipartial interval under squaring.
- Faster Monte Carlo: the draws are screened and the effect sizes
  computed in C++ (Rcpp, RcppArmadillo), once per
  [`MC()`](https://jeksterslab.github.io/betaMC/reference/MC.md) or
  [`MCMI()`](https://jeksterslab.github.io/betaMC/reference/MCMI.md)
  call. The draws are the same as before for the same seed.
- The `thetahatstar` elements of `mc` and `betamc` objects are now
  matrices (one draw per row) instead of lists. `mc` objects also hold
  `vechsigmacap` and `es`.
- A draw that fails the admissibility checks 1000 times in a row is
  dropped (earlier versions could loop without end).
- Selective standardization: the arguments `std_y` and `std_x`
  standardize only y or only the named regressors (for example, leave a
  dummy variable in its 0-1 units). The defaults give the same results
  as before.
- The internal copies from linearAlgebra 0.0.5 (a faster duplication
  matrix and its inverse), nBootstrap 0.1.0 (quantiles in C++),
  strRegression 0.0.2, and processLM 0.0.11 are current. The
  pseudo-inverse of the duplication matrix is now exact, so results for
  a fixed seed may differ slightly from 1.3.4; the difference is within
  Monte Carlo error.

### Patch

- [`MC()`](https://jeksterslab.github.io/betaMC/reference/MC.md) and
  [`MCMI()`](https://jeksterslab.github.io/betaMC/reference/MCMI.md)
  stop with an error for an `lm` fit without an intercept, with
  `weights`, or with an `offset`. These fits were accepted before, and
  the results were wrong.
- [`DiffBetaMC()`](https://jeksterslab.github.io/betaMC/reference/DiffBetaMC.md)
  computes the differences of the standardized slopes without a loop.
- Documentation: the
  [`MC()`](https://jeksterslab.github.io/betaMC/reference/MC.md) and
  [`MCMI()`](https://jeksterslab.github.io/betaMC/reference/MCMI.md)
  help pages list
  [`FSqMC()`](https://jeksterslab.github.io/betaMC/reference/FSqMC.md)
  and
  [`SCorSqMC()`](https://jeksterslab.github.io/betaMC/reference/SCorSqMC.md)
  among the functions that accept their output;
  [`SCorSqMC()`](https://jeksterslab.github.io/betaMC/reference/SCorSqMC.md)
  joins the Beta Monte Carlo family.
- Documentation: formulas use `\operatorname{}` and `\boldsymbol{}`; the
  undefined `\matnrm{}` macro in the Monte Carlo bias, variance, and
  RMSE help pages is now `\mathrm{}`; the DOI links use `https`.

### Documentation

- The examples name every argument and call
  [`stats::lm()`](https://rdrr.io/r/stats/lm.html).
- [`DeltaRSqMC()`](https://jeksterslab.github.io/betaMC/reference/DeltaRSqMC.md)
  cites Algina, Keselman, and Penfield (2007, 2008a, 2008b, 2010), as
  `DeltaRSqSandwich()` and `DeltaRSqNB()` do.

## betaMC 1.3.4

CRAN release: 2026-06-11

### Patch

- Minor documentation edits.

## betaMC 1.3.3

CRAN release: 2025-10-19

### Patch

- Minor edits to methods.

## betaMC 1.3.2

CRAN release: 2024-04-14

### Patch

- Minor documentation edits.

## betaMC 1.3.1

CRAN release: 2023-10-15

### Patch

- Minor edits to setting seed.

## betaMC 1.3.0

CRAN release: 2023-08-22

### Minor

- Added the
  [`MCMI()`](https://jeksterslab.github.io/betaMC/reference/MCMI.md)
  function to perform Monte Carlo simulation of parameters for data sets
  with missing values using multiple imputation.

## betaMC 1.2.0

CRAN release: 2023-04-13

### Minor

- The Monte Carlo simulation of parameters is done by the
  [`MC()`](https://jeksterslab.github.io/betaMC/reference/MC.md)
  function. The output of the
  [`MC()`](https://jeksterslab.github.io/betaMC/reference/MC.md)
  function is passed to the
  [`BetaMC()`](https://jeksterslab.github.io/betaMC/reference/BetaMC.md),
  [`RSqMC()`](https://jeksterslab.github.io/betaMC/reference/RSqMC.md),
  [`SCorMC()`](https://jeksterslab.github.io/betaMC/reference/SCorMC.md),
  [`DeltaRSqMC()`](https://jeksterslab.github.io/betaMC/reference/DeltaRSqMC.md),
  [`PCorMC()`](https://jeksterslab.github.io/betaMC/reference/PCorMC.md),
  and
  [`DiffBetaMC()`](https://jeksterslab.github.io/betaMC/reference/DiffBetaMC.md)
  functions to generate Monte Carlo confidence intervals for various
  regression effect sizes.

## betaMC 1.0.0

CRAN release: 2023-01-09

### Major

- And so it begins.
