# Changelog

## betaMC 1.3.4.9001

- Add SCorSqMC() for the image of a signed-semipartial interval under
  squaring.

## betaMC 1.3.4.9000

### Minor

- New
  [`FSqMC()`](https://jeksterslab.github.io/betaMC/reference/FSqMC.md):
  Cohen’s f-squared and the local f-squared of each regressor.
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
