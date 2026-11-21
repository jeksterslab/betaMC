# betaMC 1.3.5

## Minor

- New `FSqMC()`: Cohen's f-squared and the local f-squared of each
  regressor.
- New `SCorSqMC()`: the image of a signed-semipartial interval under squaring.
- Faster Monte Carlo: the draws are screened and the effect sizes
  computed in C++ (Rcpp, RcppArmadillo), once per `MC()` or `MCMI()`
  call. The draws are the same as before for the same seed.
- The `thetahatstar` elements of `mc` and `betamc` objects are now
  matrices (one draw per row) instead of lists. `mc` objects also hold
  `vechsigmacap` and `es`.
- A draw that fails the admissibility checks 1000 times in a row is
  dropped (earlier versions could loop without end).
- Selective standardization: the arguments `std_y` and `std_x` standardize
  only y or only the named regressors (for example, leave a dummy
  variable in its 0-1 units). The defaults give the same results as
  before.
- The internal copies from linearAlgebra 0.0.5 (a faster duplication matrix
  and its inverse), nBootstrap 0.1.0 (quantiles in C++), strRegression
  0.0.2, and processLM 0.0.11 are current. The pseudo-inverse of the
  duplication matrix is now exact, so results for a fixed seed may differ
  slightly from 1.3.4; the difference is within Monte Carlo error.

## Patch

- `MC()` and `MCMI()` stop with an error for an `lm` fit without an
  intercept, with `weights`, or with an `offset`. These fits were accepted
  before, and the results were wrong.
- `DiffBetaMC()` computes the differences of the standardized slopes
  without a loop.
- Documentation: the `MC()` and `MCMI()` help pages list `FSqMC()` and
  `SCorSqMC()` among the functions that accept their output; `SCorSqMC()`
  joins the Beta Monte Carlo family.
- Documentation: formulas use `\operatorname{}` and `\boldsymbol{}`;
  the undefined `\matnrm{}` macro in the Monte Carlo bias, variance, and
  RMSE help pages is now `\mathrm{}`; the DOI links use `https`.

## Documentation

- The examples name every argument and call `stats::lm()`.
- `DeltaRSqMC()` cites Algina, Keselman, and Penfield (2007, 2008a, 2008b,
  2010), as `DeltaRSqSandwich()` and `DeltaRSqNB()` do.

# betaMC 1.3.4

## Patch

- Minor documentation edits.

# betaMC 1.3.3

## Patch

- Minor edits to methods.

# betaMC 1.3.2

## Patch

- Minor documentation edits.

# betaMC 1.3.1

## Patch

- Minor edits to setting seed.

# betaMC 1.3.0

## Minor

- Added the `MCMI()` function
  to perform  Monte Carlo simulation of parameters
  for data sets with missing values
  using multiple imputation.

# betaMC 1.2.0

## Minor

- The Monte Carlo simulation of parameters
  is done by the `MC()` function.
  The output of the `MC()` function
  is passed to the
  `BetaMC()`, `RSqMC()`, `SCorMC()`, `DeltaRSqMC()`, `PCorMC()`,
  and `DiffBetaMC()` functions
  to generate Monte Carlo confidence intervals
  for various regression effect sizes.

# betaMC 1.0.0

## Major

- And so it begins.
