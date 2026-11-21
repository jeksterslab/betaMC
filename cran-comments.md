## Release summary

This is a minor update (1.3.4 -> 1.3.5).

- New functions: `FSqMC()` and `SCorSqMC()`.
- Faster Monte Carlo: the draws are screened and the effect sizes are computed in C++ (Rcpp, RcppArmadillo). The draws are the same as before for the same seed.
- Selective standardization through `std_y` and `std_x`.
- The `thetahatstar` elements of `mc` and `betamc` objects are matrices (one draw per row) instead of lists.
- `MC()` and `MCMI()` now stop with an error for an `lm` fit without an intercept, with `weights`, or with an `offset` (these fits gave wrong results before).
- A draw that fails the admissibility checks 1000 times in a row is dropped (earlier versions could loop without end).
- Documentation edits (formulas, links).

## Test environments

- Linux (x86_64, Apptainer image), R 4.6.2 (2026-10-29): `R CMD check --as-cran`, Status: OK (checked twice, on a fresh copy of the project). Test coverage is 95.56%.
- win-builder (R-devel): TODO (the maintainer runs this before submission).
- R-hub: TODO (the maintainer runs this before submission).

## R CMD check results

0 errors | 0 warnings | 0 notes (Linux, R 4.6.2). The win-builder and R-hub results go here (TODO: after the maintainer runs them).

## Reverse dependencies

betaDelta, betaNB, and betaSandwich Suggest betaMC. A search of their `R/`, `tests/`, and `vignettes/` finds no call to a betaMC function or to the `thetahatstar` elements of `mc` objects. (TODO: confirm with `tools::package_dependencies(reverse = TRUE)` before submission.)
