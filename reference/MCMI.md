# Generate the Sampling Distribution of Regression Parameters Using the Monte Carlo Method for Data with Missing Values

Generate the Sampling Distribution of Regression Parameters Using the
Monte Carlo Method for Data with Missing Values

## Usage

``` r
MCMI(
  object,
  mi,
  R = 20000L,
  type = "hc3",
  g1 = 1,
  g2 = 1.5,
  k = 0.7,
  decomposition = "eigen",
  pd = TRUE,
  tol = 1e-06,
  fixed_x = FALSE,
  seed = NULL
)
```

## Arguments

- object:

  Object of class `lm`.

- mi:

  Object of class `mids` (output of
  [`mice::mice()`](https://amices.org/mice/reference/mice.html)), object
  of class `amelia` (output of
  [`Amelia::amelia()`](https://rdrr.io/pkg/Amelia/man/amelia.html)), or
  a list of multiply imputed data sets.

- R:

  Positive integer. Number of Monte Carlo replications.

- type:

  Character string. Sampling covariance matrix type. Possible values are
  `"mvn"`, `"adf"`, `"hc0"`, `"hc1"`, `"hc2"`, `"hc3"`, `"hc4"`,
  `"hc4m"`, and `"hc5"`. `type = "mvn"` uses the normal-theory sampling
  covariance matrix. `type = "adf"` uses the asymptotic
  distribution-free sampling covariance matrix. `type = "hc0"` through
  `"hc5"` uses different versions of heteroskedasticity-consistent
  sampling covariance matrix.

- g1:

  Numeric. `g1` value for `type = "hc4m"`.

- g2:

  Numeric. `g2` value for `type = "hc4m"`.

- k:

  Numeric. Constant for `type = "hc5"`

- decomposition:

  Character string. Matrix decomposition of the sampling
  variance-covariance matrix for the data generation. If
  `decomposition = "chol"`, use Cholesky decomposition. If
  `decomposition = "eigen"`, use eigenvalue decomposition. If
  `decomposition = "svd"`, use singular value decomposition.

- pd:

  Logical. If `pd = TRUE`, check if the sampling variance-covariance
  matrix is positive definite using `tol`.

- tol:

  Numeric. Tolerance used for `pd`.

- fixed_x:

  Logical. If `fixed_x = TRUE`, treat the regressors as fixed. If
  `fixed_x = FALSE`, treat the regressors as random.

- seed:

  Integer. Seed number for reproducibility.

## Value

Returns an object of class `mc` which is a list with the following
elements:

- call:

  Function call.

- args:

  Function arguments.

- lm_process:

  Processed `lm` object.

- scale:

  Sampling variance-covariance matrix of parameter estimates.

- location:

  Parameter estimates.

- thetahatstar:

  Sampling distribution of parameter estimates.

- fun:

  Function used ("MCMI").

## Details

Multiple imputation is used to deal with missing values in a data set.
The vector of parameter estimates and the corresponding sampling
covariance matrix are estimated for each of the imputed data sets.
Results are combined to arrive at the pooled vector of parameter
estimates and the corresponding sampling covariance matrix. The pooled
estimates are then used to generate the sampling distribution of
regression parameters. See
[`MC()`](https://github.com/jeksterslab/betaMC/reference/MC.md) for more
details on the Monte Carlo method.

## References

Dudgeon, P. (2017). Some improvements in confidence intervals for
standardized regression coefficients. *Psychometrika*, *82*(4), 928–951.
[doi:10.1007/s11336-017-9563-z](https://doi.org/10.1007/s11336-017-9563-z)

MacKinnon, D. P., Lockwood, C. M., & Williams, J. (2004). Confidence
limits for the indirect effect: Distribution of the product and
resampling methods. *Multivariate Behavioral Research*, *39*(1), 99-128.
[doi:10.1207/s15327906mbr3901_4](https://doi.org/10.1207/s15327906mbr3901_4)

Pesigan, I. J. A., & Cheung, S. F. (2024). Monte Carlo confidence
intervals for the indirect effect with missing data. *Behavior Research
Methods*.
[doi:10.3758/s13428-023-02114-4](https://doi.org/10.3758/s13428-023-02114-4)

Preacher, K. J., & Selig, J. P. (2012). Advantages of Monte Carlo
confidence intervals for indirect effects. *Communication Methods and
Measures*, *6*(2), 77–98.
[doi:10.1080/19312458.2012.679848](https://doi.org/10.1080/19312458.2012.679848)

## See also

Other Beta Monte Carlo Functions:
[`BetaMC()`](https://github.com/jeksterslab/betaMC/reference/BetaMC.md),
[`DeltaRSqMC()`](https://github.com/jeksterslab/betaMC/reference/DeltaRSqMC.md),
[`DiffBetaMC()`](https://github.com/jeksterslab/betaMC/reference/DiffBetaMC.md),
[`MC()`](https://github.com/jeksterslab/betaMC/reference/MC.md),
[`PCorMC()`](https://github.com/jeksterslab/betaMC/reference/PCorMC.md),
[`RSqMC()`](https://github.com/jeksterslab/betaMC/reference/RSqMC.md),
[`SCorMC()`](https://github.com/jeksterslab/betaMC/reference/SCorMC.md)

## Author

Ivan Jacob Agaloos Pesigan

## Examples

``` r
# Data ---------------------------------------------------------------------
data("nas1982", package = "betaMC")
nas1982_missing <- mice::ampute(nas1982)$amp # data set with missing values

# Multiple Imputation
mi <- mice::mice(nas1982_missing, m = 5, seed = 42, print = FALSE)

# Fit Model in lm ----------------------------------------------------------
## Note that this does not deal with missing values.
## The fitted model (`object`) is updated with each imputed data
## within the `MCMI()` function.
object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982_missing)

# Monte Carlo --------------------------------------------------------------
mc <- MCMI(
  object,
  mi = mi,
  R = 100, # use a large value e.g., 20000L for actual research
  seed = 0508
)
mc
#> Call:
#> MCMI(object = object, mi = mi, R = 100, seed = 508)
#> The first set of simulated parameter estimates
#> and model-implied covariance matrix.
#> 
#> $coef
#> [1] 0.08360573 0.18269795 0.15525579
#> 
#> $sigmasq
#> [1] 20.30319
#> 
#> $vechsigmacapx
#> [1] 3712.8505  609.1971  635.5153  332.2336  201.1010  549.9604
#> 
#> $sigmacapx
#>           [,1]     [,2]     [,3]
#> [1,] 3712.8505 609.1971 635.5153
#> [2,]  609.1971 332.2336 201.1010
#> [3,]  635.5153 201.1010 549.9604
#> 
#> $sigmaysq
#> [1] 117.1189
#> 
#> $sigmayx
#> [1] 520.3821 142.8529 175.2580
#> 
#> $sigmacap
#>          [,1]      [,2]     [,3]     [,4]
#> [1,] 117.1189  520.3821 142.8529 175.2580
#> [2,] 520.3821 3712.8505 609.1971 635.5153
#> [3,] 142.8529  609.1971 332.2336 201.1010
#> [4,] 175.2580  635.5153 201.1010 549.9604
#> 
#> $pd
#> [1] TRUE
#> 
# The `mc` object can be passed as the first argument
# to the following functions
#   - BetaMC
#   - DeltaRSqMC
#   - DiffBetaMC
#   - PCorMC
#   - RSqMC
#   - SCorMC
```
