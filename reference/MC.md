# Generate the Sampling Distribution of Regression Parameters Using the Monte Carlo Method

Generate the Sampling Distribution of Regression Parameters Using the
Monte Carlo Method

## Usage

``` r
MC(
  object,
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

  Sampling distribution of parameter estimates (a matrix, one draw per
  row).

- vechsigmacap:

  Sampling distribution of the half-vectorization of the model-implied
  covariance matrix (a matrix, one draw per row).

- es:

  Sampling distribution of the effect sizes (a list of matrices).

- fun:

  Function used ("MC").

## Details

Let the parameter vector of the unstandardized regression model be given
by \$\$ \boldsymbol{\theta} = \left\\{ \boldsymbol{b}, \sigma^{2},
\operatorname{vech} \left(
\boldsymbol{\Sigma}\_{\boldsymbol{X}\boldsymbol{X}} \right) \right\\}
\$\$ where \\(\boldsymbol{b}\\) is the vector of regression slopes,
\\(\sigma^{2}\\) is the error variance, and \\( \operatorname{vech}
\left( \boldsymbol{\Sigma}\_{\boldsymbol{X}\boldsymbol{X}} \right) \\)
is the vector of unique elements of the covariance matrix of the
regressor variables. The empirical sampling distribution of
\\(\boldsymbol{\theta}\\) is generated using the Monte Carlo method,
that is, random values of parameter estimates are sampled from the
multivariate normal distribution using the estimated parameter vector as
the mean vector and the specified sampling covariance matrix using the
`type` argument as the covariance matrix. A replacement sampling
approach is implemented to ensure that the model-implied covariance
matrix is positive definite.

## References

Dudgeon, P. (2017). Some improvements in confidence intervals for
standardized regression coefficients. *Psychometrika*, *82*(4), 928–951.
[doi:10.1007/s11336-017-9563-z](https://doi.org/10.1007/s11336-017-9563-z)

MacKinnon, D. P., Lockwood, C. M., & Williams, J. (2004). Confidence
limits for the indirect effect: Distribution of the product and
resampling methods. *Multivariate Behavioral Research*, *39*(1), 99–128.
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
[`BetaMC()`](https://jeksterslab.github.io/betaMC/reference/BetaMC.md),
[`DeltaRSqMC()`](https://jeksterslab.github.io/betaMC/reference/DeltaRSqMC.md),
[`DiffBetaMC()`](https://jeksterslab.github.io/betaMC/reference/DiffBetaMC.md),
[`FSqMC()`](https://jeksterslab.github.io/betaMC/reference/FSqMC.md),
[`MCMI()`](https://jeksterslab.github.io/betaMC/reference/MCMI.md),
[`PCorMC()`](https://jeksterslab.github.io/betaMC/reference/PCorMC.md),
[`RSqMC()`](https://jeksterslab.github.io/betaMC/reference/RSqMC.md),
[`SCorMC()`](https://jeksterslab.github.io/betaMC/reference/SCorMC.md),
[`SCorSqMC()`](https://jeksterslab.github.io/betaMC/reference/SCorSqMC.md)

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
mc
#> Call:
#> MC(object = object, R = 100, seed = 508)
#> The first set of simulated parameter estimates
#> and model-implied covariance matrix.
#> 
#> $thetahatstar
#>  [1] 9.001664e-02 1.639685e-01 1.216022e-01 1.976124e+01 3.798801e+03
#>  [6] 5.987625e+02 5.926223e+02 3.852861e+02 2.052789e+02 5.766830e+02
#> 
#> $sigmacap
#>          [,1]      [,2]     [,3]     [,4]
#> [1,] 108.2644  512.1977 142.0357 157.1310
#> [2,] 512.1977 3798.8014 598.7625 592.6223
#> [3,] 142.0357  598.7625 385.2861 205.2789
#> [4,] 157.1310  592.6223 205.2789 576.6830
#> 
# The `mc` object can be passed as the first argument
# to the following functions
#   - BetaMC
#   - DeltaRSqMC
#   - DiffBetaMC
#   - FSqMC
#   - PCorMC
#   - RSqMC
#   - SCorMC
#   - SCorSqMC
```
