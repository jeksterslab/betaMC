# Print Method for an Object of Class `mc`

Print Method for an Object of Class `mc`

## Usage

``` r
# S3 method for class 'mc'
print(x, ...)
```

## Arguments

- x:

  Object of Class `mc`.

- ...:

  additional arguments.

## Value

Prints the first set of simulated parameter estimates and model-implied
covariance matrix.

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
mc <- MC(object = object, R = 100)

## Methods -----------------------------------------------------------------
print(x = mc)
#> Call:
#> MC(object = object, R = 100)
#> The first set of simulated parameter estimates
#> and model-implied covariance matrix.
#> 
#> $thetahatstar
#>  [1] 9.426435e-02 2.839037e-01 1.033200e-01 1.748592e+01 3.785957e+03
#>  [6] 5.737654e+02 5.955459e+02 3.105671e+02 1.594021e+02 6.161660e+02
#> 
#> $sigmacap
#>          [,1]      [,2]     [,3]     [,4]
#> [1,] 134.3989  581.3067 158.7262 165.0559
#> [2,] 581.3067 3785.9575 573.7654 595.5459
#> [3,] 158.7262  573.7654 310.5671 159.4021
#> [4,] 165.0559  595.5459 159.4021 616.1660
#> 
```
