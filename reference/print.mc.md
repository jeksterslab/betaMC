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
object <- stats::lm(
  formula = QUALITY ~ NARTIC + PCTGRT + PCTSUPP,
  data = nas1982
)
mc <- MC(object = object, R = 100)
print(x = mc)
#> Call:
#> MC(object = object, R = 100)
#> The first set of simulated parameter estimates
#> and model-implied covariance matrix.
#> 
#> $thetahatstar
#>  [1] 7.669018e-02 3.110721e-01 1.404036e-01 1.351980e+01 3.364274e+03
#>  [6] 5.506635e+02 3.764402e+02 3.213907e+02 1.270293e+02 3.917781e+02
#> 
#> $sigmacap
#>          [,1]      [,2]     [,3]     [,4]
#> [1,] 117.6055  482.1564 160.0415 123.3916
#> [2,] 482.1564 3364.2744 550.6635 376.4402
#> [3,] 160.0415  550.6635 321.3907 127.0293
#> [4,] 123.3916  376.4402 127.0293 391.7781
#> 
```
