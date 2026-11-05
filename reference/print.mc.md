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
object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
mc <- MC(object, R = 100)
print(mc)
#> Call:
#> MC(object = object, R = 100)
#> The first set of simulated parameter estimates
#> and model-implied covariance matrix.
#> 
#> $thetahatstar
#>  [1] 1.007953e-01 2.713394e-01 7.363866e-02 1.748585e+01 3.785957e+03
#>  [6] 5.737654e+02 5.955459e+02 3.105671e+02 1.594021e+02 6.161660e+02
#> 
#> $sigmacap
#>          [,1]      [,2]     [,3]     [,4]
#> [1,] 128.7523  581.1470 153.8401 148.6539
#> [2,] 581.1470 3785.9575 573.7654 595.5459
#> [3,] 153.8401  573.7654 310.5671 159.4021
#> [4,] 148.6539  595.5459 159.4021 616.1660
#> 
```
