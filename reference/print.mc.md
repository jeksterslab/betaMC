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
#>  [1] 7.589203e-02 1.670426e-01 1.504328e-01 1.836997e+01 5.093445e+03
#>  [6] 5.901704e+02 8.408989e+02 3.122785e+02 1.548085e+02 5.120073e+02
#> 
#> $sigmacap
#>          [,1]      [,2]     [,3]     [,4]
#> [1,] 109.9507  611.6342 120.2413 166.6998
#> [2,] 611.6342 5093.4452 590.1704 840.8989
#> [3,] 120.2413  590.1704 312.2785 154.8085
#> [4,] 166.6998  840.8989 154.8085 512.0073
#> 
```
