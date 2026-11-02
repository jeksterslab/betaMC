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
#> $coef
#> [1] 0.07589203 0.16704257 0.15043278
#> 
#> $sigmasq
#> [1] 18.36997
#> 
#> $vechsigmacapx
#> [1] 5093.4452  590.1704  840.8989  312.2785  154.8085  512.0073
#> 
#> $sigmacapx
#>           [,1]     [,2]     [,3]
#> [1,] 5093.4452 590.1704 840.8989
#> [2,]  590.1704 312.2785 154.8085
#> [3,]  840.8989 154.8085 512.0073
#> 
#> $sigmaysq
#> [1] 109.9507
#> 
#> $sigmayx
#> [1] 611.6342 120.2413 166.6998
#> 
#> $sigmacap
#>          [,1]      [,2]     [,3]     [,4]
#> [1,] 109.9507  611.6342 120.2413 166.6998
#> [2,] 611.6342 5093.4452 590.1704 840.8989
#> [3,] 120.2413  590.1704 312.2785 154.8085
#> [4,] 166.6998  840.8989 154.8085 512.0073
#> 
#> $pd
#> [1] TRUE
#> 
```
