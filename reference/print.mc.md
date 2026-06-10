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
#> [1] 0.10079529 0.27133940 0.07363866
#> 
#> $sigmasq
#> [1] 17.48585
#> 
#> $vechsigmacapx
#> [1] 3785.9575  573.7654  595.5459  310.5671  159.4021  616.1660
#> 
#> $sigmacapx
#>           [,1]     [,2]     [,3]
#> [1,] 3785.9575 573.7654 595.5459
#> [2,]  573.7654 310.5671 159.4021
#> [3,]  595.5459 159.4021 616.1660
#> 
#> $sigmaysq
#> [1] 128.7523
#> 
#> $sigmayx
#> [1] 581.1470 153.8401 148.6539
#> 
#> $sigmacap
#>          [,1]      [,2]     [,3]     [,4]
#> [1,] 128.7523  581.1470 153.8401 148.6539
#> [2,] 581.1470 3785.9575 573.7654 595.5459
#> [3,] 153.8401  573.7654 310.5671 159.4021
#> [4,] 148.6539  595.5459 159.4021 616.1660
#> 
#> $pd
#> [1] TRUE
#> 
```
