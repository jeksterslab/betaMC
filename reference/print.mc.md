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
#>  [1] 4.918237e-02 2.465495e-01 1.339064e-01 2.249688e+01 4.752946e+03
#>  [6] 3.619041e+02 4.632538e+02 4.015860e+02 9.377988e+01 5.037425e+02
#> 
#> $sigmacap
#>           [,1]      [,2]      [,3]      [,4]
#> [1,]  88.50831  385.0211 129.36788 113.35965
#> [2,] 385.02111 4752.9465 361.90409 463.25378
#> [3,] 129.36788  361.9041 401.58603  93.77988
#> [4,] 113.35965  463.2538  93.77988 503.74253
#> 
```
