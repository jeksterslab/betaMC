# Summary Method for an Object of Class `mc`

Summary Method for an Object of Class `mc`

## Usage

``` r
# S3 method for class 'mc'
summary(object, digits = 4, ...)
```

## Arguments

- object:

  Object of Class `mc`, that is, the output of the
  [`MC()`](https://jeksterslab.github.io/betaMC/reference/MC.md)
  function.

- digits:

  Digits to print.

- ...:

  additional arguments.

## Value

Returns a list with the following elements:

- mean:

  Mean of the sampling distribution of \\(\boldsymbol{\hat{\theta}}\\).

- var:

  Variance of the sampling distribution of
  \\(\boldsymbol{\hat{\theta}}\\).

- bias:

  Monte Carlo simulation bias.

- rmse:

  Monte Carlo simulation root mean square error.

- location:

  Location parameter used in the Monte Carlo simulation.

- scale:

  Scale parameter used in the Monte Carlo simulation.

## Author

Ivan Jacob Agaloos Pesigan

## Examples

``` r
# Fit the regression model
object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
mc <- MC(object, R = 100)
summary(mc)
#> MC(object = object, R = 100)
#> $mean
#>        b1        b2        b3   sigmasq sigmax1x1 sigmax2x1 sigmax3x1 sigmax2x2 
#>    0.0836    0.2126    0.1040   22.0565 3591.1370  459.2090  499.9337  331.7607 
#> sigmax3x2 sigmax3x3 
#>  148.2039  559.1090 
#> 
#> $var
#>                b1      b2      b3   sigmasq    sigmax1x1  sigmax2x1   sigmax3x1
#> b1         0.0003 -0.0003 -0.0002   -0.0138      -7.1309    -0.4233      0.2996
#> b2        -0.0003  0.0029 -0.0007    0.0254      -1.8341     1.2337     -3.0375
#> b3        -0.0002 -0.0007  0.0015   -0.0264       7.7469     0.2285      2.0885
#> sigmasq   -0.0138  0.0254 -0.0264   14.7450    -744.1175   -72.1677   -184.0590
#> sigmax1x1 -7.1309 -1.8341  7.7469 -744.1175 1164887.6109 76134.4050 109349.0895
#> sigmax2x1 -0.4233  1.2337  0.2285  -72.1677   76134.4050 33368.4880  21626.0332
#> sigmax3x1  0.2996 -3.0375  2.0885 -184.0590  109349.0895 21626.0332  42950.0972
#> sigmax2x2  0.1115 -1.3831  0.7604  -26.7643    5782.2073  1758.4006   3327.8418
#> sigmax3x2 -0.2064  0.4140  0.5998  -42.8063    3235.2158  7041.9195   6190.6498
#> sigmax3x3  0.3464  1.0605 -1.0526    1.0873  -14477.3034  1670.5194   2670.0097
#>           sigmax2x2 sigmax3x2   sigmax3x3
#> b1           0.1115   -0.2064      0.3464
#> b2          -1.3831    0.4140      1.0605
#> b3           0.7604    0.5998     -1.0526
#> sigmasq    -26.7643  -42.8063      1.0873
#> sigmax1x1 5782.2073 3235.2158 -14477.3034
#> sigmax2x1 1758.4006 7041.9195   1670.5194
#> sigmax3x1 3327.8418 6190.6498   2670.0097
#> sigmax2x2 5709.1304 1262.8373   -908.2115
#> sigmax3x2 1262.8373 3895.9783   1405.2301
#> sigmax3x3 -908.2115 1405.2301   7457.0195
#> 
#> $bias
#>        b1        b2        b3   sigmasq sigmax1x1 sigmax2x1 sigmax3x1 sigmax2x2 
#>   -0.0006   -0.0034   -0.0086    0.8117   83.9679  -11.9968  -10.6093   -1.4687 
#> sigmax3x2 sigmax3x3 
#>   -2.7082    4.6703 
#> 
#> $rmse
#>        b1        b2        b3   sigmasq sigmax1x1 sigmax2x1 sigmax3x1 sigmax2x2 
#>    0.0163    0.0539    0.0391    3.9059 1077.1673  182.1503  206.4780   75.1944 
#> sigmax3x2 sigmax3x3 
#>   62.1639   86.0480 
#> 
#> $location
#>        b1        b2        b3   sigmasq sigmax1x1 sigmax2x1 sigmax3x1 sigmax2x2 
#>    0.0842    0.2160    0.1126   21.2448 3507.1691  471.2058  510.5430  333.2295 
#> sigmax3x2 sigmax3x3 
#>  150.9121  554.4386 
#> 
#> $scale
#>                b1      b2      b3   sigmasq    sigmax1x1  sigmax2x1   sigmax3x1
#> b1         0.0002 -0.0003 -0.0002   -0.0073      -6.4514    -0.1818      0.0793
#> b2        -0.0003  0.0027 -0.0006    0.0097       5.4783     0.6385     -1.2960
#> b3        -0.0002 -0.0006  0.0015   -0.0510       4.7717    -0.4470      1.1153
#> sigmasq   -0.0073  0.0097 -0.0510   15.5891    -623.3795   -69.6223   -115.0921
#> sigmax1x1 -6.4514  5.4783  4.7717 -623.3795 1234077.9191 70017.7837 135353.8871
#> sigmax2x1 -0.1818  0.6385 -0.4470  -69.6223   70017.7837 32380.2229  19033.6847
#> sigmax3x1  0.0793 -1.2960  1.1153 -115.0921  135353.8871 19033.6847  43294.1991
#> sigmax2x2  0.1241 -1.2801  0.5313   11.0921    -161.6877  2275.8820   3152.0325
#> sigmax3x2 -0.1904  0.6456  0.2802  -18.6658   10148.7795  6598.7503   5669.9978
#> sigmax3x3  0.2132  0.6850 -0.9885   42.7723   -8922.3944   920.0158   1333.8790
#>            sigmax2x2  sigmax3x2  sigmax3x3
#> b1            0.1241    -0.1904     0.2132
#> b2           -1.2801     0.6456     0.6850
#> b3            0.5313     0.2802    -0.9885
#> sigmasq      11.0921   -18.6658    42.7723
#> sigmax1x1  -161.6877 10148.7795 -8922.3944
#> sigmax2x1  2275.8820  6598.7503   920.0158
#> sigmax3x1  3152.0325  5669.9978  1333.8790
#> sigmax2x2  5980.8603  1092.1986 -1134.2969
#> sigmax3x2  1092.1986  3700.7183   704.7217
#> sigmax3x3 -1134.2969   704.7217  7350.2416
#> 
```
