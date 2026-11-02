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
  [`MC()`](https://github.com/jeksterslab/betaMC/reference/MC.md)
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
#>    0.0855    0.2192    0.1144   21.0793 3536.5569  466.4309  528.7742  342.2669 
#> sigmax3x2 sigmax3x3 
#>  155.7474  557.1612 
#> 
#> $var
#>                b1      b2      b3   sigmasq    sigmax1x1  sigmax2x1   sigmax3x1
#> b1         0.0002 -0.0004 -0.0001   -0.0079      -7.7167    -0.5720      0.0516
#> b2        -0.0004  0.0029 -0.0005   -0.0004       8.0391    -0.2946     -2.9652
#> b3        -0.0001 -0.0005  0.0013   -0.0584       6.4137     0.2717      1.7175
#> sigmasq   -0.0079 -0.0004 -0.0584   16.9411    -314.3656    34.8260    -74.0046
#> sigmax1x1 -7.7167  8.0391  6.4137 -314.3656 1123104.2354 83493.9236 100323.7300
#> sigmax2x1 -0.5720 -0.2946  0.2717   34.8260   83493.9236 31672.0469  14133.3509
#> sigmax3x1  0.0516 -2.9652  1.7175  -74.0046  100323.7300 14133.3509  31909.1440
#> sigmax2x2  0.0337 -0.9942  0.4918   18.5096    5453.7952  2730.9280   4642.5670
#> sigmax3x2 -0.2628  0.4262  0.1824  -15.3243   11307.9487  5844.6613   4136.8488
#> sigmax3x3  0.2602  0.5481 -0.9642   67.8322  -15249.8855 -1129.7055    255.9731
#>            sigmax2x2  sigmax3x2   sigmax3x3
#> b1            0.0337    -0.2628      0.2602
#> b2           -0.9942     0.4262      0.5481
#> b3            0.4918     0.1824     -0.9642
#> sigmasq      18.5096   -15.3243     67.8322
#> sigmax1x1  5453.7952 11307.9487 -15249.8855
#> sigmax2x1  2730.9280  5844.6613  -1129.7055
#> sigmax3x1  4642.5670  4136.8488    255.9731
#> sigmax2x2  5151.3870  1066.1870  -1859.1171
#> sigmax3x2  1066.1870  2911.9506   -102.7055
#> sigmax3x3 -1859.1171  -102.7055   8272.2350
#> 
#> $bias
#>        b1        b2        b3   sigmasq sigmax1x1 sigmax2x1 sigmax3x1 sigmax2x2 
#>    0.0013    0.0032    0.0018   -0.1655   29.3878   -4.7749   18.2312    9.0374 
#> sigmax3x2 sigmax3x3 
#>    4.8353    2.7225 
#> 
#> $rmse
#>        b1        b2        b3   sigmasq sigmax1x1 sigmax2x1 sigmax3x1 sigmax2x2 
#>    0.0148    0.0533    0.0360    4.0987 1054.8634  177.1387  178.6685   71.9830 
#> sigmax3x2 sigmax3x3 
#>   53.9093   90.5369 
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
