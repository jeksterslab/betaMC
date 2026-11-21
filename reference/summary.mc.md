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
object <- stats::lm(
  formula = QUALITY ~ NARTIC + PCTGRT + PCTSUPP,
  data = nas1982
)
mc <- MC(object = object, R = 100)
summary(object = mc)
#> MC(object = object, R = 100)
#> $mean
#>        b1        b2        b3   sigmasq sigmax1x1 sigmax2x1 sigmax3x1 sigmax2x2 
#>    0.0806    0.2203    0.1167   22.0774 3553.9014  457.5913  501.1252  325.0540 
#> sigmax3x2 sigmax3x3 
#>  147.4334  557.6951 
#> 
#> $var
#>                b1      b2      b3   sigmasq    sigmax1x1  sigmax2x1   sigmax3x1
#> b1         0.0002 -0.0002 -0.0002    0.0018      -7.1697    -0.1245      0.4425
#> b2        -0.0002  0.0027 -0.0008   -0.0113      -1.2578     1.1880     -3.1806
#> b3        -0.0002 -0.0008  0.0019   -0.0873       5.3607    -1.5479      1.2285
#> sigmasq    0.0018 -0.0113 -0.0873   16.2352    -965.1159   -54.0598   -180.3352
#> sigmax1x1 -7.1697 -1.2578  5.3607 -965.1159 1228865.5327 65193.0016 108523.5897
#> sigmax2x1 -0.1245  1.1880 -1.5479  -54.0598   65193.0016 29555.2443  18565.6060
#> sigmax3x1  0.4425 -3.1806  1.2285 -180.3352  108523.5897 18565.6060  42902.3479
#> sigmax2x2  0.1649 -1.5954  0.2522   54.6705   -5757.9488  1558.4648   2550.5579
#> sigmax3x2 -0.0829  0.2596 -0.0210  -23.8563   -1891.5942  6480.8700   5501.7791
#> sigmax3x3  0.4415  0.8540 -1.2814   54.1576  -20020.2797  2356.6409   2878.6717
#>            sigmax2x2  sigmax3x2   sigmax3x3
#> b1            0.1649    -0.0829      0.4415
#> b2           -1.5954     0.2596      0.8540
#> b3            0.2522    -0.0210     -1.2814
#> sigmasq      54.6705   -23.8563     54.1576
#> sigmax1x1 -5757.9488 -1891.5942 -20020.2797
#> sigmax2x1  1558.4648  6480.8700   2356.6409
#> sigmax3x1  2550.5579  5501.7791   2878.6717
#> sigmax2x2  5738.0427  1013.4447   -186.1841
#> sigmax3x2  1013.4447  3662.3312   1270.4851
#> sigmax3x3  -186.1841  1270.4851   8148.9886
#> 
#> $bias
#>        b1        b2        b3   sigmasq sigmax1x1 sigmax2x1 sigmax3x1 sigmax2x2 
#>   -0.0036    0.0043    0.0041    0.8326   46.7323  -13.6145   -9.4178   -8.1754 
#> sigmax3x2 sigmax3x3 
#>   -3.4787    3.2564 
#> 
#> $rmse
#>        b1        b2        b3   sigmasq sigmax1x1 sigmax2x1 sigmax3x1 sigmax2x2 
#>    0.0150    0.0521    0.0440    4.0946 1103.9750  171.5956  206.3056   75.8123 
#> sigmax3x2 sigmax3x3 
#>   60.3143   89.8783 
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
