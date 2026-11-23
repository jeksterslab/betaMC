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
summary(object = mc)
#> MC(object = object, R = 100)
#> $mean
#>        b1        b2        b3   sigmasq sigmax1x1 sigmax2x1 sigmax3x1 sigmax2x2 
#>    0.0854    0.2168    0.1106   21.3408 3476.2717  450.7256  481.5844  334.2147 
#> sigmax3x2 sigmax3x3 
#>  147.0346  567.6276 
#> 
#> $var
#>                b1      b2      b3   sigmasq    sigmax1x1  sigmax2x1  sigmax3x1
#> b1         0.0002 -0.0003 -0.0003   -0.0065      -6.6444    -0.1839    -0.0097
#> b2        -0.0003  0.0027 -0.0004    0.0119      -0.8127     0.3889    -1.8574
#> b3        -0.0003 -0.0004  0.0016   -0.0439       6.6713    -0.4521     0.5550
#> sigmasq   -0.0065  0.0119 -0.0439   14.0700    -261.7337   -31.9444   -47.4159
#> sigmax1x1 -6.6444 -0.8127  6.6713 -261.7337 1002375.6304 70779.1704 97434.1521
#> sigmax2x1 -0.1839  0.3889 -0.4521  -31.9444   70779.1704 34955.8570 19769.9746
#> sigmax3x1 -0.0097 -1.8574  0.5550  -47.4159   97434.1521 19769.9746 32646.7883
#> sigmax2x2 -0.0596 -1.2310  0.9009    4.4861    3105.5741  3819.1078  3706.4559
#> sigmax3x2 -0.1879  0.4887  0.5690  -44.7170   10990.3096  8481.8052  7073.8093
#> sigmax3x3  0.4756  1.1752 -1.4617   -6.9351  -10927.5362  1108.7195  1524.7698
#>            sigmax2x2  sigmax3x2   sigmax3x3
#> b1           -0.0596    -0.1879      0.4756
#> b2           -1.2310     0.4887      1.1752
#> b3            0.9009     0.5690     -1.4617
#> sigmasq       4.4861   -44.7170     -6.9351
#> sigmax1x1  3105.5741 10990.3096 -10927.5362
#> sigmax2x1  3819.1078  8481.8052   1108.7195
#> sigmax3x1  3706.4559  7073.8093   1524.7698
#> sigmax2x2  5856.5493  2154.7970  -2393.6214
#> sigmax3x2  2154.7970  4810.1292    307.7547
#> sigmax3x3 -2393.6214   307.7547   9215.1110
#> 
#> $bias
#>        b1        b2        b3   sigmasq sigmax1x1 sigmax2x1 sigmax3x1 sigmax2x2 
#>    0.0011    0.0008   -0.0020    0.0960  -30.8974  -20.4801  -28.9586    0.9852 
#> sigmax3x2 sigmax3x3 
#>   -3.8775   13.1890 
#> 
#> $rmse
#>        b1        b2        b3   sigmasq sigmax1x1 sigmax2x1 sigmax3x1 sigmax2x2 
#>    0.0145    0.0517    0.0401    3.7334  996.6476  187.1516  182.0959   76.1509 
#> sigmax3x2 sigmax3x3 
#>   69.1163   96.4205 
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
