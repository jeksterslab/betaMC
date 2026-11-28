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
#>    0.0839    0.2192    0.1139   21.0913 3548.2780  462.2844  504.0546  313.6647 
#> sigmax3x2 sigmax3x3 
#>  148.7661  557.9799 
#> 
#> $var
#>                b1      b2      b3   sigmasq    sigmax1x1  sigmax2x1  sigmax3x1
#> b1         0.0002 -0.0002 -0.0002   -0.0004      -7.2054    -0.4426    -0.3360
#> b2        -0.0002  0.0024 -0.0008    0.0088      -3.0394     1.1979    -1.2937
#> b3        -0.0002 -0.0008  0.0019   -0.0652       7.9420    -0.0909     2.3301
#> sigmasq   -0.0004  0.0088 -0.0652   15.5390    -646.1435   -42.7301   -86.0710
#> sigmax1x1 -7.2054 -3.0394  7.9420 -646.1435 1049387.4796 65823.7433 84730.5908
#> sigmax2x1 -0.4426  1.1979 -0.0909  -42.7301   65823.7433 33357.9361 17734.4529
#> sigmax3x1 -0.3360 -1.2937  2.3301  -86.0710   84730.5908 17734.4529 29137.5133
#> sigmax2x2 -0.0203 -1.3053  0.7897   48.9100   -4883.6008   451.5634  1239.3124
#> sigmax3x2 -0.2206  0.7784  0.6081  -36.1227    4469.4890  7373.0067  5532.3409
#> sigmax3x3  0.3182  1.4102 -1.2707   34.6614  -17758.2448  -516.3021  -565.9538
#>            sigmax2x2 sigmax3x2   sigmax3x3
#> b1           -0.0203   -0.2206      0.3182
#> b2           -1.3053    0.7784      1.4102
#> b3            0.7897    0.6081     -1.2707
#> sigmasq      48.9100  -36.1227     34.6614
#> sigmax1x1 -4883.6008 4469.4890 -17758.2448
#> sigmax2x1   451.5634 7373.0067   -516.3021
#> sigmax3x1  1239.3124 5532.3409   -565.9538
#> sigmax2x2  5577.6019  583.6393  -2535.7470
#> sigmax3x2   583.6393 4116.0462   -154.2766
#> sigmax3x3 -2535.7470 -154.2766   9077.2966
#> 
#> $bias
#>        b1        b2        b3   sigmasq sigmax1x1 sigmax2x1 sigmax3x1 sigmax2x2 
#>   -0.0003    0.0032    0.0013   -0.1535   41.1089   -8.9214   -6.4883  -19.5647 
#> sigmax3x2 sigmax3x3 
#>   -2.1460    3.5412 
#> 
#> $rmse
#>        b1        b2        b3   sigmasq sigmax1x1 sigmax2x1 sigmax3x1 sigmax2x2 
#>    0.0132    0.0492    0.0429    3.9252 1020.0900  181.9449  169.9654   76.8414 
#> sigmax3x2 sigmax3x3 
#>   63.8709   94.8634 
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
