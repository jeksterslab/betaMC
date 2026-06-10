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

  Mean of the sampling distribution of \\\boldsymbol{\hat{\theta}}\\.

- var:

  Variance of the sampling distribution of
  \\\boldsymbol{\hat{\theta}}\\.

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
#>    0.0832    0.2273    0.1142   21.1065 3611.1447  476.0296  517.5181  330.6449 
#> sigmax3x2 sigmax3x3 
#>  150.8569  543.0762 
#> 
#> $var
#>                b1      b2      b3   sigmasq   sigmax1x1  sigmax2x1  sigmax3x1
#> b1         0.0002 -0.0003 -0.0002   -0.0120     -4.2726    -0.4293     0.3883
#> b2        -0.0003  0.0025 -0.0006    0.0141      6.8647    -1.2025    -2.8625
#> b3        -0.0002 -0.0006  0.0015   -0.0663      2.6280     0.2757     1.8956
#> sigmasq   -0.0120  0.0141 -0.0663   19.1385   -268.3510    49.9355  -127.4505
#> sigmax1x1 -4.2726  6.8647  2.6280 -268.3510 871595.5465 68865.0344 95797.0266
#> sigmax2x1 -0.4293 -1.2025  0.2757   49.9355  68865.0344 37344.1407 15439.5289
#> sigmax3x1  0.3883 -2.8625  1.8956 -127.4505  95797.0266 15439.5289 35468.7569
#> sigmax2x2  0.1309 -1.7208  0.9148   -3.3713  -5075.7348  3483.6566  1882.7798
#> sigmax3x2 -0.2104 -0.0873  0.4911  -26.8380   9765.8579  6961.7183  4439.5420
#> sigmax3x3  0.2514  0.2417 -0.8208   28.7762  -6929.1966   -60.4522   252.8172
#>            sigmax2x2 sigmax3x2  sigmax3x3
#> b1            0.1309   -0.2104     0.2514
#> b2           -1.7208   -0.0873     0.2417
#> b3            0.9148    0.4911    -0.8208
#> sigmasq      -3.3713  -26.8380    28.7762
#> sigmax1x1 -5075.7348 9765.8579 -6929.1966
#> sigmax2x1  3483.6566 6961.7183   -60.4522
#> sigmax3x1  1882.7798 4439.5420   252.8172
#> sigmax2x2  5388.2098 1439.8419 -1607.3905
#> sigmax3x2  1439.8419 3113.2530    57.5257
#> sigmax3x3 -1607.3905   57.5257  6116.1742
#> 
#> $bias
#>        b1        b2        b3   sigmasq sigmax1x1 sigmax2x1 sigmax3x1 sigmax2x2 
#>   -0.0010    0.0113    0.0016   -0.1384  103.9756    4.8238    6.9751   -2.5846 
#> sigmax3x2 sigmax3x3 
#>   -0.0552  -11.3625 
#> 
#> $rmse
#>        b1        b2        b3   sigmasq sigmax1x1 sigmax2x1 sigmax3x1 sigmax2x2 
#>    0.0147    0.0513    0.0390    4.3550  934.7141  192.3382  187.5173   73.0822 
#> sigmax3x2 sigmax3x3 
#>   55.5169   78.6392 
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
