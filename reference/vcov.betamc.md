# Sampling Variance-Covariance Matrix Method for an Object of Class `betamc`

Sampling Variance-Covariance Matrix Method for an Object of Class
`betamc`

## Usage

``` r
# S3 method for class 'betamc'
vcov(object, ...)
```

## Arguments

- object:

  Object of Class `betamc`, that is, the output of the
  [`BetaMC()`](https://github.com/jeksterslab/betaMC/reference/BetaMC.md),
  [`RSqMC()`](https://github.com/jeksterslab/betaMC/reference/RSqMC.md),
  [`SCorMC()`](https://github.com/jeksterslab/betaMC/reference/SCorMC.md),
  [`DeltaRSqMC()`](https://github.com/jeksterslab/betaMC/reference/DeltaRSqMC.md),
  [`PCorMC()`](https://github.com/jeksterslab/betaMC/reference/PCorMC.md),
  or
  [`DiffBetaMC()`](https://github.com/jeksterslab/betaMC/reference/DiffBetaMC.md)
  functions.

- ...:

  additional arguments.

## Value

Returns the variance-covariance matrix of estimates.

## Author

Ivan Jacob Agaloos Pesigan
