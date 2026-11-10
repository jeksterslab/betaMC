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
  [`BetaMC()`](https://jeksterslab.github.io/betaMC/reference/BetaMC.md),
  [`RSqMC()`](https://jeksterslab.github.io/betaMC/reference/RSqMC.md),
  [`SCorMC()`](https://jeksterslab.github.io/betaMC/reference/SCorMC.md),
  [`DeltaRSqMC()`](https://jeksterslab.github.io/betaMC/reference/DeltaRSqMC.md),
  [`PCorMC()`](https://jeksterslab.github.io/betaMC/reference/PCorMC.md),
  or
  [`DiffBetaMC()`](https://jeksterslab.github.io/betaMC/reference/DiffBetaMC.md)
  functions.

- ...:

  additional arguments.

## Value

Returns the variance-covariance matrix of estimates.

## Author

Ivan Jacob Agaloos Pesigan
