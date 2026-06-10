# Confidence Intervals Method for an Object of Class `betamc`

Confidence Intervals Method for an Object of Class `betamc`

## Usage

``` r
# S3 method for class 'betamc'
confint(object, parm = NULL, level = 0.95, ...)
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

- parm:

  a specification of which parameters are to be given confidence
  intervals, either a vector of numbers or a vector of names. If
  missing, all parameters are considered.

- level:

  the confidence level required.

- ...:

  additional arguments.

## Value

Returns a matrix of confidence intervals.

## Author

Ivan Jacob Agaloos Pesigan
