# Summary Method for an Object of Class `betamc`

Summary Method for an Object of Class `betamc`

## Usage

``` r
# S3 method for class 'betamc'
summary(object, alpha = NULL, digits = 4, ...)
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

- alpha:

  Numeric vector. Significance level \\\alpha\\. If `alpha = NULL`, use
  the argument `alpha` used in `object`.

- digits:

  Digits to print.

- ...:

  additional arguments.

## Value

Returns a matrix of estimates, standard errors, number of Monte Carlo
replications, and confidence intervals.

## Author

Ivan Jacob Agaloos Pesigan
