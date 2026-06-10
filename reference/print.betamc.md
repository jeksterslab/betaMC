# Print Method for an Object of Class `betamc`

Print Method for an Object of Class `betamc`

## Usage

``` r
# S3 method for class 'betamc'
print(x, alpha = NULL, digits = 4, ...)
```

## Arguments

- x:

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
  the argument `alpha` used in `x`.

- digits:

  Digits to print.

- ...:

  additional arguments.

## Value

Prints a matrix of estimates, standard errors, number of Monte Carlo
replications, and confidence intervals.

## Author

Ivan Jacob Agaloos Pesigan
