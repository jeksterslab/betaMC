# Benchmark: Comparing the Monte Carlo Method with Nonparametric Bootstrapping

We compare the Monte Carlo (MC) method with nonparametric bootstrapping
(NB) for standardized regression coefficients. In this example, we use
the data set and the model used in [betaMC: Example Using the BetaMC
Function](https://github.com/jeksterslab/betaMC/articles/example-beta-mc.md).

\
[`library`](https://rdrr.io/r/base/library.html)`(`[`betaMC`](https://github.com/jeksterslab/betaMC)`)`\
[`library`](https://rdrr.io/r/base/library.html)`(``boot``)`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`microbenchmark`](https://github.com/joshuaulrich/microbenchmark/)`)`

The
[`BetaMC()`](https://github.com/jeksterslab/betaMC/reference/BetaMC.md)
function is used to generate MC confidence intervals. The `BetaNB()`
function is used to generate NB confidence intervals.

\
`BetaNB`` ``<-`` ``function``(``formula``, ``data``, ``B``)`` ``{`\
`  ``statistic`` ``<-`` ``function``(``formula``, ``data``, ``indices``)`` ``{`\
`    `[`return`](https://rdrr.io/r/base/function.html)`(`\
`      `[`coef`](https://rdrr.io/r/stats/coef.html)`(`[`lm`](https://rdrr.io/r/stats/lm.html)`(``formula ``=`` ``formula``, data ``=`` `[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html)`(`[`scale`](https://rdrr.io/r/base/scale.html)`(``data``[``indices``, ``]``)``)``)``)``[``-``1``]`\
`    ``)`\
`  ``}`\
`  `[`return`](https://rdrr.io/r/base/function.html)`(`[`boot.ci`](https://rdrr.io/pkg/boot/man/boot.ci.html)`(`[`boot`](https://rdrr.io/pkg/boot/man/boot.html)`(``data ``=`` ``data``, statistic ``=`` ``statistic``, formula ``=`` ``formula``, R ``=`` ``B``)``)``)`\
`}`

## Data and Model

\
`df`` ``<-`` ``betaMC``::`[`nas1982`](https://github.com/jeksterslab/betaMC/reference/nas1982.md)

## Benchmark

### Arguments

| Variables | Values | Notes                               |
|:----------|:-------|:------------------------------------|
| R         | 5000   | Number of Monte Carlo replications. |
| B         | 5000   | Number of bootstrap samples.        |

\
`benchmark`` ``<-`` `[`microbenchmark`](https://rdrr.io/pkg/microbenchmark/man/microbenchmark.html)`(`\
`  MC ``=`` ``{`\
`    ``formula`` ``<-`` ``"QUALITY ~ NARTIC + PCTGRT + PCTSUPP"`\
`    ``object`` ``<-`` `[`lm`](https://rdrr.io/r/stats/lm.html)`(``formula ``=`` ``formula``, data ``=`` ``df``)`\
`    ``mc`` ``<-`` `[`MC`](https://github.com/jeksterslab/betaMC/reference/MC.md)`(``object ``=`` ``object``, R ``=`` ``R``, type ``=`` ``"mvn"``)`\
`    `[`BetaMC`](https://github.com/jeksterslab/betaMC/reference/BetaMC.md)`(``object ``=`` ``mc``)`\
`  ``}``,`\
`  NB ``=`` ``{`\
`    ``formula`` ``<-`` ``"QUALITY ~ NARTIC + PCTGRT + PCTSUPP"`\
`    ``object`` ``<-`` `[`lm`](https://rdrr.io/r/stats/lm.html)`(``formula ``=`` ``formula``, data ``=`` ``df``)`\
`    ``BetaNB``(``formula ``=`` ``formula``, data ``=`` ``df``, B ``=`` ``B``)`\
`  ``}``,`\
`  times ``=`` ``10`\
`)`

### Summary of Benchmark Results

\
[`summary`](https://rdrr.io/r/base/summary.html)`(``benchmark``, unit ``=`` ``"ms"``)`\
`#>   expr        min         lq       mean     median         uq        max neval`\
`#> 1   MC   10.82229   11.06452   11.45689   11.13623   11.28208   14.16231    10`\
`#> 2   NB 1956.19441 1980.22046 1998.34086 1995.74770 2011.37966 2045.64165    10`

### Summary of Benchmark Results Relative to the Faster Method

\
[`summary`](https://rdrr.io/r/base/summary.html)`(``benchmark``, unit ``=`` ``"relative"``)`\
`#>   expr      min       lq     mean   median       uq      max neval`\
`#> 1   MC   1.0000   1.0000   1.0000   1.0000   1.0000   1.0000    10`\
`#> 2   NB 180.7561 178.9702 174.4226 179.2122 178.2809 144.4426    10`

## Plot

![](fig-vignettes-benchmark-unnamed-chunk-15-1.png)
