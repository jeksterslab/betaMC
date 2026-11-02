# betaMC: Monte Carlo Method Combined with Multiple Imputation

The Monte Carlo Method combined with multiple imputation described in
(**Pesigan-Cheung-2023?**) is implemented using the
[`MCMI()`](https://github.com/jeksterslab/betaMC/reference/MCMI.md)
function in the context of linear regression. Multiple imputation is
used to deal with missing values in a data set. The vector of parameter
estimates and the corresponding sampling covariance matrix are estimated
for each of the imputed data sets. Results are combined to arrive at the
pooled vector of parameter estimates and the corresponding sampling
covariance matrix. The pooled estimates are then used to generate the
sampling distribution of regression parameters. The output of the
[`MCMI()`](https://github.com/jeksterslab/betaMC/reference/MCMI.md)
function can be passed to the
[`BetaMC()`](https://github.com/jeksterslab/betaMC/reference/BetaMC.md),
[`RSqMC()`](https://github.com/jeksterslab/betaMC/reference/RSqMC.md),
[`DeltaRSqMC()`](https://github.com/jeksterslab/betaMC/reference/DeltaRSqMC.md),
[`SCorMC()`](https://github.com/jeksterslab/betaMC/reference/SCorMC.md),
[`PCorMC()`](https://github.com/jeksterslab/betaMC/reference/PCorMC.md),
and
[`DiffBetaMC()`](https://github.com/jeksterslab/betaMC/reference/DiffBetaMC.md)
functions to generate confidence intervals for various regression effect
sizes.

In this example, we use the data set and the model used in [betaMC:
Example Using the BetaMC
Function](https://github.com/jeksterslab/betaMC/articles/example-beta-mc.md).
We use the
[`mice::ampute()`](https://amices.org/mice/reference/ampute.html)
function to generate a data set with missing values.

\
[`library`](https://rdrr.io/r/base/library.html)`(`[`betaMC`](https://github.com/jeksterslab/betaMC)`)`

\
`df`` ``<-`` ``betaMC``::`[`nas1982`](https://github.com/jeksterslab/betaMC/reference/nas1982.md)

\
[`set.seed`](https://rdrr.io/r/base/Random.html)`(``42``)`\
`df`` ``<-`` ``mice``::`[`ampute`](https://amices.org/mice/reference/ampute.html)`(``df``)``$``amp`\
`df`\
`#>    QUALITY NFACUL NGRADS PCTSUPP PCTGRT NARTIC PCTPUB`\
`#> 1       12     13     19      16      8     14     39`\
`#> 2       23     29     72      67      3     61     66`\
`#> 3       29     38    111      NA     13     68     68`\
`#> 4       36     16     28      52     63     49     75`\
`#> 5       44     40    104      64     53     NA     83`\
`#> 6       21     14     28      59     29     65     79`\
`#> 7       40     44     16      81     35     79     82`\
`#> 8       42     NA     57      65     40    187     82`\
`#> 9       24     16     18      87     19     NA     75`\
`#> 10      30     37     41      43      8     NA     54`\
`#> 11      20     20     45      26     25     49     50`\
`#> 12       8     11     27       7      0      9     27`\
`#> 13      NA     29    112      64     35     65     69`\
`#> 14      14     14     57      10      0     11     43`\
`#> 15      27     38    167      28     NA    196     84`\
`#> 16      46     27    113      62     52    173     85`\
`#> 17      NA     32    122      51     19     79     69`\
`#> 18      42     NA    116      56     32    208     73`\
`#> 19      33     32     54      49     19    120     69`\
`#> 20      31     42     79      41     NA    114     71`\
`#> 21      23     30     76      22     20     87     67`\
`#> 22      18     NA     62      39      6     10     39`\
`#> 23      29     41     98      41     12    101     66`\
`#> 24      21     23     52      33      4     59     78`\
`#> 25      45     NA    222      64     32    274     70`\
`#> 26      25     26     63      39     NA    160     89`\
`#> 27      18     16     24       4     31     39     63`\
`#> 28      NA     38    154      55     34     84     63`\
`#> 29      21     19     40       7      5     60     84`\
`#> 30      24     16     18      25     63     31     63`\
`#> 31      15     13     29      23     15     62     85`\
`#> 32      15     23     41      51      4     24     NA`\
`#> 33      36     32     69      65     16    122     75`\
`#> 34      38     21     38      28     48     92     91`\
`#> 35      32     28     90      70     36    117     61`\
`#> 36      27     22     52      10     27    114     86`\
`#> 37      16     20     80      46     10     19     40`\
`#> 38      26     32     41      13      6     64     56`\
`#> 39      NA     26     81      70     58    155    100`\
`#> 40      26     40     81      42     10     70     68`\
`#> 41      14     19     87      15      5     72     79`\
`#> 42      12     17     26       9      6     15     59`\
`#> 43      29     29     71      74     17     85     76`\
`#> 44      34     27     20       0     29     79     57`\
`#> 45      28     26     70      68     27     84     73`\
`#> 46      NA     36     59      57     67    172     83`

### Multiple Imputation

\
`mi`` ``<-`` ``mice``::`[`mice`](https://amices.org/mice/reference/mice.html)`(`\
`  ``df``,`\
`  m ``=`` ``100``,`\
`  seed ``=`` ``42``,`\
`  print ``=`` ``FALSE`\
`)`

### Regression

Fit the regression model using the
[`lm()`](https://rdrr.io/r/stats/lm.html) function. Note that this does
not deal with missing values. The fitted model (`object`) is updated
with each imputed data within the
[`MCMI()`](https://github.com/jeksterslab/betaMC/reference/MCMI.md)
function.

\
`object`` ``<-`` `[`lm`](https://rdrr.io/r/stats/lm.html)`(``QUALITY`` ``~`` ``NARTIC`` ``+`` ``PCTGRT`` ``+`` ``PCTSUPP``, data ``=`` ``df``)`

### Monte Carlo Sampling Distribution of Parameters

\
`hc3`` ``<-`` `[`MCMI`](https://github.com/jeksterslab/betaMC/reference/MCMI.md)`(``object``, mi ``=`` ``mi``, type ``=`` ``"hc3"``)`

### Standardized Regression Slopes

\
[`BetaMC`](https://github.com/jeksterslab/betaMC/reference/BetaMC.md)`(``hc3``)`\
`#> Call:`\
`#> BetaMC(object = hc3)`\
`#> `\
`#> Standardized regression slopes`\
`#> type = "hc3"`\
`#>            est     se     R   0.05%    0.5%   2.5%  97.5%  99.5% 99.95%`\
`#> NARTIC  0.5197 0.0787 20000  0.2380  0.3003 0.3513 0.6582 0.7024 0.7480`\
`#> PCTGRT  0.4017 0.0858 20000  0.1035  0.1707 0.2277 0.5626 0.6112 0.6789`\
`#> PCTSUPP 0.2327 0.0908 20000 -0.0872 -0.0165 0.0459 0.4020 0.4557 0.5277`

### Multiple Correlation Coefficients

\
[`RSqMC`](https://github.com/jeksterslab/betaMC/reference/RSqMC.md)`(``hc3``)`\
`#> Call:`\
`#> RSqMC(object = hc3)`\
`#> `\
`#> R-squared and adjusted R-squared`\
`#> type = "hc3"`\
`#>        est     se     R  0.05%   0.5%   2.5%  97.5%  99.5% 99.95%`\
`#> rsq 0.8154 0.0613 20000 0.4924 0.5924 0.6573 0.8983 0.9235 0.9469`\
`#> adj 0.8022 0.0674 20000 0.4416 0.5517 0.6230 0.8881 0.9159 0.9416`

### Improvement in R-squared

\
[`DeltaRSqMC`](https://github.com/jeksterslab/betaMC/reference/DeltaRSqMC.md)`(``hc3``)`\
`#> Call:`\
`#> DeltaRSqMC(object = hc3)`\
`#> `\
`#> Improvement in R-squared`\
`#> type = "hc3"`\
`#>            est     se     R  0.05%   0.5%   2.5%  97.5%  99.5% 99.95%`\
`#> NARTIC  0.2070 0.0717 20000 0.0029 0.0246 0.0622 0.3452 0.4051 0.4774`\
`#> PCTGRT  0.1270 0.0612 20000 0.0014 0.0108 0.0273 0.2652 0.3209 0.4105`\
`#> PCTSUPP 0.0435 0.0347 20000 0.0000 0.0001 0.0019 0.1307 0.1753 0.2537`

### Semipartial Correlation Coefficients

\
[`SCorMC`](https://github.com/jeksterslab/betaMC/reference/SCorMC.md)`(``hc3``)`\
`#> Call:`\
`#> SCorMC(object = hc3)`\
`#> `\
`#> Semipartial correlations`\
`#> type = "hc3"`\
`#>            est     se     R   0.05%    0.5%   2.5%  97.5%  99.5% 99.95%`\
`#> NARTIC  0.4549 0.0856 20000  0.0538  0.1569 0.2494 0.5875 0.6365 0.6909`\
`#> PCTGRT  0.3564 0.0887 20000  0.0379  0.1040 0.1652 0.5150 0.5665 0.6407`\
`#> PCTSUPP 0.2085 0.0820 20000 -0.0818 -0.0146 0.0401 0.3615 0.4187 0.5037`

### Squared Partial Correlation Coefficients

\
[`PCorMC`](https://github.com/jeksterslab/betaMC/reference/PCorMC.md)`(``hc3``)`\
`#> Call:`\
`#> PCorMC(object = hc3)`\
`#> `\
`#> Squared partial correlations`\
`#> type = "hc3"`\
`#>            est     se     R  0.05%   0.5%   2.5%  97.5%  99.5% 99.95%`\
`#> NARTIC  0.5289 0.1165 20000 0.0104 0.1010 0.2268 0.6859 0.7413 0.8014`\
`#> PCTGRT  0.4082 0.1218 20000 0.0060 0.0523 0.1242 0.5944 0.6614 0.7348`\
`#> PCTSUPP 0.1917 0.1150 20000 0.0000 0.0004 0.0083 0.4380 0.5289 0.6313`

### Differences of Standardized Regression Slopes

\
[`DiffBetaMC`](https://github.com/jeksterslab/betaMC/reference/DiffBetaMC.md)`(``hc3``)`\
`#> Call:`\
`#> DiffBetaMC(object = hc3)`\
`#> `\
`#> Differences of standardized regression slopes`\
`#> type = "hc3"`\
`#>                   est    se     R   0.05%    0.5%    2.5%  97.5%  99.5% 99.95%`\
`#> NARTIC-PCTGRT  0.1179 0.144 20000 -0.3728 -0.2603 -0.1686 0.3908 0.4787 0.5991`\
`#> NARTIC-PCTSUPP 0.2869 0.138 20000 -0.1638 -0.0734  0.0077 0.5514 0.6357 0.7383`\
`#> PCTGRT-PCTSUPP 0.1690 0.143 20000 -0.3142 -0.2036 -0.1092 0.4455 0.5407 0.6334`

## References

Dudgeon, P. (2017). Some improvements in confidence intervals for
standardized regression coefficients. *Psychometrika*, *82*(4), 928–951.
<https://doi.org/10.1007/s11336-017-9563-z>
