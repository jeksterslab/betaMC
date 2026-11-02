# betaMC

Ivan Jacob Agaloos Pesigan 2026-11-02

## Description

Generates Monte Carlo confidence intervals for standardized regression
coefficients (beta) and other effect sizes, including multiple
correlation, semipartial correlations, improvement in R-squared, squared
partial correlations, and differences in standardized regression
coefficients, for models fitted by
[`lm()`](https://rdrr.io/r/stats/lm.html). `betaMC` combines ideas from
Monte Carlo confidence intervals for the indirect effect (Pesigan and
Cheung, 2024: <http://doi.org/10.3758/s13428-023-02114-4>) and the
sampling covariance matrix of regression coefficients (Dudgeon, 2017:
<http://doi.org/10.1007/s11336-017-9563-z>) to generate confidence
intervals effect sizes in regression.

## Installation

You can install the CRAN release of `betaMC` with:

\
[`install.packages`](https://rdrr.io/r/utils/install.packages.html)`(``"betaMC"``)`

You can install the development version of `betaMC` from
[GitHub](https://github.com/jeksterslab/betaMC) with:

\
`if`` ``(``!`[`require`](https://rdrr.io/r/base/library.html)`(`[`"pak"`](https://pak.r-lib.org/)`)``)`` `[`install.packages`](https://rdrr.io/r/utils/install.packages.html)`(``"pak"``)`\
`pak``::`[`pkg_install`](https://pak.r-lib.org/reference/pkg_install.html)`(``"jeksterslab/betaMC"``)`

## Example

In this example, a multiple regression model is fitted using program
quality ratings (`QUALITY`) as the regressand/outcome variable and
number of published articles attributed to the program faculty members
(`NARTIC`), percent of faculty members holding research grants
(`PCTGRT`), and percentage of program graduates who received support
(`PCTSUPP`) as regressor/predictor variables using a data set from 1982
ratings of 46 doctoral programs in psychology in the USA (National
Research Council, 1982). Confidence intervals for the standardized
regression coefficients are generated using the
[`BetaMC()`](https://github.com/jeksterslab/betaMC/reference/BetaMC.md)
function from the `betaMC` package.

\
[`library`](https://rdrr.io/r/base/library.html)`(`[`betaMC`](https://github.com/jeksterslab/betaMC)`)`

\
`df`` ``<-`` ``betaMC``::`[`nas1982`](https://github.com/jeksterslab/betaMC/reference/nas1982.md)

### Regression

Fit the regression model using the
[`lm()`](https://rdrr.io/r/stats/lm.html) function.

\
`object`` ``<-`` `[`lm`](https://rdrr.io/r/stats/lm.html)`(``QUALITY`` ``~`` ``NARTIC`` ``+`` ``PCTGRT`` ``+`` ``PCTSUPP``, data ``=`` ``df``)`

### Monte Carlo Sampling Distribution of Parameters

#### Normal-Theory Approach

\
`mvn`` ``<-`` `[`MC`](https://github.com/jeksterslab/betaMC/reference/MC.md)`(``object``, type ``=`` ``"mvn"``)`

#### Asymptotic distribution-free Approach

\
`adf`` ``<-`` `[`MC`](https://github.com/jeksterslab/betaMC/reference/MC.md)`(``object``, type ``=`` ``"adf"``)`

#### Heteroskedasticity Consistent Approach (HC3)

\
`hc3`` ``<-`` `[`MC`](https://github.com/jeksterslab/betaMC/reference/MC.md)`(``object``, type ``=`` ``"hc3"``)`

### Standardized Regression Slopes

#### Normal-Theory Approach

\
[`BetaMC`](https://github.com/jeksterslab/betaMC/reference/BetaMC.md)`(``mvn``, alpha ``=`` ``0.05``)`\
`#> Call:`\
`#> BetaMC(object = mvn, alpha = 0.05)`\
`#> `\
`#> Standardized regression slopes`\
`#> type = "mvn"`\
`#>            est     se     R   2.5%  97.5%`\
`#> NARTIC  0.4951 0.0755 20000 0.3384 0.6332`\
`#> PCTGRT  0.3915 0.0766 20000 0.2396 0.5361`\
`#> PCTSUPP 0.2632 0.0746 20000 0.1198 0.4096`

#### Asymptotic distribution-free Approach

\
[`BetaMC`](https://github.com/jeksterslab/betaMC/reference/BetaMC.md)`(``adf``, alpha ``=`` ``0.05``)`\
`#> Call:`\
`#> BetaMC(object = adf, alpha = 0.05)`\
`#> `\
`#> Standardized regression slopes`\
`#> type = "adf"`\
`#>            est     se     R   2.5%  97.5%`\
`#> NARTIC  0.4951 0.0675 20000 0.3545 0.6170`\
`#> PCTGRT  0.3915 0.0708 20000 0.2440 0.5218`\
`#> PCTSUPP 0.2632 0.0766 20000 0.1056 0.4097`

#### Heteroskedasticity Consistent Approach (HC3)

\
[`BetaMC`](https://github.com/jeksterslab/betaMC/reference/BetaMC.md)`(``hc3``, alpha ``=`` ``0.05``)`\
`#> Call:`\
`#> BetaMC(object = hc3, alpha = 0.05)`\
`#> `\
`#> Standardized regression slopes`\
`#> type = "hc3"`\
`#>            est     se     R   2.5%  97.5%`\
`#> NARTIC  0.4951 0.0803 20000 0.3230 0.6362`\
`#> PCTGRT  0.3915 0.0822 20000 0.2190 0.5412`\
`#> PCTSUPP 0.2632 0.0848 20000 0.0887 0.4232`

### Other Effect Sizes

The `betaMC` package also has functions to generate Monte Carlo
confidence intervals for other effect sizes such as
[`RSqMC()`](https://github.com/jeksterslab/betaMC/reference/RSqMC.md)
for multiple correlation coefficients (R-squared and adjusted
R-squared),
[`DeltaRSqMC()`](https://github.com/jeksterslab/betaMC/reference/DeltaRSqMC.md)
for improvement in R-squared,
[`SCorMC()`](https://github.com/jeksterslab/betaMC/reference/SCorMC.md)
for semipartial correlation coefficients,
[`PCorMC()`](https://github.com/jeksterslab/betaMC/reference/PCorMC.md)
for squared partial correlation coefficients, and
[`DiffBetaMC()`](https://github.com/jeksterslab/betaMC/reference/DiffBetaMC.md)
for differences of standardized regression coefficients.

#### Multiple Correlation Coefficients (R-squared and adjusted R-squared)

\
[`RSqMC`](https://github.com/jeksterslab/betaMC/reference/RSqMC.md)`(``hc3``, alpha ``=`` ``0.05``)`\
`#> Call:`\
`#> RSqMC(object = hc3, alpha = 0.05)`\
`#> `\
`#> R-squared and adjusted R-squared`\
`#> type = "hc3"`\
`#>        est     se     R   2.5% 97.5%`\
`#> rsq 0.8045 0.0622 20000 0.6440 0.888`\
`#> adj 0.7906 0.0666 20000 0.6186 0.880`

#### Improvement in R-squared

\
[`DeltaRSqMC`](https://github.com/jeksterslab/betaMC/reference/DeltaRSqMC.md)`(``hc3``, alpha ``=`` ``0.05``)`\
`#> Call:`\
`#> DeltaRSqMC(object = hc3, alpha = 0.05)`\
`#> `\
`#> Improvement in R-squared`\
`#> type = "hc3"`\
`#>            est     se     R   2.5%  97.5%`\
`#> NARTIC  0.1859 0.0690 20000 0.0484 0.3196`\
`#> PCTGRT  0.1177 0.0551 20000 0.0250 0.2396`\
`#> PCTSUPP 0.0569 0.0365 20000 0.0059 0.1461`

#### Semipartial Correlation Coefficients

\
[`SCorMC`](https://github.com/jeksterslab/betaMC/reference/SCorMC.md)`(``hc3``, alpha ``=`` ``0.05``)`\
`#> Call:`\
`#> SCorMC(object = hc3, alpha = 0.05)`\
`#> `\
`#> Semipartial correlations`\
`#> type = "hc3"`\
`#>            est     se     R   2.5%  97.5%`\
`#> NARTIC  0.4312 0.0873 20000 0.2199 0.5653`\
`#> PCTGRT  0.3430 0.0838 20000 0.1580 0.4895`\
`#> PCTSUPP 0.2385 0.0773 20000 0.0766 0.3823`

#### Squared Partial Correlation Coefficients

\
[`PCorMC`](https://github.com/jeksterslab/betaMC/reference/PCorMC.md)`(``hc3``, alpha ``=`` ``0.05``)`\
`#> Call:`\
`#> PCorMC(object = hc3, alpha = 0.05)`\
`#> `\
`#> Squared partial correlations`\
`#> type = "hc3"`\
`#>            est     se     R   2.5%  97.5%`\
`#> NARTIC  0.4874 0.1204 20000 0.1752 0.6499`\
`#> PCTGRT  0.3757 0.1161 20000 0.1054 0.5570`\
`#> PCTSUPP 0.2254 0.1116 20000 0.0253 0.4492`

#### Differences of Standardized Regression Coefficients

\
[`DiffBetaMC`](https://github.com/jeksterslab/betaMC/reference/DiffBetaMC.md)`(``hc3``, alpha ``=`` ``0.05``)`\
`#> Call:`\
`#> DiffBetaMC(object = hc3, alpha = 0.05)`\
`#> `\
`#> Differences of standardized regression slopes`\
`#> type = "hc3"`\
`#>                   est     se     R    2.5%  97.5%`\
`#> NARTIC-PCTGRT  0.1037 0.1429 20000 -0.1833 0.3810`\
`#> NARTIC-PCTSUPP 0.2319 0.1330 20000 -0.0372 0.4820`\
`#> PCTGRT-PCTSUPP 0.1282 0.1356 20000 -0.1419 0.3903`

## Documentation

See [GitHub Pages](https://jeksterslab.github.io/betaMC/index.html) for
package documentation.

## Citation

To cite `betaMC` in publications, please cite Pesigan & Cheung (2024).

## References

Dudgeon, P. (2017). Some improvements in confidence intervals for
standardized regression coefficients. *Psychometrika*, *82*(4), 928–951.
<https://doi.org/10.1007/s11336-017-9563-z>

National Research Council. (1982). *An assessment of research-doctorate
programs in the United States: Social and behavioral sciences*. National
Academies Press. <https://doi.org/10.17226/9781>

Pesigan, I. J. A., & Cheung, S. F. (2024). Monte Carlo confidence
intervals for the indirect effect with missing data. *Behavior Research
Methods*, *56*(3), 1678–1696.
<https://doi.org/10.3758/s13428-023-02114-4>
