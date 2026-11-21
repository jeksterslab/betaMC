# betaMC

Ivan Jacob Agaloos Pesigan 2026-11-21

## Description

Generates Monte Carlo confidence intervals for standardized regression
coefficients (beta) and other effect sizes, including multiple
correlation, semipartial correlations, improvement in R-squared, squared
partial correlations, and differences in standardized regression
coefficients, for models fitted by
[`lm()`](https://rdrr.io/r/stats/lm.html). `betaMC` combines ideas from
Monte Carlo confidence intervals for the indirect effect (Pesigan and
Cheung, 2024: <https://doi.org/10.3758/s13428-023-02114-4>) and the
sampling covariance matrix of regression coefficients (Dudgeon, 2017:
<https://doi.org/10.1007/s11336-017-9563-z>) to generate confidence
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
[`BetaMC()`](https://jeksterslab.github.io/betaMC/reference/BetaMC.md)
function from the `betaMC` package.

\
[`library`](https://rdrr.io/r/base/library.html)`(``package ``=`` ``betaMC``)`

\
`df`` ``<-`` ``betaMC``::`[`nas1982`](https://jeksterslab.github.io/betaMC/reference/nas1982.md)

### Regression

Fit the regression model using the
[`lm()`](https://rdrr.io/r/stats/lm.html) function.

\
`object`` ``<-`` ``stats``::`[`lm`](https://rdrr.io/r/stats/lm.html)`(``formula ``=`` ``QUALITY`` ``~`` ``NARTIC`` ``+`` ``PCTGRT`` ``+`` ``PCTSUPP``, data ``=`` ``df``)`

### Monte Carlo Sampling Distribution of Parameters

#### Normal-Theory Approach

\
`mvn`` ``<-`` `[`MC`](https://jeksterslab.github.io/betaMC/reference/MC.md)`(``object ``=`` ``object``, type ``=`` ``"mvn"``)`

#### Asymptotic distribution-free Approach

\
`adf`` ``<-`` `[`MC`](https://jeksterslab.github.io/betaMC/reference/MC.md)`(``object ``=`` ``object``, type ``=`` ``"adf"``)`

#### Heteroskedasticity Consistent Approach (HC3)

\
`hc3`` ``<-`` `[`MC`](https://jeksterslab.github.io/betaMC/reference/MC.md)`(``object ``=`` ``object``, type ``=`` ``"hc3"``)`

### Standardized Regression Slopes

#### Normal-Theory Approach

\
[`BetaMC`](https://jeksterslab.github.io/betaMC/reference/BetaMC.md)`(``object ``=`` ``mvn``, alpha ``=`` ``0.05``)`\
`#> Call:`\
`#> BetaMC(object = mvn, alpha = 0.05)`\
`#> `\
`#> Standardized regression slopes`\
`#> type = "mvn"`\
`#>            est     se     R   2.5%  97.5%`\
`#> NARTIC  0.4951 0.0756 20000 0.3416 0.6348`\
`#> PCTGRT  0.3915 0.0769 20000 0.2388 0.5370`\
`#> PCTSUPP 0.2632 0.0748 20000 0.1191 0.4118`

#### Asymptotic distribution-free Approach

\
[`BetaMC`](https://jeksterslab.github.io/betaMC/reference/BetaMC.md)`(``object ``=`` ``adf``, alpha ``=`` ``0.05``)`\
`#> Call:`\
`#> BetaMC(object = adf, alpha = 0.05)`\
`#> `\
`#> Standardized regression slopes`\
`#> type = "adf"`\
`#>            est     se     R   2.5%  97.5%`\
`#> NARTIC  0.4951 0.0678 20000 0.3504 0.6156`\
`#> PCTGRT  0.3915 0.0710 20000 0.2435 0.5191`\
`#> PCTSUPP 0.2632 0.0772 20000 0.1055 0.4075`

#### Heteroskedasticity Consistent Approach (HC3)

\
[`BetaMC`](https://jeksterslab.github.io/betaMC/reference/BetaMC.md)`(``object ``=`` ``hc3``, alpha ``=`` ``0.05``)`\
`#> Call:`\
`#> BetaMC(object = hc3, alpha = 0.05)`\
`#> `\
`#> Standardized regression slopes`\
`#> type = "hc3"`\
`#>            est     se     R   2.5%  97.5%`\
`#> NARTIC  0.4951 0.0798 20000 0.3224 0.6354`\
`#> PCTGRT  0.3915 0.0821 20000 0.2190 0.5394`\
`#> PCTSUPP 0.2632 0.0848 20000 0.0922 0.4253`

### Other Effect Sizes

The `betaMC` package also has functions to generate Monte Carlo
confidence intervals for other effect sizes such as
[`RSqMC()`](https://jeksterslab.github.io/betaMC/reference/RSqMC.md) for
multiple correlation coefficients (R-squared and adjusted R-squared),
[`DeltaRSqMC()`](https://jeksterslab.github.io/betaMC/reference/DeltaRSqMC.md)
for improvement in R-squared,
[`SCorMC()`](https://jeksterslab.github.io/betaMC/reference/SCorMC.md)
for semipartial correlation coefficients,
[`PCorMC()`](https://jeksterslab.github.io/betaMC/reference/PCorMC.md)
for squared partial correlation coefficients, and
[`DiffBetaMC()`](https://jeksterslab.github.io/betaMC/reference/DiffBetaMC.md)
for differences of standardized regression coefficients.

#### Multiple Correlation Coefficients (R-squared and adjusted R-squared)

\
[`RSqMC`](https://jeksterslab.github.io/betaMC/reference/RSqMC.md)`(``object ``=`` ``hc3``, alpha ``=`` ``0.05``)`\
`#> Call:`\
`#> RSqMC(object = hc3, alpha = 0.05)`\
`#> `\
`#> R-squared and adjusted R-squared`\
`#> type = "hc3"`\
`#>        est     se     R   2.5%  97.5%`\
`#> rsq 0.8045 0.0616 20000 0.6467 0.8875`\
`#> adj 0.7906 0.0660 20000 0.6215 0.8795`

#### Improvement in R-squared

\
[`DeltaRSqMC`](https://jeksterslab.github.io/betaMC/reference/DeltaRSqMC.md)`(``object ``=`` ``hc3``, alpha ``=`` ``0.05``)`\
`#> Call:`\
`#> DeltaRSqMC(object = hc3, alpha = 0.05)`\
`#> `\
`#> Improvement in R-squared`\
`#> type = "hc3"`\
`#>            est     se     R   2.5%  97.5%`\
`#> NARTIC  0.1859 0.0691 20000 0.0494 0.3210`\
`#> PCTGRT  0.1177 0.0543 20000 0.0255 0.2363`\
`#> PCTSUPP 0.0569 0.0374 20000 0.0065 0.1489`

#### Semipartial Correlation Coefficients

\
[`SCorMC`](https://jeksterslab.github.io/betaMC/reference/SCorMC.md)`(``object ``=`` ``hc3``, alpha ``=`` ``0.05``)`\
`#> Call:`\
`#> SCorMC(object = hc3, alpha = 0.05)`\
`#> `\
`#> Semipartial correlations`\
`#> type = "hc3"`\
`#>            est     se     R   2.5%  97.5%`\
`#> NARTIC  0.4312 0.0870 20000 0.2223 0.5666`\
`#> PCTGRT  0.3430 0.0829 20000 0.1598 0.4862`\
`#> PCTSUPP 0.2385 0.0776 20000 0.0808 0.3858`

#### Squared Partial Correlation Coefficients

\
[`PCorMC`](https://jeksterslab.github.io/betaMC/reference/PCorMC.md)`(``object ``=`` ``hc3``, alpha ``=`` ``0.05``)`\
`#> Call:`\
`#> PCorMC(object = hc3, alpha = 0.05)`\
`#> `\
`#> Squared partial correlations`\
`#> type = "hc3"`\
`#>            est     se     R   2.5%  97.5%`\
`#> NARTIC  0.4874 0.1194 20000 0.1762 0.6498`\
`#> PCTGRT  0.3757 0.1151 20000 0.1034 0.5542`\
`#> PCTSUPP 0.2254 0.1118 20000 0.0274 0.4526`

#### Differences of Standardized Regression Coefficients

\
[`DiffBetaMC`](https://jeksterslab.github.io/betaMC/reference/DiffBetaMC.md)`(``object ``=`` ``hc3``, alpha ``=`` ``0.05``)`\
`#> Call:`\
`#> DiffBetaMC(object = hc3, alpha = 0.05)`\
`#> `\
`#> Differences of standardized regression slopes`\
`#> type = "hc3"`\
`#>                   est     se     R    2.5%  97.5%`\
`#> NARTIC-PCTGRT  0.1037 0.1422 20000 -0.1742 0.3775`\
`#> NARTIC-PCTSUPP 0.2319 0.1325 20000 -0.0388 0.4804`\
`#> PCTGRT-PCTSUPP 0.1282 0.1367 20000 -0.1497 0.3840`

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
