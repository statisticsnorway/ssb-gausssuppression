# R package GaussSuppression

| [GaussSuppression on CRAN](https://cran.r-project.org/package=GaussSuppression) |     | [pkgdown website](https://statisticsnorway.github.io/ssb-gausssuppression/) |     | [GitHub Repository](https://github.com/statisticsnorway/ssb-gausssuppression) |
|---------------------------------------------------------------------------------|-----|-----------------------------------------------------------------------------|-----|-------------------------------------------------------------------------------|

------------------------------------------------------------------------

[![Mentioned in Awesome Official
Statistics](https://awesome.re/mentioned-badge.svg)](http://www.awesomeofficialstatistics.org)

## R-package to protect tables by suppression using the Gaussian elimination algorithm

A statistical disclosure control tool to protect tables by suppression
using the Gaussian elimination secondary suppression algorithm
[(Langsrud, 2024)](https://doi.org/10.1007%2F978-3-031-69651-0_6). A
suggestion is to start by working with functions
[SuppressSmallCounts()](https://statisticsnorway.github.io/ssb-gausssuppression/reference/SuppressSmallCounts.html)
and
[SuppressDominantCells()](https://statisticsnorway.github.io/ssb-gausssuppression/reference/SuppressDominantCells.html).
These functions use primary suppression functions for the minimum
frequency rule and the dominance rule, respectively. Novel functionality
for suppression of disclosive cells is also included. General primary
suppression functions can be supplied as input to the general working
horse function,
[GaussSuppressionFromData()](https://statisticsnorway.github.io/ssb-gausssuppression/reference/GaussSuppressionFromData.html).
Suppressed frequencies can be replaced by synthetic decimal numbers as
described in [Langsrud
(2019)](https://doi.org/10.1007%2Fs11222-018-9848-9).

------------------------------------------------------------------------

📌 See the [broader list of available
functions](https://statisticsnorway.github.io/ssb-gausssuppression/reference/index.html).

------------------------------------------------------------------------

See the package vignettes: [Magnitude table
suppression](https://cran.r-project.org/web/packages/GaussSuppression/vignettes/Magnitude_table_suppression.html),
[Small count frequency table
suppression](https://cran.r-project.org/web/packages/GaussSuppression/vignettes/Small_count_frequency_table_suppression.html),
[Defining tables for
GaussSuppression](https://cran.r-project.org/web/packages/GaussSuppression/vignettes/define_tables.html).

------------------------------------------------------------------------

### Installation

You can install GaussSuppression from CRAN with

``` r
install.packages("GaussSuppression")
```

Alternatively install from GitHub
by`devtools::install_github("statisticsnorway/ssb-gausssuppression")` if
you want to test the newest changes.

------------------------------------------------------------------------

Official version on CRAN:
<https://cran.r-project.org/package=GaussSuppression>
