# Cell suppression from synthetic decimal numbers

Decimal numbers, as calculated by
[`GaussSuppressDec`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/GaussSuppressDec.md),
are used to decide suppression (whole numbers or not). Technically, the
calculations are done via
[`GaussSuppressionFromData`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/GaussSuppressionFromData.md),
but without running
[`GaussSuppression`](https://statisticsnorway.github.io/ssb-ssbtools/reference/GaussSuppression.html).
All suppressed cells are primary suppressed.

## Usage

``` r
SuppressionFromDecimals(
  data,
  decVar,
  freqVar = NULL,
  numVar = NULL,
  preAggregate = FALSE,
  digits = 9,
  ...
)
```

## Arguments

- data:

  Input data as a data frame

- decVar:

  One ore several (`nRep>1`) decimal number variables.

- freqVar:

  A single variable holding counts (not needed)

- numVar:

  Other numerical variables to be aggregated

- preAggregate:

  Parameter to
  [`GaussSuppressionFromData`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/GaussSuppressionFromData.md)

- digits:

  Parameter to
  [`RoundWhole`](https://statisticsnorway.github.io/ssb-ssbtools/reference/RoundWhole.html).
  Values close to whole numbers will be rounded.

- ...:

  Other parameters to
  [`GaussSuppressionFromData`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/GaussSuppressionFromData.md)

## Value

Aggregated data with suppression information

## Details

Several decimal number variables reduce the probability of obtaining
whole numbers by chance.

## Author

Øyvind Langsrud

## Examples

``` r
z2 <- SSBtoolsData("z2")

# Find suppression and decimal numbers with "fylke" in model 
a1 <- GaussSuppressDec(z2,
                       fun = SuppressSmallCounts,   
                       dimVar = c("region", "fylke", "hovedint"), 
                       freqVar = "ant", protectZeros = FALSE, maxN = 2, 
                       output = "inner")
#> [extend0 44*4->44*4]
#> GaussSuppression_anySum: ..........................

# Add decimal numbers to data 
z2$freqDec <- a1$freqDec

# Find suppression with "kostragr" in model 
a2 <- SuppressionFromDecimals(z2, dimVar = c("region", "kostragr", "hovedint"), 
                              freqVar = "ant", decVar = "freqDec")
#> GaussSuppression_none: 
tail(a2)
#>    region hovedint ant    freqDec primary suppressed
#> 65      J    trygd  20 20.0000000   FALSE      FALSE
#> 66      K    Total  35 35.0000000   FALSE      FALSE
#> 67      K    annet   4  5.6718736    TRUE       TRUE
#> 68      K   arbeid   2  0.3281264    TRUE       TRUE
#> 69      K soshjelp  18 18.0000000   FALSE      FALSE
#> 70      K    trygd  11 11.0000000   FALSE      FALSE

b1 <- GaussSuppressDec(data = SSBtoolsData("magnitude1"), 
                       fun = SuppressDominantCells, 
                       numVar = "value", 
                       formula = ~sector2 * geo + sector4 * eu,
                       contributorVar = "company", k = c(80, 99))
#> [preAggregate 20*6->20*7]
#> [extraAggregate 20*7->10*7] Checking .....
#> GaussSuppression_numttHTT: .........:::::
 
b2 <- SuppressionFromDecimals(b1[b1$isInner, ], 
                              formula = ~(sector2 + sector4) * eu, 
                              numVar = "value", 
                              decVar = "freqDec")
#> GaussSuppression_none: 
FormulaSelection(b2, ~sector2 * eu)                                 
#>    sector4    eu       freqDec value primary suppressed
#> 1    Total Total  1.110223e-16 462.3   FALSE      FALSE
#> 2  private Total -1.110223e-16 429.5   FALSE      FALSE
#> 3   public Total  2.220446e-16  32.8   FALSE      FALSE
#> 8    Total    EU  5.551115e-16 425.2   FALSE      FALSE
#> 9    Total nonEU -4.440892e-16  37.1   FALSE      FALSE
#> 10 private    EU  3.330669e-16 392.4   FALSE      FALSE
#> 11 private nonEU -4.440892e-16  37.1   FALSE      FALSE
#> 12  public    EU  2.220446e-16  32.8   FALSE      FALSE
                              
```
