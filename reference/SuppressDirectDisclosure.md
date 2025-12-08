# Suppression of directly-disclosive cells

Function for suppressing directly-disclosive cells in frequency tables.
The method detects and primary suppresses directly-disclosive cells with
the
[FindDisclosiveCells](https://statisticsnorway.github.io/ssb-ssbtools/reference/FindDisclosiveCells.html)
function, and applies a secondary suppression using Gauss suppression
(see
[GaussSuppressionFromData](https://statisticsnorway.github.io/ssb-gausssuppression/reference/GaussSuppressionFromData.md)).

## Usage

``` r
SuppressDirectDisclosure(
  data,
  dimVar,
  freqVar,
  coalition = 1,
  secondaryZeros = coalition,
  candidates = DirectDisclosureCandidates,
  ...
)
```

## Arguments

- data:

  the input data

- dimVar:

  main dimensional variables for the output table

- freqVar:

  variable containing frequency counts

- coalition:

  numeric variable, parameter for primary suppression. Default value is
  1.

- secondaryZeros:

  logical or numeric value for secondary suppression. If logical, it is
  converted to resp numeric value (0 or 1). If numeric, it describes the
  largest number that is prioritized over zeroes in secondary
  suppression. Default value is equal to coalition.

- candidates:

  function parameter for gauss suppression.

- ...:

  optional parameters that can be passed to the primary suppression
  method. See
  [FindDisclosiveCells](https://statisticsnorway.github.io/ssb-ssbtools/reference/FindDisclosiveCells.html)
  for details. In the case of SuppressDirectDisclosure2, `...` are
  parameters to GaussSuppressionFromData.

## Value

data.frame containing the result of the suppression

## Details

SuppressDirectDisclosure has no support for hierarchical data.
SuppressDirectDisclosure2 has, but is less general in other ways.

## Author

Daniel Lupp

## Examples

``` r
tex <- data.frame(v1 = rep(c('a', 'b', 'c'), times = 4),
                  v2 = c('i','i', 'i','h','h','h','i','i','i','h','h','h'),
                  v3 = c('y', 'y', 'y', 'y', 'y', 'y','z','z', 'z', 'z', 'z', 'z'),
                  freq = c(0,0,5,0,2,3,1,0,3,1,1,2))
SuppressDirectDisclosure(tex, c("v1", "v2", "v3"), "freq")
#> GaussSuppression_anySumNOTprimary: ........................
#>       v1    v2    v3 freq v1-prikk v2-prikk v3-prikk primary suppressed
#> 1  Total Total Total   18    FALSE    FALSE    FALSE   FALSE      FALSE
#> 2  Total Total     y   10    FALSE    FALSE    FALSE   FALSE      FALSE
#> 3  Total Total     z    8    FALSE    FALSE    FALSE   FALSE      FALSE
#> 4  Total     h Total    9    FALSE    FALSE    FALSE   FALSE      FALSE
#> 5  Total     h     y    5    FALSE    FALSE    FALSE   FALSE      FALSE
#> 6  Total     h     z    4    FALSE    FALSE    FALSE   FALSE      FALSE
#> 7  Total     i Total    9    FALSE    FALSE    FALSE   FALSE      FALSE
#> 8  Total     i     y    5    FALSE    FALSE    FALSE   FALSE      FALSE
#> 9  Total     i     z    4    FALSE    FALSE    FALSE   FALSE      FALSE
#> 10     a Total Total    2    FALSE    FALSE    FALSE   FALSE      FALSE
#> 11     a Total     y    0    FALSE    FALSE    FALSE   FALSE       TRUE
#> 12     a Total     z    2    FALSE    FALSE     TRUE    TRUE       TRUE
#> 13     a     h Total    1    FALSE     TRUE    FALSE    TRUE       TRUE
#> 14     a     h     y    0    FALSE    FALSE    FALSE   FALSE      FALSE
#> 15     a     h     z    1    FALSE     TRUE     TRUE    TRUE       TRUE
#> 16     a     i Total    1    FALSE     TRUE    FALSE    TRUE       TRUE
#> 17     a     i     y    0    FALSE    FALSE    FALSE   FALSE       TRUE
#> 18     a     i     z    1    FALSE     TRUE     TRUE    TRUE       TRUE
#> 19     b Total Total    3    FALSE    FALSE    FALSE   FALSE      FALSE
#> 20     b Total     y    2    FALSE    FALSE     TRUE    TRUE       TRUE
#> 21     b Total     z    1    FALSE    FALSE    FALSE   FALSE       TRUE
#> 22     b     h Total    3    FALSE     TRUE    FALSE    TRUE       TRUE
#> 23     b     h     y    2    FALSE     TRUE     TRUE    TRUE       TRUE
#> 24     b     h     z    1    FALSE     TRUE    FALSE    TRUE       TRUE
#> 25     b     i Total    0    FALSE    FALSE    FALSE   FALSE       TRUE
#> 26     b     i     y    0    FALSE    FALSE    FALSE   FALSE      FALSE
#> 27     b     i     z    0    FALSE    FALSE    FALSE   FALSE       TRUE
#> 28     c Total Total   13    FALSE    FALSE    FALSE   FALSE      FALSE
#> 29     c Total     y    8    FALSE    FALSE    FALSE   FALSE      FALSE
#> 30     c Total     z    5    FALSE    FALSE    FALSE   FALSE      FALSE
#> 31     c     h Total    5    FALSE    FALSE    FALSE   FALSE       TRUE
#> 32     c     h     y    3    FALSE    FALSE    FALSE   FALSE       TRUE
#> 33     c     h     z    2    FALSE    FALSE    FALSE   FALSE       TRUE
#> 34     c     i Total    8     TRUE    FALSE    FALSE    TRUE       TRUE
#> 35     c     i     y    5     TRUE    FALSE    FALSE    TRUE       TRUE
#> 36     c     i     z    3     TRUE    FALSE    FALSE    TRUE       TRUE
SuppressDirectDisclosure(tex, c("v1", "v2", "v3"), "freq", coalition = 2, unknown.threshold = 10)
#> GaussSuppression_anySumNOTprimary: .....................
#>       v1    v2    v3 freq v1-prikk v2-prikk v3-prikk primary suppressed
#> 1  Total Total Total   18    FALSE    FALSE    FALSE   FALSE      FALSE
#> 2  Total Total     y   10    FALSE    FALSE    FALSE   FALSE      FALSE
#> 3  Total Total     z    8    FALSE    FALSE    FALSE   FALSE      FALSE
#> 4  Total     h Total    9    FALSE    FALSE    FALSE   FALSE      FALSE
#> 5  Total     h     y    5    FALSE    FALSE    FALSE   FALSE      FALSE
#> 6  Total     h     z    4    FALSE    FALSE    FALSE   FALSE      FALSE
#> 7  Total     i Total    9    FALSE    FALSE    FALSE   FALSE      FALSE
#> 8  Total     i     y    5    FALSE    FALSE    FALSE   FALSE      FALSE
#> 9  Total     i     z    4    FALSE    FALSE    FALSE   FALSE      FALSE
#> 10     a Total Total    2    FALSE    FALSE    FALSE   FALSE      FALSE
#> 11     a Total     y    0    FALSE    FALSE    FALSE   FALSE       TRUE
#> 12     a Total     z    2    FALSE    FALSE     TRUE    TRUE       TRUE
#> 13     a     h Total    1    FALSE     TRUE    FALSE    TRUE       TRUE
#> 14     a     h     y    0    FALSE    FALSE    FALSE   FALSE      FALSE
#> 15     a     h     z    1    FALSE     TRUE     TRUE    TRUE       TRUE
#> 16     a     i Total    1    FALSE     TRUE    FALSE    TRUE       TRUE
#> 17     a     i     y    0    FALSE    FALSE    FALSE   FALSE       TRUE
#> 18     a     i     z    1    FALSE     TRUE     TRUE    TRUE       TRUE
#> 19     b Total Total    3    FALSE    FALSE    FALSE   FALSE      FALSE
#> 20     b Total     y    2    FALSE    FALSE     TRUE    TRUE       TRUE
#> 21     b Total     z    1    FALSE    FALSE    FALSE   FALSE       TRUE
#> 22     b     h Total    3    FALSE     TRUE    FALSE    TRUE       TRUE
#> 23     b     h     y    2    FALSE     TRUE     TRUE    TRUE       TRUE
#> 24     b     h     z    1    FALSE     TRUE    FALSE    TRUE       TRUE
#> 25     b     i Total    0    FALSE    FALSE    FALSE   FALSE       TRUE
#> 26     b     i     y    0    FALSE    FALSE    FALSE   FALSE      FALSE
#> 27     b     i     z    0    FALSE    FALSE    FALSE   FALSE       TRUE
#> 28     c Total Total   13    FALSE    FALSE    FALSE   FALSE      FALSE
#> 29     c Total     y    8     TRUE    FALSE    FALSE    TRUE       TRUE
#> 30     c Total     z    5    FALSE    FALSE    FALSE   FALSE       TRUE
#> 31     c     h Total    5    FALSE    FALSE    FALSE   FALSE       TRUE
#> 32     c     h     y    3     TRUE    FALSE     TRUE    TRUE       TRUE
#> 33     c     h     z    2     TRUE    FALSE    FALSE    TRUE       TRUE
#> 34     c     i Total    8     TRUE    FALSE    FALSE    TRUE       TRUE
#> 35     c     i     y    5     TRUE    FALSE    FALSE    TRUE       TRUE
#> 36     c     i     z    3     TRUE     TRUE    FALSE    TRUE       TRUE

z3 <- SSBtools::SSBtoolsData("z3")
a1 <- SuppressDirectDisclosure(z3, c(1, 4, 5), 7)
#> GaussSuppression_anySumNOTprimary: .........................
b1 <- try(SuppressDirectDisclosure(z3, 1:6, 7))
#> Error in SuppressDirectDisclosure(z3, 1:6, 7) : 
#>   Try SuppressKDisclosure? - Hierarchies have been detected. This method does not currently support hierarchical data.
```
