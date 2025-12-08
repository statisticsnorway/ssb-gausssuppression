# GaussSuppression from data and suppressed data

Extended version of
[`GaussSuppressionFromData`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/GaussSuppressionFromData.md)
that takes into account suppression pattern in suppressed data sent as
input

## Usage

``` r
AdditionalSuppression(
  data,
  ...,
  fun = GaussSuppressionFromData,
  primary = GetDefault(fun, "primary"),
  suppressedData = NULL,
  makePrimary = TRUE,
  makeForced = TRUE,
  forceNotPrimary = TRUE
)
```

## Arguments

- data:

  Input data as a data frame

- ...:

  Further parameters to
  [`GaussSuppressionFromData`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/GaussSuppressionFromData.md)

- fun:

  A function:
  [`GaussSuppressionFromData`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/GaussSuppressionFromData.md)
  or one of its wrappers such as
  [`SuppressSmallCounts`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/SuppressSmallCounts.md)
  and
  [`SuppressDominantCells`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/SuppressDominantCells.md).

- primary:

  As input to
  [`GaussSuppressionFromData`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/GaussSuppressionFromData.md)
  before possible extension caused by `suppressedData`. Supply `NULL` if
  all primary suppressions are retrieved form `suppressedData`.

- suppressedData:

  A data frame or a list of data frames as output from
  [`GaussSuppressionFromData`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/GaussSuppressionFromData.md).

- makePrimary:

  When `TRUE`, suppression in `suppressedData` is preserved.

- makeForced:

  When TRUE, non-suppression in `suppressedData` is preserved. An
  exception is possible primary suppression which has priority over
  `forced`. Use forceNotPrimary to avoid this exception.

- forceNotPrimary:

  When TRUE, non-suppression in `suppressedData` is forced to be not
  primary suppressed.

## Value

Aggregated data with suppression information

## Details

This function is an easy alternative to using
`PrimaryFromSuppressedData` and the relating functions manually. See the
examples of
[`PrimaryFromSuppressedData`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/PrimaryFromSuppressedData.md).
By default, the suppression pattern in `suppressedData` is preserved.
The behavior can be tuned by the parameters.

Note that the variables used in `suppressedData` in addition to
`"suppressed"` are those with matching names in `crossTable`. Others are
ignored. See examples (d3, d4, d5). NOW A FIX IS INCLUDED by attribute
totCode. EXAMPLES NOT YET CHANGED.

## Examples

``` r
z1 <- SSBtoolsData("z1")
z2 <- SSBtoolsData("z2")
z3 <- SSBtoolsData("z3")

# Ordinary suppressions
a <- GaussSuppressionFromData(z1, 1:2, 3, maxN = 5)
#> GaussSuppression_anySum: ........................................
b <- GaussSuppressionFromData(z2, 1:4, 5, maxN = 1)
#> GaussSuppression_anySum: ..............................

# As b and also suppression pattern in a preserved 
b1 <- AdditionalSuppression(z2, 1:4, 5, maxN = 1, suppressedData = a)
#> GaussSuppression_anySum: ...........................

# Rows with differences
cbind(b, b1)[b1$suppressed != b$suppressed, ]
#>    region hovedint ant primary suppressed region hovedint ant primary forced
#> 2       1    annet  14   FALSE      FALSE      1    annet  14   FALSE  FALSE
#> 3       1   arbeid  11   FALSE      FALSE      1   arbeid  11   FALSE  FALSE
#> 37      8    annet  15   FALSE      FALSE      8    annet  15   FALSE  FALSE
#> 38      8   arbeid   7   FALSE      FALSE      8   arbeid   7   FALSE  FALSE
#> 47      A    annet  11   FALSE       TRUE      A    annet  11   FALSE   TRUE
#> 48      A   arbeid  11   FALSE       TRUE      A   arbeid  11   FALSE   TRUE
#> 57      C    annet   5   FALSE      FALSE      C    annet   5    TRUE  FALSE
#> 58      C   arbeid   8   FALSE      FALSE      C   arbeid   8    TRUE  FALSE
#> 62      D    annet  13   FALSE      FALSE      D    annet  13    TRUE  FALSE
#> 63      D   arbeid   2   FALSE      FALSE      D   arbeid   2    TRUE  FALSE
#> 77      G    annet   6   FALSE      FALSE      G    annet   6    TRUE  FALSE
#> 78      G   arbeid   4   FALSE      FALSE      G   arbeid   4    TRUE  FALSE
#> 82      H    annet   9   FALSE      FALSE      H    annet   9    TRUE  FALSE
#> 83      H   arbeid   3   FALSE      FALSE      H   arbeid   3    TRUE  FALSE
#>    unsafe suppressed
#> 2   FALSE       TRUE
#> 3   FALSE       TRUE
#> 37  FALSE       TRUE
#> 38  FALSE       TRUE
#> 47  FALSE      FALSE
#> 48  FALSE      FALSE
#> 57  FALSE       TRUE
#> 58  FALSE       TRUE
#> 62  FALSE       TRUE
#> 63  FALSE       TRUE
#> 77  FALSE       TRUE
#> 78  FALSE       TRUE
#> 82  FALSE       TRUE
#> 83  FALSE       TRUE

# All primary from a 
b2 <- AdditionalSuppression(z2, 1:4, 5, suppressedData = a, primary = NULL, singleton = NULL)
#> GaussSuppression_none: ............................

# Rows with suppression 
b2[b2$suppressed, ]
#>    region hovedint ant primary forced unsafe suppressed
#> 17      4    annet   7   FALSE  FALSE  FALSE       TRUE
#> 18      4   arbeid   1   FALSE  FALSE  FALSE       TRUE
#> 37      8    annet  15   FALSE  FALSE  FALSE       TRUE
#> 38      8   arbeid   7   FALSE  FALSE  FALSE       TRUE
#> 52      B    annet   7    TRUE  FALSE  FALSE       TRUE
#> 53      B   arbeid   1    TRUE  FALSE  FALSE       TRUE
#> 57      C    annet   5    TRUE  FALSE  FALSE       TRUE
#> 58      C   arbeid   8    TRUE  FALSE  FALSE       TRUE
#> 62      D    annet  13    TRUE  FALSE  FALSE       TRUE
#> 63      D   arbeid   2    TRUE  FALSE  FALSE       TRUE
#> 77      G    annet   6    TRUE  FALSE  FALSE       TRUE
#> 78      G   arbeid   4    TRUE  FALSE  FALSE       TRUE
#> 82      H    annet   9    TRUE  FALSE  FALSE       TRUE
#> 83      H   arbeid   3    TRUE  FALSE  FALSE       TRUE

# All primary from b2
d1 <- AdditionalSuppression(data = z3, 1:6, 7, suppressedData = b2, primary = NULL, 
                            singleton = NULL)

# No suppression since no common codes 
d1[d1$suppressed, ]
#> [1] region     hovedint   mnd        ant        primary    forced     unsafe    
#> [8] suppressed
#> <0 rows> (or 0-length row.names)

# Use another coding of fylke
z3$fylke_ <- z3$fylke - 4
d2 <- AdditionalSuppression(data = z3, c(1, 3:6, 8), 7, suppressedData = b2, primary = NULL, 
                            singleton = NULL)
#> GaussSuppression_none: .........................

# Two primary found in b2 -> several secondary
d2[d2$suppressed,]
#>     region hovedint    mnd ant primary forced unsafe suppressed
#> 187      7    annet  Total  67   FALSE  FALSE  FALSE       TRUE
#> 189      7    annet M06M12  32   FALSE  FALSE  FALSE       TRUE
#> 192      7    annet m10m12  11   FALSE  FALSE  FALSE       TRUE
#> 193      7   arbeid  Total  75   FALSE  FALSE  FALSE       TRUE
#> 195      7   arbeid M06M12  24   FALSE  FALSE  FALSE       TRUE
#> 198      7   arbeid m10m12  10   FALSE  FALSE  FALSE       TRUE
#> 217      8    annet  Total  98    TRUE  FALSE  FALSE       TRUE
#> 219      8    annet M06M12  26   FALSE  FALSE  FALSE       TRUE
#> 222      8    annet m10m12   8   FALSE  FALSE  FALSE       TRUE
#> 223      8   arbeid  Total  92    TRUE  FALSE  FALSE       TRUE
#> 225      8   arbeid M06M12  36   FALSE  FALSE  FALSE       TRUE
#> 228      8   arbeid m10m12  13   FALSE  FALSE  FALSE       TRUE
#> 437      Q   arbeid m06m09   1   FALSE  FALSE  FALSE       TRUE
#> 438      Q   arbeid m10m12   1   FALSE  FALSE  FALSE       TRUE
#> 449      Q    trygd m06m09   3   FALSE  FALSE  FALSE       TRUE
#> 450      Q    trygd m10m12   2   FALSE  FALSE  FALSE       TRUE
#> 517      T    annet  Total   3   FALSE  FALSE  FALSE       TRUE
#> 519      T    annet M06M12   2   FALSE  FALSE  FALSE       TRUE
#> 522      T    annet m10m12   1   FALSE  FALSE  FALSE       TRUE
#> 523      T   arbeid  Total   3   FALSE  FALSE  FALSE       TRUE
#> 525      T   arbeid M06M12   2   FALSE  FALSE  FALSE       TRUE
#> 527      T   arbeid m06m09   1   FALSE  FALSE  FALSE       TRUE
#> 539      T    trygd m06m09   2   FALSE  FALSE  FALSE       TRUE
#> 540      T    trygd m10m12   1   FALSE  FALSE  FALSE       TRUE
#> 677      Y   arbeid m06m09   1   FALSE  FALSE  FALSE       TRUE
#> 678      Y   arbeid m10m12   1   FALSE  FALSE  FALSE       TRUE
#> 689      Y    trygd m06m09   3   FALSE  FALSE  FALSE       TRUE
#> 690      Y    trygd m10m12   2   FALSE  FALSE  FALSE       TRUE
#> 698      Z    annet M01M05   1   FALSE  FALSE  FALSE       TRUE
#> 699      Z    annet M06M12   4   FALSE  FALSE  FALSE       TRUE
#> 700      Z    annet m01m05   1   FALSE  FALSE  FALSE       TRUE
#> 702      Z    annet m10m12   2   FALSE  FALSE  FALSE       TRUE
#> 704      Z   arbeid M01M05   1   FALSE  FALSE  FALSE       TRUE
#> 705      Z   arbeid M06M12   1   FALSE  FALSE  FALSE       TRUE
#> 706      Z   arbeid m01m05   1   FALSE  FALSE  FALSE       TRUE
#> 707      Z   arbeid m06m09   1   FALSE  FALSE  FALSE       TRUE
#> 719      Z    trygd m06m09   2   FALSE  FALSE  FALSE       TRUE
#> 720      Z    trygd m10m12   2   FALSE  FALSE  FALSE       TRUE
#> 907      g    annet  Total   2   FALSE  FALSE  FALSE       TRUE
#> 908      g    annet M01M05   2   FALSE  FALSE  FALSE       TRUE
#> 910      g    annet m01m05   2   FALSE  FALSE  FALSE       TRUE
#> 913      g   arbeid  Total   1   FALSE  FALSE  FALSE       TRUE
#> 914      g   arbeid M01M05   1   FALSE  FALSE  FALSE       TRUE
#> 916      g   arbeid m01m05   1   FALSE  FALSE  FALSE       TRUE


# Examples demonstrating limitations of AdditionalSuppression
# Variable mnd in suppressedData is not used 

# No suppression since unsuppressed rows used by makeForced and forceNotPrimary
d3 <- AdditionalSuppression(data = z3, c(1, 3:4, 8), 7, suppressedData = d2, primary = NULL, 
                            singleton = NULL)
#> GaussSuppression_none: ...........................
d3[d3$suppressed, ]
#>     region hovedint ant primary forced unsafe suppressed
#> 32       7    annet  67    TRUE  FALSE  FALSE       TRUE
#> 33       7   arbeid  75    TRUE  FALSE  FALSE       TRUE
#> 37       8    annet  98    TRUE  FALSE  FALSE       TRUE
#> 38       8   arbeid  92    TRUE  FALSE  FALSE       TRUE
#> 87       T    annet   3    TRUE  FALSE  FALSE       TRUE
#> 88       T   arbeid   3    TRUE  FALSE  FALSE       TRUE
#> 152      g    annet   2    TRUE  FALSE  FALSE       TRUE
#> 153      g   arbeid   1    TRUE  FALSE  FALSE       TRUE

# Now suppression, but not too much
d4 <- AdditionalSuppression(data = z3, c(1, 3:4, 8), 7, suppressedData = d2, 
                            forceNotPrimary = FALSE, primary = NULL, singleton = NULL)
#> GaussSuppression_none: ...........................
d4[d4$suppressed, ]
#>     region hovedint ant primary forced unsafe suppressed
#> 32       7    annet  67    TRUE  FALSE  FALSE       TRUE
#> 33       7   arbeid  75    TRUE  FALSE  FALSE       TRUE
#> 37       8    annet  98    TRUE  FALSE  FALSE       TRUE
#> 38       8   arbeid  92    TRUE  FALSE  FALSE       TRUE
#> 87       T    annet   3    TRUE  FALSE  FALSE       TRUE
#> 88       T   arbeid   3    TRUE  FALSE  FALSE       TRUE
#> 152      g    annet   2    TRUE  FALSE  FALSE       TRUE
#> 153      g   arbeid   1    TRUE  FALSE  FALSE       TRUE

# The correct way is to limit the input
d5 <- AdditionalSuppression(data = z3, c(1, 3:4, 8), 7, suppressedData = d2[d2$mnd == "Total", ], 
                            primary = NULL, singleton = NULL)
#> GaussSuppression_none: ...........................
d5[d5$suppressed, ]
#>     region hovedint ant primary forced unsafe suppressed
#> 32       7    annet  67    TRUE  FALSE  FALSE       TRUE
#> 33       7   arbeid  75    TRUE  FALSE  FALSE       TRUE
#> 37       8    annet  98    TRUE  FALSE  FALSE       TRUE
#> 38       8   arbeid  92    TRUE  FALSE  FALSE       TRUE
#> 87       T    annet   3    TRUE  FALSE  FALSE       TRUE
#> 88       T   arbeid   3    TRUE  FALSE  FALSE       TRUE
#> 152      g    annet   2    TRUE  FALSE  FALSE       TRUE
#> 153      g   arbeid   1    TRUE  FALSE  FALSE       TRUE
```
