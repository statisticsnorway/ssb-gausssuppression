# Incremental Time Suppression

Suppression function useful for sequential calculations over successive
time periods.

## Usage

``` r
IncrementalTimeSuppression(
  data,
  fun,
  timeVar,
  formula,
  suppressedData = NULL,
  subTotals = FALSE,
  finalTotal = FALSE,
  totalPriority = !isFALSE(finalTotal),
  ...
)
```

## Arguments

- data:

  Input data as a data frame

- fun:

  A function:
  [`GaussSuppressionFromData`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/GaussSuppressionFromData.md)
  or one of its wrappers such as
  [`SuppressSmallCounts`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/SuppressSmallCounts.md)
  and
  [`SuppressDominantCells`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/SuppressDominantCells.md).

- timeVar:

  The time period variable

- formula:

  formula A formula defining tables within the time periods. Therefore,
  the variable `timeVar` should not be included.

- suppressedData:

  A data frame or a list of data frames as output from
  [`GaussSuppressionFromData`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/GaussSuppressionFromData.md).

- subTotals:

  Whether all cumulative totals over time should be included.

- finalTotal:

  When `FALSE`, the `timeVar` total is named according to time period
  categories.

- totalPriority:

  When `FALSE`, the `timeVar` totals are not prioritized for
  publication. In other words, these totals are preferred for secondary
  suppression.

- ...:

  Further parameters to
  [`GaussSuppressionFromData`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/GaussSuppressionFromData.md)

## Value

A data.frame

## Note

This function has been made internal since it is new and future
non-backward compatible changes may occur.

## See also

[`AdditionalSuppression`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/AdditionalSuppression.md)

## Examples

``` r
# Generating a dataset spanning four quarters 
d2s <- SSBtoolsData("d2s")
d <- rbind(d2s, d2s, d2s, d2s)
set.seed(10)
d$freq[25:96] <- round(d$freq[25:96] + 9 * rnorm(72))
d$freq[d$freq < 0] <- 0
d$quarter <- rep(c("Q1", "Q2", "Q3", "Q4"), each = 24)

# Redefine the function so that several parameters are preset. 
# Also, a simpler function name.   
STS <- function(data, ...) {
  IncrementalTimeSuppression(data, 
          fun = SuppressSmallCounts, 
          timeVar = "quarter", 
          formula = ~main_income * size, 
          maxN = 15, freqVar = "freq", ...)}
          
          
# Default settings without suppressedData as input
a1 <- STS(d[1:24, ])   
#> [preAggregate 24*6->8*3]
#> [extend0 8*3->8*3]
#> GaussSuppression_anySum: .............
a2 <- STS(d[1:48, ])
#> [preAggregate 48*6->16*4]
#> [extend0 16*4->16*4]
#> GaussSuppression_anySum: .......................................
a3 <- STS(d[1:72, ])
#> [preAggregate 72*6->24*4]
#> [extend0 24*4->24*4]
#> GaussSuppression_anySum: .........................
a4 <- STS(d, finalTotal = TRUE)
#> [preAggregate 96*6->32*4]
#> [extend0 32*4->32*4]
#> GaussSuppression_anySum: ................................

# The quarters and named totals
unique(a1$quarter)
#> [1] "Q1"
unique(a2$quarter)
#> [1] "quarter_Q1_Q2" "Q1"            "Q2"           
unique(a3$quarter)
#> [1] "quarter_Q1_Q3" "Q1"            "Q2"            "Q3"           
unique(a4$quarter)
#> [1] "Total" "Q1"    "Q2"    "Q3"    "Q4"   

# Default settings with suppressedData as input
b2 <- STS(d[1:48, ], suppressedData = a1)
#> [preAggregate 48*6->16*4]
#> [extend0 16*4->16*4]
#> GaussSuppression_anySum: .....................................
b3 <- STS(d[1:72, ], suppressedData = b2)
#> [preAggregate 72*6->24*4]
#> [extend0 24*4->24*4]
#> GaussSuppression_anySum: .............................................
b4 <- STS(d, finalTotal = TRUE, suppressedData = b3)
#> [preAggregate 96*6->32*4]
#> [extend0 32*4->32*4]
#> GaussSuppression_anySum: ............................

# Without totalPriority, suppression will be the same as before. 
# suppressedData makes no difference.
# However, if, for example, there is a changed version of the suppression 
# algorithm, it may be important to use suppressedData 
identical(a2$suppressed, b2$suppressed)
#> [1] TRUE
identical(a3$suppressed, b3$suppressed)
#> [1] TRUE
identical(a4$suppressed, b4$suppressed) # totalPriority here, since finalTotal 
#> [1] FALSE

# With totalPriority and  all the subtotals 
# Note: subtotals are not prioritized
c2 <- STS(d[1:48, ], subTotals = TRUE, totalPriority = TRUE)
#> [preAggregate 48*6->16*4]
#> [extend0 16*4->16*4]
#> GaussSuppression_anySum: .......................................
c3 <- STS(d[1:72, ], subTotals = TRUE, totalPriority = TRUE)
#> [preAggregate 72*7->24*5]
#> [extend0 24*5->24*5]
#> GaussSuppression_anySum: ................................
c4 <- STS(d, subTotals = TRUE, finalTotal = TRUE)
#> [preAggregate 96*8->32*6]
#> [extend0 32*6->32*6]
#> GaussSuppression_anySum: ..............................
unique(c2$quarter)
#> [1] "quarter_Q1_Q2" "Q1"            "Q2"           
unique(c3$quarter)
#> [1] "quarter_Q1_Q3" "Q1"            "Q2"            "Q3"           
#> [5] "quarter_Q1_Q2"
unique(c4$quarter)
#> [1] "Total"         "Q1"            "Q2"            "Q3"           
#> [5] "Q4"            "quarter_Q1_Q2" "quarter_Q1_Q3"

# With such a method, we can see that is important to take into account 
# previously published results.
# Here this is not done and we see differences.
a2[a2$suppressed | c2$suppressed, ]
#>          quarter main_income  size freq primary suppressed
#> 12 quarter_Q1_Q2       other   BIG   13    TRUE       TRUE
#> 13 quarter_Q1_Q2       other small   36   FALSE       TRUE
#> 14 quarter_Q1_Q2    pensions   BIG  165   FALSE       TRUE
#> 15 quarter_Q1_Q2    pensions small   47   FALSE       TRUE
#> 16 quarter_Q1_Q2       wages   BIG   20   FALSE      FALSE
#> 17 quarter_Q1_Q2       wages small   21   FALSE      FALSE
#> 32            Q1       other   BIG   10    TRUE       TRUE
#> 33            Q1       other small   16   FALSE       TRUE
#> 34            Q1    pensions   BIG   79   FALSE      FALSE
#> 35            Q1    pensions small   33   FALSE      FALSE
#> 36            Q1       wages   BIG   20   FALSE       TRUE
#> 37            Q1       wages small    2    TRUE       TRUE
#> 40            Q2       other   BIG    3    TRUE       TRUE
#> 41            Q2       other small   20   FALSE       TRUE
#> 42            Q2    pensions   BIG   86   FALSE       TRUE
#> 43            Q2    pensions small   14    TRUE       TRUE
#> 44            Q2       wages   BIG    0    TRUE       TRUE
#> 45            Q2       wages small   19   FALSE       TRUE
c2[a2$suppressed | c2$suppressed, ]
#>          quarter main_income  size freq primary suppressed
#> 12 quarter_Q1_Q2       other   BIG   13    TRUE       TRUE
#> 13 quarter_Q1_Q2       other small   36   FALSE       TRUE
#> 14 quarter_Q1_Q2    pensions   BIG  165   FALSE      FALSE
#> 15 quarter_Q1_Q2    pensions small   47   FALSE      FALSE
#> 16 quarter_Q1_Q2       wages   BIG   20   FALSE       TRUE
#> 17 quarter_Q1_Q2       wages small   21   FALSE       TRUE
#> 32            Q1       other   BIG   10    TRUE       TRUE
#> 33            Q1       other small   16   FALSE       TRUE
#> 34            Q1    pensions   BIG   79   FALSE       TRUE
#> 35            Q1    pensions small   33   FALSE       TRUE
#> 36            Q1       wages   BIG   20   FALSE       TRUE
#> 37            Q1       wages small    2    TRUE       TRUE
#> 40            Q2       other   BIG    3    TRUE       TRUE
#> 41            Q2       other small   20   FALSE       TRUE
#> 42            Q2    pensions   BIG   86   FALSE       TRUE
#> 43            Q2    pensions small   14    TRUE       TRUE
#> 44            Q2       wages   BIG    0    TRUE       TRUE
#> 45            Q2       wages small   19   FALSE       TRUE
c3[SSBtools::Match( c2[a2$suppressed | c2$suppressed, 1:4], c3[1:4]), ]
#>          quarter main_income  size freq primary suppressed
#> 70 quarter_Q1_Q2       other   BIG   13    TRUE       TRUE
#> 71 quarter_Q1_Q2       other small   36   FALSE       TRUE
#> 72 quarter_Q1_Q2    pensions   BIG  165   FALSE       TRUE
#> 73 quarter_Q1_Q2    pensions small   47   FALSE       TRUE
#> 74 quarter_Q1_Q2       wages   BIG   20   FALSE       TRUE
#> 75 quarter_Q1_Q2       wages small   21   FALSE      FALSE
#> 46            Q1       other   BIG   10    TRUE       TRUE
#> 47            Q1       other small   16   FALSE       TRUE
#> 48            Q1    pensions   BIG   79   FALSE      FALSE
#> 49            Q1    pensions small   33   FALSE      FALSE
#> 50            Q1       wages   BIG   20   FALSE       TRUE
#> 51            Q1       wages small    2    TRUE       TRUE
#> 54            Q2       other   BIG    3    TRUE       TRUE
#> 55            Q2       other small   20   FALSE      FALSE
#> 56            Q2    pensions   BIG   86   FALSE       TRUE
#> 57            Q2    pensions small   14    TRUE       TRUE
#> 58            Q2       wages   BIG    0    TRUE       TRUE
#> 59            Q2       wages small   19   FALSE       TRUE
c4[SSBtools::Match( c2[a2$suppressed | c2$suppressed, 1:4], c4[1:4]), ]
#>          quarter main_income  size freq primary suppressed
#> 92 quarter_Q1_Q2       other   BIG   13    TRUE       TRUE
#> 93 quarter_Q1_Q2       other small   36   FALSE       TRUE
#> 94 quarter_Q1_Q2    pensions   BIG  165   FALSE       TRUE
#> 95 quarter_Q1_Q2    pensions small   47   FALSE       TRUE
#> 96 quarter_Q1_Q2       wages   BIG   20   FALSE       TRUE
#> 97 quarter_Q1_Q2       wages small   21   FALSE       TRUE
#> 60            Q1       other   BIG   10    TRUE       TRUE
#> 61            Q1       other small   16   FALSE       TRUE
#> 62            Q1    pensions   BIG   79   FALSE      FALSE
#> 63            Q1    pensions small   33   FALSE      FALSE
#> 64            Q1       wages   BIG   20   FALSE       TRUE
#> 65            Q1       wages small    2    TRUE       TRUE
#> 68            Q2       other   BIG    3    TRUE       TRUE
#> 69            Q2       other small   20   FALSE      FALSE
#> 70            Q2    pensions   BIG   86   FALSE       TRUE
#> 71            Q2    pensions small   14    TRUE       TRUE
#> 72            Q2       wages   BIG    0    TRUE       TRUE
#> 73            Q2       wages small   19   FALSE       TRUE


# Here we take into account previously published results.
d2 <- STS(d[1:48, ], subTotals = TRUE, totalPriority = TRUE, suppressedData = a1)
#> [preAggregate 48*6->16*4]
#> [extend0 16*4->16*4]
#> GaussSuppression_anySum: .....................................
d3 <- STS(d[1:72, ], subTotals = TRUE, totalPriority = TRUE, suppressedData = d2)
#> [preAggregate 72*7->24*5]
#> [extend0 24*5->24*5]
#> GaussSuppression_anySum: ............................
d4 <- STS(d, subTotals = TRUE, finalTotal = TRUE, suppressedData = d3)
#> [preAggregate 96*8->32*6]
#> [extend0 32*6->32*6]
#> GaussSuppression_anySum: .........................

SSBtools::SortRows(d2[d2$suppressed, ])
#>          quarter main_income  size freq primary forced unsafe suppressed
#> 32            Q1       other   BIG   10    TRUE  FALSE  FALSE       TRUE
#> 33            Q1       other small   16    TRUE  FALSE  FALSE       TRUE
#> 36            Q1       wages   BIG   20    TRUE  FALSE  FALSE       TRUE
#> 37            Q1       wages small    2    TRUE  FALSE  FALSE       TRUE
#> 40            Q2       other   BIG    3    TRUE  FALSE  FALSE       TRUE
#> 41            Q2       other small   20   FALSE  FALSE  FALSE       TRUE
#> 42            Q2    pensions   BIG   86   FALSE  FALSE  FALSE       TRUE
#> 43            Q2    pensions small   14    TRUE  FALSE  FALSE       TRUE
#> 44            Q2       wages   BIG    0    TRUE  FALSE  FALSE       TRUE
#> 45            Q2       wages small   19   FALSE  FALSE  FALSE       TRUE
#> 12 quarter_Q1_Q2       other   BIG   13    TRUE  FALSE  FALSE       TRUE
#> 13 quarter_Q1_Q2       other small   36   FALSE  FALSE  FALSE       TRUE
#> 14 quarter_Q1_Q2    pensions   BIG  165   FALSE  FALSE  FALSE       TRUE
#> 15 quarter_Q1_Q2    pensions small   47   FALSE  FALSE  FALSE       TRUE
SSBtools::SortRows(d3[d3$suppressed, ])
#>          quarter main_income  size freq primary forced unsafe suppressed
#> 46            Q1       other   BIG   10    TRUE  FALSE  FALSE       TRUE
#> 47            Q1       other small   16    TRUE  FALSE  FALSE       TRUE
#> 50            Q1       wages   BIG   20    TRUE  FALSE  FALSE       TRUE
#> 51            Q1       wages small    2    TRUE  FALSE  FALSE       TRUE
#> 54            Q2       other   BIG    3    TRUE  FALSE  FALSE       TRUE
#> 55            Q2       other small   20    TRUE  FALSE  FALSE       TRUE
#> 56            Q2    pensions   BIG   86    TRUE  FALSE  FALSE       TRUE
#> 57            Q2    pensions small   14    TRUE  FALSE  FALSE       TRUE
#> 58            Q2       wages   BIG    0    TRUE  FALSE  FALSE       TRUE
#> 59            Q2       wages small   19    TRUE  FALSE  FALSE       TRUE
#> 62            Q3       other   BIG    0    TRUE  FALSE  FALSE       TRUE
#> 29            Q3       other Total   10    TRUE  FALSE  FALSE       TRUE
#> 63            Q3       other small   10    TRUE  FALSE  FALSE       TRUE
#> 64            Q3    pensions   BIG   71   FALSE  FALSE  FALSE       TRUE
#> 65            Q3    pensions small   32   FALSE  FALSE  FALSE       TRUE
#> 66            Q3       wages   BIG   17   FALSE  FALSE  FALSE       TRUE
#> 31            Q3       wages Total   19   FALSE  FALSE  FALSE       TRUE
#> 67            Q3       wages small    2    TRUE  FALSE  FALSE       TRUE
#> 70 quarter_Q1_Q2       other   BIG   13    TRUE  FALSE  FALSE       TRUE
#> 71 quarter_Q1_Q2       other small   36    TRUE  FALSE  FALSE       TRUE
#> 72 quarter_Q1_Q2    pensions   BIG  165    TRUE  FALSE  FALSE       TRUE
#> 73 quarter_Q1_Q2    pensions small   47    TRUE  FALSE  FALSE       TRUE
#> 14 quarter_Q1_Q3       other   BIG   13    TRUE  FALSE  FALSE       TRUE
#> 7  quarter_Q1_Q3       other Total   59   FALSE  FALSE  FALSE       TRUE
#> 15 quarter_Q1_Q3       other small   46   FALSE  FALSE  FALSE       TRUE
#> 18 quarter_Q1_Q3       wages   BIG   37   FALSE  FALSE  FALSE       TRUE
#> 9  quarter_Q1_Q3       wages Total   60   FALSE  FALSE  FALSE       TRUE
#> 19 quarter_Q1_Q3       wages small   23   FALSE  FALSE  FALSE       TRUE

# With such a method, some annual totals must be suppressed
SSBtools::SortRows(d4[d4$suppressed, ]) 
#>           quarter main_income  size freq primary forced unsafe suppressed
#> 60             Q1       other   BIG   10    TRUE  FALSE  FALSE       TRUE
#> 61             Q1       other small   16    TRUE  FALSE  FALSE       TRUE
#> 64             Q1       wages   BIG   20    TRUE  FALSE  FALSE       TRUE
#> 65             Q1       wages small    2    TRUE  FALSE  FALSE       TRUE
#> 68             Q2       other   BIG    3    TRUE  FALSE  FALSE       TRUE
#> 69             Q2       other small   20    TRUE  FALSE  FALSE       TRUE
#> 70             Q2    pensions   BIG   86    TRUE  FALSE  FALSE       TRUE
#> 71             Q2    pensions small   14    TRUE  FALSE  FALSE       TRUE
#> 72             Q2       wages   BIG    0    TRUE  FALSE  FALSE       TRUE
#> 73             Q2       wages small   19    TRUE  FALSE  FALSE       TRUE
#> 76             Q3       other   BIG    0    TRUE  FALSE  FALSE       TRUE
#> 31             Q3       other Total   10    TRUE  FALSE  FALSE       TRUE
#> 77             Q3       other small   10    TRUE  FALSE  FALSE       TRUE
#> 78             Q3    pensions   BIG   71    TRUE  FALSE  FALSE       TRUE
#> 79             Q3    pensions small   32    TRUE  FALSE  FALSE       TRUE
#> 80             Q3       wages   BIG   17    TRUE  FALSE  FALSE       TRUE
#> 33             Q3       wages Total   19    TRUE  FALSE  FALSE       TRUE
#> 81             Q3       wages small    2    TRUE  FALSE  FALSE       TRUE
#> 84             Q4       other   BIG   10    TRUE  FALSE  FALSE       TRUE
#> 35             Q4       other Total   54   FALSE  FALSE  FALSE       TRUE
#> 85             Q4       other small   44   FALSE  FALSE  FALSE       TRUE
#> 88             Q4       wages   BIG   24   FALSE  FALSE  FALSE       TRUE
#> 37             Q4       wages Total   34   FALSE  FALSE  FALSE       TRUE
#> 89             Q4       wages small   10    TRUE  FALSE  FALSE       TRUE
#> 16          Total       other   BIG   23   FALSE  FALSE  FALSE       TRUE
#> 17          Total       other small   90   FALSE  FALSE  FALSE       TRUE
#> 20          Total       wages   BIG   61   FALSE  FALSE  FALSE       TRUE
#> 21          Total       wages small   33   FALSE  FALSE  FALSE       TRUE
#> 92  quarter_Q1_Q2       other   BIG   13    TRUE  FALSE  FALSE       TRUE
#> 93  quarter_Q1_Q2       other small   36    TRUE  FALSE  FALSE       TRUE
#> 94  quarter_Q1_Q2    pensions   BIG  165    TRUE  FALSE  FALSE       TRUE
#> 95  quarter_Q1_Q2    pensions small   47    TRUE  FALSE  FALSE       TRUE
#> 100 quarter_Q1_Q3       other   BIG   13    TRUE  FALSE  FALSE       TRUE
#> 53  quarter_Q1_Q3       other Total   59    TRUE  FALSE  FALSE       TRUE
#> 101 quarter_Q1_Q3       other small   46    TRUE  FALSE  FALSE       TRUE
#> 104 quarter_Q1_Q3       wages   BIG   37    TRUE  FALSE  FALSE       TRUE
#> 55  quarter_Q1_Q3       wages Total   60    TRUE  FALSE  FALSE       TRUE
#> 105 quarter_Q1_Q3       wages small   23    TRUE  FALSE  FALSE       TRUE

# If necessary, several suppressed data sets can be taken into account
e4 <- STS(d, finalTotal = TRUE, suppressedData = list(a1, a2))
#> [preAggregate 96*6->32*4]
#> [extend0 32*4->32*4]
#> GaussSuppression_anySum: .............................
```
