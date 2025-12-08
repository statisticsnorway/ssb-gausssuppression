# Few contributors suppression

This function provides functionality for suppressing magnitude tables
based on the few contributors rule
([`NContributorsRule`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/NContributorsRule.md)).

## Usage

``` r
SuppressFewContributors(
  data,
  maxN,
  numVar = NULL,
  dimVar = NULL,
  hierarchies = NULL,
  formula = NULL,
  contributorVar = NULL,
  removeCodes = character(0),
  remove0 = TRUE,
  candidatesVar = NULL,
  ...,
  spec = PackageSpecs("fewContributorsSpec")
)
```

## Arguments

- data:

  Input data, typically a data frame, tibble, or data.table. If `data`
  is not a classic data frame, it will be coerced to one internally
  unless `preAggregate` is `TRUE` and `aggregatePackage` is
  `"data.table"`.

- maxN:

  Suppression threshold. Cells where the number of unique contributors
  is less than or equal to `maxN` are marked as primary suppressed. This
  parameter is passed to
  [`NContributorsRule()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/NContributorsRule.md)
  via
  [`GaussSuppressionFromData()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/GaussSuppressionFromData.md).
  Note that within those functions, the parameter name `charVar` is used
  instead of `contributorVar`.

- numVar:

  Numerical variable to be aggregated. Any `candidatesVar` that is
  specified and not included in `numVar` will be aggregated accordingly.
  Additionally, if `remove0` is specified as a variable name and it is
  not included in `numVar`, it will also be aggregated accordingly. See
  parameters `candidatesVar` and `remove0` below.

- dimVar:

  The main dimensional variables and additional aggregating variables.
  This parameter can be useful when hierarchies and formula are
  unspecified.

- hierarchies:

  List of hierarchies, which can be converted by
  [`AutoHierarchies`](https://statisticsnorway.github.io/ssb-ssbtools/reference/AutoHierarchies.html).
  Thus, the variables can also be coded by `"rowFactor"` or `""`, which
  correspond to using the categories in the data.

- formula:

  A model formula

- contributorVar:

  Extra variables to be used as grouping elements when counting
  contributors. Typically, the variable contains the contributor IDs.

- removeCodes:

  Vector of codes to be omitted when counting contributors. With empty
  `contributorVar` row indices are assumed and conversion to integer is
  performed.

- remove0:

  When set to `TRUE` (default), data rows in which the first `numVar`
  (if any) is zero are excluded from the count of contributors.
  Alternatively, `remove0` can be specified as one or more variable
  names. In this case, all data rows with a zero in any of the specified
  variables are omitted from the contributor count. Specifying `remove0`
  as variable name(s) is useful for avoiding warning when there are
  multiple `numVar` variables.

- candidatesVar:

  Variable to be used in the candidate function to prioritize cells for
  publication and thus not suppression. The first `numVar` variable will
  be used if it is not specified.

- ...:

  Further arguments to be passed to the supplied functions and to
  [`ModelMatrix`](https://statisticsnorway.github.io/ssb-ssbtools/reference/ModelMatrix.html)
  (such as `inputInOutput` and `removeEmpty`).

- spec:

  `NULL` or a named list of arguments that will act as default values.

## Value

data.frame containing aggregated data and supppression information.
Columns `nRule` and `nAll` contain the number of contributors. In the
former, `removeCodes` is taken into account.

## Examples

``` r
num <- c(100,
         90, 10,
         80, 20,
         70, 30,
         50, 25, 25,
         40, 20, 20, 20,
         25, 25, 25, 25)
v1 <- c("v1",
        rep(c("v2", "v3", "v4"), each = 2),
        rep("v5", 3),
        rep(c("v6", "v7"), each = 4))
sweight <- c(1, 2, 1, 2, 1, 2, 1, 2, 1, 1, 2, 1, 1, 1, 2, 1, 1, 1)
d <- data.frame(v1 = v1, num = num, sweight = sweight)

SuppressFewContributors(d, formula = ~v1, maxN = 1, numVar = "num")
#> [extraAggregate 18*2->7*3] Checking ....
#> GaussSuppression_numttHTT: .......
#>      v1 num nRule nAll primary suppressed
#> 1 Total 700    18   18   FALSE      FALSE
#> 2    v1 100     1    1    TRUE       TRUE
#> 3    v2 100     2    2   FALSE      FALSE
#> 4    v3 100     2    2   FALSE      FALSE
#> 5    v4 100     2    2   FALSE       TRUE
#> 6    v5 100     3    3   FALSE      FALSE
#> 7    v6 100     4    4   FALSE      FALSE
#> 8    v7 100     4    4   FALSE      FALSE
SuppressFewContributors(d, formula = ~v1, maxN = 2, numVar = "num")
#> [extraAggregate 18*2->7*3] Checking ....
#> GaussSuppression_numttHTT: ....
#>      v1 num nRule nAll primary suppressed
#> 1 Total 700    18   18   FALSE      FALSE
#> 2    v1 100     1    1    TRUE       TRUE
#> 3    v2 100     2    2    TRUE       TRUE
#> 4    v3 100     2    2    TRUE       TRUE
#> 5    v4 100     2    2    TRUE       TRUE
#> 6    v5 100     3    3   FALSE      FALSE
#> 7    v6 100     4    4   FALSE      FALSE
#> 8    v7 100     4    4   FALSE      FALSE
SuppressFewContributors(d, formula = ~v1, maxN = 3, numVar = "num")
#> [extraAggregate 18*2->7*3] Checking ....
#> GaussSuppression_numttHTT: ...
#>      v1 num nRule nAll primary suppressed
#> 1 Total 700    18   18   FALSE      FALSE
#> 2    v1 100     1    1    TRUE       TRUE
#> 3    v2 100     2    2    TRUE       TRUE
#> 4    v3 100     2    2    TRUE       TRUE
#> 5    v4 100     2    2    TRUE       TRUE
#> 6    v5 100     3    3    TRUE       TRUE
#> 7    v6 100     4    4   FALSE      FALSE
#> 8    v7 100     4    4   FALSE      FALSE


d2 <- SSBtoolsData("d2")[-5]
set.seed(123)
d2$v <- round(rnorm(nrow(d2))^2, 1)
d2$family_id <- round(2*as.integer(factor(d2$region)) + runif(nrow(d2)))

# Hierarchical region variables are detected automatically -> same output column
SuppressFewContributors(data = d2, maxN = 2, numVar = "v", contributorVar = "family_id",
                      dimVar = c("region", "county", "k_group"))
#> [preAggregate 44*6->20*6]
#> [extraAggregate 20*6->11*6] Checking .....
#> GaussSuppression_numttHTT: ....::::
#>    region freq    v nRule nAll primary suppressed
#> 1       1    8  3.2     3    3   FALSE       TRUE
#> 2      10    8 10.0     4    4   FALSE       TRUE
#> 3     300   32 27.3    14   14   FALSE      FALSE
#> 4       4    4  1.5     2    2    TRUE       TRUE
#> 5     400   12 10.9     5    5   FALSE      FALSE
#> 6       5    8  6.7     3    3   FALSE       TRUE
#> 7       6    8  7.1     3    3   FALSE      FALSE
#> 8       8    8  9.7     4    4   FALSE      FALSE
#> 9   Total   44 38.2    19   19   FALSE      FALSE
#> 10      A    4  2.3     2    2    TRUE       TRUE
#> 11      B    4  1.5     2    2    TRUE       TRUE
#> 12      C    4  3.3     1    1    TRUE       TRUE
#> 13      D    4  3.4     2    2    TRUE       TRUE
#> 14      E    4  3.9     1    1    TRUE       TRUE
#> 15      F    4  3.2     2    2    TRUE       TRUE
#> 16      G    4  5.5     2    2    TRUE       TRUE
#> 17      H    4  4.2     2    2    TRUE       TRUE
#> 18      I    4  0.9     1    1    TRUE       TRUE
#> 19      J    4  3.0     2    2    TRUE       TRUE
#> 20      K    4  7.0     2    2    TRUE       TRUE

# Formula. Hierarchical variables still detected automatically.
# And codes 1:9 not counted 
SuppressFewContributors(data = d2, maxN = 1, numVar = "v", contributorVar = "family_id",
                      formula = ~main_income * k_group + region + county - k_group,
                      removeCodes = 1:9)
#> [preAggregate 44*6->44*7]
#> [extraAggregate 44*7->44*7] Checking .....
#> GaussSuppression_numttHTT: .......:::::::::::::::::::
#>    main_income region freq    v nRule nAll primary suppressed
#> 1        Total  Total   44 38.2    12   19   FALSE      FALSE
#> 2   assistance  Total   11  9.5     6   10   FALSE      FALSE
#> 3        other  Total   11  9.7     6    9   FALSE      FALSE
#> 4     pensions  Total   11  9.3     5    9   FALSE      FALSE
#> 5        wages  Total   11  9.7     6    9   FALSE      FALSE
#> 6        Total      A    4  2.3     0    2   FALSE       TRUE
#> 7        Total      B    4  1.5     0    2   FALSE      FALSE
#> 8        Total      C    4  3.3     0    1   FALSE      FALSE
#> 9        Total      D    4  3.4     0    2   FALSE      FALSE
#> 10       Total      E    4  3.9     1    1    TRUE       TRUE
#> 11       Total      F    4  3.2     2    2   FALSE       TRUE
#> 12       Total      G    4  5.5     2    2   FALSE       TRUE
#> 13       Total      H    4  4.2     2    2   FALSE       TRUE
#> 14       Total      I    4  0.9     1    1    TRUE       TRUE
#> 15       Total      J    4  3.0     2    2   FALSE       TRUE
#> 16       Total      K    4  7.0     2    2   FALSE       TRUE
#> 17       Total      1    8  3.2     1    3    TRUE       TRUE
#> 18       Total      4    4  1.5     0    2   FALSE      FALSE
#> 19       Total      5    8  6.7     0    3   FALSE      FALSE
#> 20       Total      6    8  7.1     3    3   FALSE       TRUE
#> 21       Total      8    8  9.7     4    4   FALSE      FALSE
#> 22       Total     10    8 10.0     4    4   FALSE      FALSE
#> 23  assistance    300    8  8.4     3    7   FALSE      FALSE
#> 24  assistance    400    3  1.1     3    3   FALSE      FALSE
#> 25       other    300    8  7.5     3    6   FALSE      FALSE
#> 26       other    400    3  2.2     3    3   FALSE      FALSE
#> 27    pensions    300    8  3.0     3    7   FALSE       TRUE
#> 28    pensions    400    3  6.3     2    2   FALSE       TRUE
#> 29       wages    300    8  8.4     4    7   FALSE      FALSE
#> 30       wages    400    3  1.3     2    2   FALSE      FALSE

# With hierarchies created manually
ml <- data.frame(levels = c("@", "@@", "@@@", "@@@", "@@@", "@@"), 
        codes = c("Total", "not_assistance", "other", "pensions", "wages", "assistance"))
SuppressFewContributors(data = d2, maxN = 2, numVar = "v", contributorVar = "family_id",
                      hierarchies = list(main_income = ml, k_group = "Total_Norway"))
#> [preAggregate 44*6->44*5]
#> [extraAggregate 44*5->8*5] Checking .....
#> GaussSuppression_numttHTT: ................
#>       main_income      k_group freq    v nRule nAll primary suppressed
#> 1           Total Total_Norway   44 38.2    19   19   FALSE      FALSE
#> 2           Total          300   32 27.3    14   14   FALSE      FALSE
#> 3           Total          400   12 10.9     5    5   FALSE      FALSE
#> 4  not_assistance Total_Norway   33 28.7    18   18   FALSE      FALSE
#> 5  not_assistance          300   24 18.9    13   13   FALSE      FALSE
#> 6  not_assistance          400    9  9.8     5    5   FALSE      FALSE
#> 7      assistance Total_Norway   11  9.5    10   10   FALSE      FALSE
#> 8      assistance          300    8  8.4     7    7   FALSE      FALSE
#> 9      assistance          400    3  1.1     3    3   FALSE      FALSE
#> 10          other Total_Norway   11  9.7     9    9   FALSE      FALSE
#> 11          other          300    8  7.5     6    6   FALSE      FALSE
#> 12          other          400    3  2.2     3    3   FALSE      FALSE
#> 13       pensions Total_Norway   11  9.3     9    9   FALSE      FALSE
#> 14       pensions          300    8  3.0     7    7   FALSE       TRUE
#> 15       pensions          400    3  6.3     2    2    TRUE       TRUE
#> 16          wages Total_Norway   11  9.7     9    9   FALSE      FALSE
#> 17          wages          300    8  8.4     7    7   FALSE       TRUE
#> 18          wages          400    3  1.3     2    2    TRUE       TRUE
                      
                      
```
