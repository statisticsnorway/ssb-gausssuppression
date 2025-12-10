# Suppress magnitude tables using dominance `(n,k)` or p% rule for primary suppression.

This function utilizes
[`MagnitudeRule`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/MagnitudeRule.md).

## Usage

``` r
SuppressDominantCells(
  data,
  n = 1:length(k),
  k = NULL,
  pPercent = NULL,
  allDominance = FALSE,
  dominanceVar = NULL,
  numVar = NULL,
  dimVar = NULL,
  hierarchies = NULL,
  formula = NULL,
  contributorVar = NULL,
  sWeightVar = NULL,
  ...,
  candidatesVar = NULL,
  singletonZeros = FALSE,
  preAggregate = !is.null(contributorVar) & is.null(sWeightVar),
  spec = PackageSpecs("dominanceSpec")
)
```

## Arguments

- data:

  Input data, typically a data frame, tibble, or data.table. If `data`
  is not a classic data frame, it will be coerced to one internally
  unless `preAggregate` is `TRUE` and `aggregatePackage` is
  `"data.table"`.

- n:

  Parameter `n` in dominance rule. Default is `1:length(k)`.

- k:

  Parameter `k` in dominance rule.

- pPercent:

  Parameter in the p% rule, when non-NULL. Parameters `n` and `k` will
  then be ignored. Technically, calculations are performed internally as
  if `n = 1:2`. The results of these intermediate calculations can be
  viewed by setting `allDominance = TRUE`.

- allDominance:

  Logical. If `TRUE`, additional information is included in the output,
  as described in
  [`MagnitudeRule`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/MagnitudeRule.md).

- dominanceVar:

  Numerical variable to be used in dominance rule. The first `numVar`
  variable will be used if it is not specified.

- numVar:

  Numerical variable to be aggregated. Any `dominanceVar` and
  `candidatesVar` that are specified and not included in `numVar` will
  be aggregated accordingly.

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

  Extra variables to be used as grouping elements in the dominance rule.
  Typically, the variable contains the contributor IDs.

- sWeightVar:

  Name of variable which represents sampling weights to be used in
  dominance rule

- ...:

  Further arguments to be passed to the supplied functions and to
  [`ModelMatrix`](https://statisticsnorway.github.io/ssb-ssbtools/reference/ModelMatrix.html)
  (such as `inputInOutput` and `removeEmpty`).

- candidatesVar:

  Variable to be used in the candidate function to prioritize cells for
  publication and thus not suppression. If not specified, the same
  variable that is used for the dominance rule will be applied (see
  `dominanceVar` and `numVar`).

- singletonZeros:

  When negative values cannot occur, one can determine from a
  non-suppressed marginal cell with the value 0 that all underlying
  cells also have the value 0. The use of `singletonZeros = TRUE` is
  intended to prevent this phenomenon from causing suppressed cells to
  be revealable. It is the zeros in the `dominanceVar` variable that are
  examined. Specifically, the ordinary singleton method is combined with
  a method that is actually designed for frequency tables. This approach
  also works for magnitude tables when
  [`SingletonUniqueContributor0`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/SingletonUniqueContributor.md)
  is utilized.

- preAggregate:

  Parameter to
  [`GaussSuppressionFromData`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/GaussSuppressionFromData.md).
  Necessary to include here since the specification in `spec` cannot
  take `sWeightVar` into account.

- spec:

  `NULL` or a named list of arguments that will act as default values.

## Value

data frame containing aggregated data and suppression information.

## See also

[`SSBtools::tables_by_formulas()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/tables_by_formulas.html)

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

# basic use
SuppressDominantCells(d, n = c(1,2), k = c(80,70), numVar = "num", formula = ~v1 -1)
#> [extraAggregate 18*2->7*4] Checking ....
#> GaussSuppression_numttHTT: ..
#>   v1 num primary suppressed
#> 1 v1 100    TRUE       TRUE
#> 2 v2 100    TRUE       TRUE
#> 3 v3 100    TRUE       TRUE
#> 4 v4 100    TRUE       TRUE
#> 5 v5 100    TRUE       TRUE
#> 6 v6 100   FALSE      FALSE
#> 7 v7 100   FALSE      FALSE
SuppressDominantCells(d, k = c(80,70), numVar = "num", formula = ~v1 -1) # same as above
#> [extraAggregate 18*2->7*4] Checking ....
#> GaussSuppression_numttHTT: ..
#>   v1 num primary suppressed
#> 1 v1 100    TRUE       TRUE
#> 2 v2 100    TRUE       TRUE
#> 3 v3 100    TRUE       TRUE
#> 4 v4 100    TRUE       TRUE
#> 5 v5 100    TRUE       TRUE
#> 6 v6 100   FALSE      FALSE
#> 7 v7 100   FALSE      FALSE
SuppressDominantCells(d, pPercent = 7, numVar = "num", formula = ~v1 -1) 
#> [extraAggregate 18*2->7*4] Checking ....
#> GaussSuppression_numttHTT: ...
#>   v1 num primary suppressed
#> 1 v1 100    TRUE       TRUE
#> 2 v2 100    TRUE       TRUE
#> 3 v3 100    TRUE       TRUE
#> 4 v4 100    TRUE       TRUE
#> 5 v5 100   FALSE      FALSE
#> 6 v6 100   FALSE      FALSE
#> 7 v7 100   FALSE      FALSE

# with weights
SuppressDominantCells(d, n = c(1,2), k = c(80,70), numVar = "num",
dimVar = "v1", sWeightVar = "sweight")
#> [extraAggregate 18*3->7*4] Checking ....
#> GaussSuppression_numttHTT: .......
#>      v1 num sweight weighted.num primary suppressed
#> 1 Total 700      24         1055   FALSE      FALSE
#> 2    v1 100       1          100    TRUE       TRUE
#> 3    v2 100       3          190   FALSE      FALSE
#> 4    v3 100       3          180   FALSE      FALSE
#> 5    v4 100       3          170   FALSE       TRUE
#> 6    v5 100       4          150   FALSE      FALSE
#> 7    v6 100       5          140   FALSE      FALSE
#> 8    v7 100       5          125   FALSE      FALSE

# overwriting some parameters in default spec
SuppressDominantCells(d, n = c(1,2), k = c(80,70), numVar = "num",
dimVar = "v1", sWeightVar = "sweight", domWeightMethod = "tauargus")
#> [extraAggregate 18*3->7*4] Checking ....
#> GaussSuppression_numttHTT: ....
#>      v1 num sweight weighted.num primary suppressed
#> 1 Total 700      24         1055   FALSE      FALSE
#> 2    v1 100       1          100    TRUE       TRUE
#> 3    v2 100       3          190    TRUE       TRUE
#> 4    v3 100       3          180    TRUE       TRUE
#> 5    v4 100       3          170    TRUE       TRUE
#> 6    v5 100       4          150   FALSE      FALSE
#> 7    v6 100       5          140   FALSE      FALSE
#> 8    v7 100       5          125   FALSE      FALSE

# using dominance and few contributors rule together, see second example compared to first
SuppressDominantCells(d, n = c(1,2), k = c(80,70), numVar = "num", formula = ~v1 -1,
primary = c(DominanceRule, NContributorsRule), maxN = 3, allDominance = TRUE)
#> [extraAggregate 18*2->7*4] Checking ....
#> GaussSuppression_numttHTT: ..
#>   v1 num dominant1 dominant2 max1contributor max2contributor n_contr
#> 1 v1 100      1.00      1.00               1              NA       1
#> 2 v2 100      0.90      1.00               2               3       2
#> 3 v3 100      0.80      1.00               4               5       2
#> 4 v4 100      0.70      1.00               6               7       2
#> 5 v5 100      0.50      0.75               8               9       3
#> 6 v6 100      0.40      0.60              11              12       4
#> 7 v7 100      0.25      0.50              15              16       4
#>   n_non0_contr nRule nAll primary suppressed
#> 1            1     1    1    TRUE       TRUE
#> 2            2     2    2    TRUE       TRUE
#> 3            2     2    2    TRUE       TRUE
#> 4            2     2    2    TRUE       TRUE
#> 5            3     3    3    TRUE       TRUE
#> 6            4     4    4   FALSE      FALSE
#> 7            4     4    4   FALSE      FALSE

SuppressDominantCells(d, n = c(1,2), k = c(80,70), numVar = "num", formula = ~v1 -1,
primary = c(DominanceRule, NContributorsRule), maxN = 4, allDominance = TRUE)
#> [extraAggregate 18*2->7*4] Checking ....
#> GaussSuppression_numttHTT: 
#>   v1 num dominant1 dominant2 max1contributor max2contributor n_contr
#> 1 v1 100      1.00      1.00               1              NA       1
#> 2 v2 100      0.90      1.00               2               3       2
#> 3 v3 100      0.80      1.00               4               5       2
#> 4 v4 100      0.70      1.00               6               7       2
#> 5 v5 100      0.50      0.75               8               9       3
#> 6 v6 100      0.40      0.60              11              12       4
#> 7 v7 100      0.25      0.50              15              16       4
#>   n_non0_contr nRule nAll primary suppressed
#> 1            1     1    1    TRUE       TRUE
#> 2            2     2    2    TRUE       TRUE
#> 3            2     2    2    TRUE       TRUE
#> 4            2     2    2    TRUE       TRUE
#> 5            3     3    3    TRUE       TRUE
#> 6            4     4    4    TRUE       TRUE
#> 7            4     4    4    TRUE       TRUE


d2 <- SSBtoolsData("d2")[1:4]   # Data considered as microdata
set.seed(123)
d2$v <- rnorm(nrow(d2))^2

# Hierarchical region variables are detected automatically -> same output column
SuppressDominantCells(data = d2, n = c(1, 2), k = c(70, 95), numVar = "v", 
                      dimVar = c("region", "county", "k_group"), allDominance = TRUE)
#> [extraAggregate 44*4->11*6] Checking ....
#> GaussSuppression_numttHTT: ..............
#>    region          v dominant1 dominant2 max1contributor max2contributor
#> 1       1  3.1878024 0.3302228 0.5721194              23              34
#> 2      10 10.0786664 0.4667651 0.6256381              44              43
#> 3     300 27.3487153 0.1414174 0.2581709              18              16
#> 4       4  1.4198773 0.4753900 0.8495649              35              24
#> 5     400 10.9990663 0.4277063 0.5732848              44              43
#> 6       5  6.7724116 0.4200770 0.7788224              26               3
#> 7       6  7.2219335 0.4421335 0.8494272              16               6
#> 8       8  9.6670903 0.4000773 0.5656266              18               8
#> 9   Total 38.3477816 0.1226765 0.2235319              44              18
#> 10      A  2.2674025 0.4642692 0.8043581              23              34
#> 11      B  1.4198773 0.4753900 0.8495649              35              24
#> 12      C  3.3067218 0.7347372 0.8781498               3              36
#> 13      D  3.4656899 0.8208854 0.9100334              26              15
#> 14      E  3.9154941 0.8154931 0.9947520              16              27
#> 15      F  3.3064395 0.8896119 0.9645732               6              17
#> 16      G  5.5201407 0.7006312 0.9352912              18              29
#> 17      H  4.1469497 0.3859174 0.7650037               8              30
#> 18      I  0.9203999 0.5125673 0.7554309               9              20
#> 19      J  3.0271572 0.5289543 0.9056270              43              21
#> 20      K  7.0515092 0.6671437 0.8796339              44              11
#>    n_contr n_non0_contr primary suppressed
#> 1        8            8   FALSE      FALSE
#> 2        8            8   FALSE      FALSE
#> 3       32           32   FALSE      FALSE
#> 4        4            4   FALSE      FALSE
#> 5       12           12   FALSE      FALSE
#> 6        8            8   FALSE      FALSE
#> 7        8            8   FALSE      FALSE
#> 8        8            8   FALSE      FALSE
#> 9       44           44   FALSE      FALSE
#> 10       4            4   FALSE      FALSE
#> 11       4            4   FALSE      FALSE
#> 12       4            4    TRUE       TRUE
#> 13       4            4    TRUE       TRUE
#> 14       4            4    TRUE       TRUE
#> 15       4            4    TRUE       TRUE
#> 16       4            4    TRUE       TRUE
#> 17       4            4   FALSE       TRUE
#> 18       4            4   FALSE      FALSE
#> 19       4            4   FALSE      FALSE
#> 20       4            4   FALSE      FALSE

# Formula. Hierarchical variables still detected automatically.
SuppressDominantCells(data = d2, n = c(1, 2), k = c(70, 95), numVar = "v", 
                      formula = ~main_income * k_group + region + county - k_group)
#> [extraAggregate 44*5->44*7] Checking ....
#> GaussSuppression_numttHTT: .::::::::::::::::::::
#>    main_income region          v primary suppressed
#> 1        Total  Total 38.3477816   FALSE      FALSE
#> 2   assistance  Total  9.4825828   FALSE      FALSE
#> 3        other  Total  9.7414023   FALSE      FALSE
#> 4     pensions  Total  9.3008137   FALSE      FALSE
#> 5        wages  Total  9.8229828   FALSE      FALSE
#> 6        Total      A  2.2674025   FALSE      FALSE
#> 7        Total      B  1.4198773   FALSE      FALSE
#> 8        Total      C  3.3067218    TRUE       TRUE
#> 9        Total      D  3.4656899    TRUE       TRUE
#> 10       Total      E  3.9154941    TRUE       TRUE
#> 11       Total      F  3.3064395    TRUE       TRUE
#> 12       Total      G  5.5201407    TRUE       TRUE
#> 13       Total      H  4.1469497   FALSE       TRUE
#> 14       Total      I  0.9203999   FALSE      FALSE
#> 15       Total      J  3.0271572   FALSE      FALSE
#> 16       Total      K  7.0515092   FALSE      FALSE
#> 17       Total      1  3.1878024   FALSE      FALSE
#> 18       Total      4  1.4198773   FALSE      FALSE
#> 19       Total      5  6.7724116   FALSE      FALSE
#> 20       Total      6  7.2219335   FALSE      FALSE
#> 21       Total      8  9.6670903   FALSE      FALSE
#> 22       Total     10 10.0786664   FALSE      FALSE
#> 23  assistance    300  8.4123940   FALSE       TRUE
#> 24  assistance    400  1.0701889    TRUE       TRUE
#> 25       other    300  7.5726446   FALSE      FALSE
#> 26       other    400  2.1687577   FALSE      FALSE
#> 27    pensions    300  2.9519862   FALSE       TRUE
#> 28    pensions    400  6.3488275    TRUE       TRUE
#> 29       wages    300  8.4116906   FALSE       TRUE
#> 30       wages    400  1.4112922    TRUE       TRUE

# With hierarchies created manually
ml <- data.frame(levels = c("@", "@@", "@@@", "@@@", "@@@", "@@"), 
        codes = c("Total", "not_assistance", "other", "pensions", "wages", "assistance"))
SuppressDominantCells(data = d2, n = c(1, 2), k = c(70, 95), numVar = "v", 
                      hierarchies = list(main_income = ml, k_group = "Total_Norway"))
#> [extraAggregate 44*3->8*5] Checking ....
#> GaussSuppression_numttHTT: ...............
#>       main_income      k_group         v primary suppressed
#> 1           Total Total_Norway 38.347782   FALSE      FALSE
#> 2           Total          300 27.348715   FALSE      FALSE
#> 3           Total          400 10.999066   FALSE      FALSE
#> 4  not_assistance Total_Norway 28.865199   FALSE      FALSE
#> 5  not_assistance          300 18.936321   FALSE       TRUE
#> 6  not_assistance          400  9.928877   FALSE       TRUE
#> 7      assistance Total_Norway  9.482583   FALSE      FALSE
#> 8      assistance          300  8.412394   FALSE       TRUE
#> 9      assistance          400  1.070189    TRUE       TRUE
#> 10          other Total_Norway  9.741402   FALSE      FALSE
#> 11          other          300  7.572645   FALSE      FALSE
#> 12          other          400  2.168758   FALSE      FALSE
#> 13       pensions Total_Norway  9.300814   FALSE      FALSE
#> 14       pensions          300  2.951986   FALSE       TRUE
#> 15       pensions          400  6.348828    TRUE       TRUE
#> 16          wages Total_Norway  9.822983   FALSE      FALSE
#> 17          wages          300  8.411691   FALSE       TRUE
#> 18          wages          400  1.411292    TRUE       TRUE
                      
# With contributorVar and p% rule    
SuppressDominantCells(data= SSBtoolsData("magnitude1"), 
                      numVar = "value", 
                      dimVar= c("sector4", "geo"), 
                      contributorVar = "company",
                      pPercent = 10, 
                      allDominance = TRUE)                       
#> [preAggregate 20*6->20*5]
#> [extraAggregate 20*5->10*5] Checking .....
#> GaussSuppression_numttHTT: .........
#>          sector4      geo freq value dominant1 dominant2 max1contributor
#> 1          Total    Total   20 462.3 0.5405581 0.8866537               A
#> 2          Total  Iceland    4  37.1 0.7115903 0.9487871               B
#> 3          Total Portugal    8 162.5 0.4855385 0.9526154               B
#> 4          Total    Spain    8 262.7 0.6623525 0.8705748               A
#> 5    Agriculture    Total    4 240.2 0.7181515 1.0000000               A
#> 6    Agriculture  Iceland    0   0.0 0.0000000 0.0000000            <NA>
#> 7    Agriculture Portugal    2 100.4 0.7559761 1.0000000               A
#> 8    Agriculture    Spain    2 139.8 0.6909871 1.0000000               A
#> 9  Entertainment    Total    6 131.5 0.5885932 0.8577947               A
#> 10 Entertainment  Iceland    1  16.8 1.0000000 1.0000000               B
#> 11 Entertainment Portugal    2   9.4 0.7553191 1.0000000               B
#> 12 Entertainment    Spain    3 105.3 0.7350427 0.8907882               A
#> 13  Governmental    Total    4  32.8 0.6585366 0.8567073               B
#> 14  Governmental  Iceland    0   0.0 0.0000000 0.0000000            <NA>
#> 15  Governmental Portugal    2  23.6 0.9152542 1.0000000               B
#> 16  Governmental    Spain    2   9.2 0.7065217 1.0000000               C
#> 17      Industry    Total    6  57.8 0.6107266 0.9083045               B
#> 18      Industry  Iceland    3  20.3 0.4729064 0.9064039               B
#> 19      Industry Portugal    2  29.1 0.8831615 1.0000000               B
#> 20      Industry    Spain    1   8.4 1.0000000 1.0000000               C
#>    max2contributor n_contr n_non0_contr primary suppressed
#> 1                B       4            4   FALSE      FALSE
#> 2                C       3            3    TRUE       TRUE
#> 3                A       3            3    TRUE       TRUE
#> 4                B       4            4   FALSE      FALSE
#> 5                B       2            2    TRUE       TRUE
#> 6             <NA>       0            0   FALSE      FALSE
#> 7                B       2            2    TRUE       TRUE
#> 8                B       2            2    TRUE       TRUE
#> 9                B       4            4   FALSE      FALSE
#> 10            <NA>       1            1    TRUE       TRUE
#> 11               D       2            2    TRUE       TRUE
#> 12               C       3            3   FALSE       TRUE
#> 13               C       3            3   FALSE       TRUE
#> 14            <NA>       0            0   FALSE      FALSE
#> 15               D       2            2    TRUE       TRUE
#> 16               D       2            2    TRUE       TRUE
#> 17               C       3            3   FALSE      FALSE
#> 18               C       3            3   FALSE       TRUE
#> 19               D       2            2    TRUE       TRUE
#> 20            <NA>       1            1    TRUE       TRUE
                      
                      
# Using formula followed by FormulaSelection                        
output <- SuppressDominantCells(data = SSBtoolsData("magnitude1"), 
                                numVar = "value", 
                                formula = ~sector2 * geo + sector4 * eu, 
                                contributorVar = "company", 
                                k = c(80, 99))
#> [preAggregate 20*6->20*7]
#> [extraAggregate 20*7->10*7] Checking .....
#> GaussSuppression_numttHTT: .........:::::
FormulaSelection(output, ~sector2 * geo) 
#>         geo sector4 freq value primary suppressed
#> 1     Total   Total   20 462.3   FALSE      FALSE
#> 2     Total private   16 429.5   FALSE      FALSE
#> 3     Total  public    4  32.8   FALSE      FALSE
#> 4   Iceland   Total    4  37.1   FALSE      FALSE
#> 5  Portugal   Total    8 162.5   FALSE      FALSE
#> 6     Spain   Total    8 262.7   FALSE      FALSE
#> 13  Iceland private    4  37.1   FALSE      FALSE
#> 14 Portugal private    6 138.9   FALSE       TRUE
#> 15    Spain private    6 253.5   FALSE       TRUE
#> 16 Portugal  public    2  23.6    TRUE       TRUE
#> 17    Spain  public    2   9.2    TRUE       TRUE
                      
                      
# This example is similar to the one in the documentation of tables_by_formulas,  
# but it uses SuppressDominantCells with the pPercent and contributorVar parameters.  
tables_by_formulas(SSBtoolsData("magnitude1"),
                   table_fun = SuppressDominantCells, 
                   table_formulas = list(table_1 = ~region * sector2, 
                                         table_2 = ~region1:sector4 - 1, 
                                         table_3 = ~region + sector4 - 1), 
                   substitute_vars = list(region = c("geo", "eu"), region1 = "eu"), 
                   collapse_vars = list(sector = c("sector2", "sector4")), 
                   dominanceVar  = "value", pPercent = 10, contributorVar = "company")                       
#> [preAggregate 20*6->20*7]
#> [extraAggregate 20*7->10*7] Checking .....
#> GaussSuppression_numttHTT: ........::::
#>      region        sector freq value primary suppressed table_1 table_2 table_3
#> 1     Total         Total   20 462.3   FALSE      FALSE    TRUE   FALSE   FALSE
#> 2   Iceland         Total    4  37.1    TRUE       TRUE    TRUE   FALSE    TRUE
#> 3  Portugal         Total    8 162.5    TRUE       TRUE    TRUE   FALSE    TRUE
#> 4     Spain         Total    8 262.7   FALSE      FALSE    TRUE   FALSE    TRUE
#> 5        EU         Total   16 425.2   FALSE       TRUE    TRUE   FALSE    TRUE
#> 6     nonEU         Total    4  37.1    TRUE       TRUE    TRUE   FALSE    TRUE
#> 7     Total       private   16 429.5   FALSE      FALSE    TRUE   FALSE   FALSE
#> 8     Total        public    4  32.8   FALSE      FALSE    TRUE   FALSE   FALSE
#> 9     Total   Agriculture    4 240.2    TRUE       TRUE   FALSE   FALSE    TRUE
#> 10    Total Entertainment    6 131.5   FALSE      FALSE   FALSE   FALSE    TRUE
#> 11    Total  Governmental    4  32.8   FALSE      FALSE   FALSE   FALSE    TRUE
#> 12    Total      Industry    6  57.8   FALSE       TRUE   FALSE   FALSE    TRUE
#> 13  Iceland       private    4  37.1    TRUE       TRUE    TRUE   FALSE   FALSE
#> 14 Portugal       private    6 138.9    TRUE       TRUE    TRUE   FALSE   FALSE
#> 15 Portugal        public    2  23.6    TRUE       TRUE    TRUE   FALSE   FALSE
#> 16    Spain       private    6 253.5   FALSE       TRUE    TRUE   FALSE   FALSE
#> 17    Spain        public    2   9.2    TRUE       TRUE    TRUE   FALSE   FALSE
#> 18       EU       private   12 392.4   FALSE       TRUE    TRUE   FALSE   FALSE
#> 19       EU        public    4  32.8   FALSE      FALSE    TRUE   FALSE   FALSE
#> 20    nonEU       private    4  37.1    TRUE       TRUE    TRUE   FALSE   FALSE
#> 21       EU   Agriculture    4 240.2    TRUE       TRUE   FALSE    TRUE   FALSE
#> 22       EU Entertainment    5 114.7   FALSE       TRUE   FALSE    TRUE   FALSE
#> 23       EU  Governmental    4  32.8   FALSE      FALSE   FALSE    TRUE   FALSE
#> 24       EU      Industry    3  37.5   FALSE      FALSE   FALSE    TRUE   FALSE
#> 25    nonEU Entertainment    1  16.8    TRUE       TRUE   FALSE    TRUE   FALSE
#> 26    nonEU      Industry    3  20.3   FALSE       TRUE   FALSE    TRUE   FALSE

 
 # Example using the dummy_aggregate parameters together with an extra
 # primary rule. A cell becomes primary if the maximum input value
 # exceeds 60% of the cell value.     
 SuppressDominantCells(data = SSBtoolsData("magnitude1"), 
           dominanceVar = "value", 
           formula = ~sector2 * geo + sector4 * eu, 
           contributorVar = "company", 
           pPercent = 3,
           primary = c(MagnitudeRule, 
                       function(..., da_out, num){da_out[[1]]/num[[1]]>0.6}),
           da_fun = c(mAx = function(x) suppressWarnings(max(x))),
           da_vars = c(mAx = "value"),
           da_args = list(name_sep = "__"))  
#> [preAggregate 20*6->20*7]
#> [extraAggregate 20*7->10*7] Checking .....
#> GaussSuppression_numttHTT: ...........
#>         geo       sector4 freq value value__mAx primary suppressed
#> 1     Total         Total   20 462.3       96.6   FALSE      FALSE
#> 2     Total       private   16 429.5       96.6   FALSE       TRUE
#> 3     Total        public    4  32.8       21.6    TRUE       TRUE
#> 4   Iceland         Total    4  37.1       16.8   FALSE      FALSE
#> 5  Portugal         Total    8 162.5       75.9   FALSE      FALSE
#> 6     Spain         Total    8 262.7       96.6   FALSE      FALSE
#> 7     Total   Agriculture    4 240.2       96.6    TRUE       TRUE
#> 8     Total Entertainment    6 131.5       77.4   FALSE       TRUE
#> 9     Total  Governmental    4  32.8       21.6    TRUE       TRUE
#> 10    Total      Industry    6  57.8       25.7   FALSE       TRUE
#> 11       EU         Total   16 425.2       96.6   FALSE      FALSE
#> 12    nonEU         Total    4  37.1       16.8   FALSE      FALSE
#> 13  Iceland       private    4  37.1       16.8   FALSE      FALSE
#> 14 Portugal       private    6 138.9       75.9   FALSE       TRUE
#> 15    Spain       private    6 253.5       96.6   FALSE       TRUE
#> 16 Portugal        public    2  23.6       21.6    TRUE       TRUE
#> 17    Spain        public    2   9.2        6.5    TRUE       TRUE
#> 18       EU   Agriculture    4 240.2       96.6    TRUE       TRUE
#> 19       EU Entertainment    5 114.7       77.4    TRUE       TRUE
#> 20    nonEU Entertainment    1  16.8       16.8    TRUE       TRUE
#> 21       EU  Governmental    4  32.8       21.6    TRUE       TRUE
#> 22       EU      Industry    3  37.5       25.7    TRUE       TRUE
#> 23    nonEU      Industry    3  20.3        9.6   FALSE       TRUE
           
         
 # More advanced example using dummy_aggregate parameters.
 # The default primary function (MagnitudeRule) is removed.
 # A cell becomes primary if the maximum input value exceeds 70% of
 # the cell value, or if the number of contributions from a single
 # company exceeds 55% of the total number of contributions.
 # Change default preAggregate to speed up.              
 SuppressDominantCells(data = SSBtoolsData("magnitude1")[c(1:3, 1:20),], 
   dominanceVar = "value", 
   formula = ~sector2 * geo + sector4 * eu, 
   primary = function(..., da_out, num, freq){
        da_out$value_max/num$value>0.7 |  da_out$company_freq_max/freq>0.55},
   da_fun = c(max = max, freq_max = function(x){max(table(x))}),
   da_vars = c(max = "value", freq_max = "company"),
   preAggregate = TRUE,    # Since default FALSE without contributorVar
   extraAggregate = FALSE, # Not needed since preAggregate and no contributorVar
   singletonMethod = "none")                             
#> [preAggregate 23*6->10*6]
#> GaussSuppression_none: ..............
#>         geo       sector4 freq value value_max company_freq_max primary
#> 1     Total         Total   23 659.3      96.6                9   FALSE
#> 2     Total       private   19 626.5      96.6                8   FALSE
#> 3     Total        public    4  32.8      21.6                2   FALSE
#> 4   Iceland         Total    4  37.1      16.8                2   FALSE
#> 5  Portugal         Total   10 262.9      75.9                5   FALSE
#> 6     Spain         Total    9 359.3      96.6                3   FALSE
#> 7     Total   Agriculture    7 437.2      96.6                4    TRUE
#> 8     Total Entertainment    6 131.5      77.4                3   FALSE
#> 9     Total  Governmental    4  32.8      21.6                2   FALSE
#> 10    Total      Industry    6  57.8      25.7                2   FALSE
#> 11       EU         Total   19 622.2      96.6                7   FALSE
#> 12    nonEU         Total    4  37.1      16.8                2   FALSE
#> 13  Iceland       private    4  37.1      16.8                2   FALSE
#> 14 Portugal       private    8 239.3      75.9                4   FALSE
#> 15    Spain       private    7 350.1      96.6                3   FALSE
#> 16 Portugal        public    2  23.6      21.6                1    TRUE
#> 17    Spain        public    2   9.2       6.5                1    TRUE
#> 18       EU   Agriculture    7 437.2      96.6                4    TRUE
#> 19       EU Entertainment    5 114.7      77.4                2   FALSE
#> 20    nonEU Entertainment    1  16.8      16.8                1    TRUE
#> 21       EU  Governmental    4  32.8      21.6                2   FALSE
#> 22       EU      Industry    3  37.5      25.7                1   FALSE
#> 23    nonEU      Industry    3  20.3       9.6                1   FALSE
#>    suppressed
#> 1       FALSE
#> 2       FALSE
#> 3       FALSE
#> 4       FALSE
#> 5       FALSE
#> 6       FALSE
#> 7        TRUE
#> 8       FALSE
#> 9       FALSE
#> 10       TRUE
#> 11      FALSE
#> 12      FALSE
#> 13      FALSE
#> 14       TRUE
#> 15       TRUE
#> 16       TRUE
#> 17       TRUE
#> 18       TRUE
#> 19       TRUE
#> 20       TRUE
#> 21      FALSE
#> 22      FALSE
#> 23       TRUE
                      
```
