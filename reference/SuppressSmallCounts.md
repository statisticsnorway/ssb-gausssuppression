# Small count frequency table suppression.

This is a wrapper function of
[`GaussSuppressionFromData`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/GaussSuppressionFromData.md)
for small count frequency suppression. For common applications, the
`spec` parameter can be adjusted, see
[`PackageSpecs`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/PackageSpecs.md)
for more information. See Details for more information on function call
customization.

## Usage

``` r
SuppressSmallCounts(
  data,
  maxN,
  freqVar = NULL,
  dimVar = NULL,
  hierarchies = NULL,
  formula = NULL,
  ...,
  spec = PackageSpecs("smallCountSpec")
)
```

## Arguments

- data:

  Input data, typically a data frame, tibble, or data.table. If `data`
  is not a classic data frame, it will be coerced to one internally
  unless `preAggregate` is `TRUE` and `aggregatePackage` is
  `"data.table"`.

- maxN:

  Suppression threshold. Cells with frequency `<= maxN` are marked as
  primary suppressed. This parameter is passed to
  [`PrimaryDefault()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/PrimaryDefault.md)
  via
  [`GaussSuppressionFromData()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/GaussSuppressionFromData.md).

- freqVar:

  A single variable holding counts (name or number).

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

- ...:

  Further arguments to be passed to the supplied functions and to
  [`ModelMatrix`](https://statisticsnorway.github.io/ssb-ssbtools/reference/ModelMatrix.html)
  (such as `inputInOutput` and `removeEmpty`).

- spec:

  `NULL` or a named list of arguments that will act as default values.

## Value

data frame containing aggregated data and suppression information.

## Details

The specs provided in the package (see
[`PackageSpecs`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/PackageSpecs.md))
provide common parameter setups for small count suppression. However, it
might be necessary to customize the parameters further. In this case,
certain parameters from
[`GaussSuppressionFromData`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/GaussSuppressionFromData.md)
might need adjusting from the values provided by the package specs. In
particular, the parameters `protectZeros` (should zeros be primary
suppressed), `extend0` (should empty cells be added before primary
suppression), and `secondaryZeros` (should zero frequency cells be
candidates for secondary suppression) might be of interest. The examples
below illustrate how to override parameters specified by a spec. Note
that this is only possible if `specLock = FALSE`.

## See also

[`SSBtools::tables_by_formulas()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/tables_by_formulas.html)

## Examples

``` r
mun_accidents <- SSBtoolsData("mun_accidents")

SuppressSmallCounts(data = mun_accidents, maxN = 3, dimVar = 1:2, freqVar = 3)
#> [extend0 24*3->24*3]
#> GaussSuppression_anySum: .............
#>      mun     inj freq primary suppressed
#> 1  Total   Total   32   FALSE      FALSE
#> 2  Total   light    3    TRUE       TRUE
#> 3  Total    none    6   FALSE      FALSE
#> 4  Total serious   23   FALSE      FALSE
#> 5  Total unknown    0    TRUE       TRUE
#> 6     k1   Total    4   FALSE       TRUE
#> 7     k1   light    0    TRUE       TRUE
#> 8     k1    none    0    TRUE       TRUE
#> 9     k1 serious    4   FALSE      FALSE
#> 10    k1 unknown    0    TRUE       TRUE
#> 11    k2   Total    6   FALSE      FALSE
#> 12    k2   light    0    TRUE       TRUE
#> 13    k2    none    1    TRUE       TRUE
#> 14    k2 serious    5   FALSE      FALSE
#> 15    k2 unknown    0    TRUE       TRUE
#> 16    k3   Total    6   FALSE      FALSE
#> 17    k3   light    2    TRUE       TRUE
#> 18    k3    none    1    TRUE       TRUE
#> 19    k3 serious    3    TRUE       TRUE
#> 20    k3 unknown    0    TRUE       TRUE
#> 21    k4   Total    9   FALSE      FALSE
#> 22    k4   light    1    TRUE       TRUE
#> 23    k4    none    4   FALSE      FALSE
#> 24    k4 serious    4   FALSE      FALSE
#> 25    k4 unknown    0    TRUE       TRUE
#> 26    k5   Total    1    TRUE       TRUE
#> 27    k5   light    0    TRUE       TRUE
#> 28    k5    none    0    TRUE       TRUE
#> 29    k5 serious    1    TRUE       TRUE
#> 30    k5 unknown    0    TRUE       TRUE
#> 31    k6   Total    6   FALSE      FALSE
#> 32    k6   light    0    TRUE       TRUE
#> 33    k6    none    0    TRUE       TRUE
#> 34    k6 serious    6   FALSE       TRUE
#> 35    k6 unknown    0    TRUE       TRUE
# override default spec
SuppressSmallCounts(data = mun_accidents, maxN = 3, dimVar = 1:2, freqVar = 3, 
                    protectZeros = FALSE)
#> [extend0 24*3->24*3]
#> GaussSuppression_anySum: ...........................
#>      mun     inj freq primary suppressed
#> 1  Total   Total   32   FALSE      FALSE
#> 2  Total   light    3    TRUE       TRUE
#> 3  Total    none    6   FALSE       TRUE
#> 4  Total serious   23   FALSE      FALSE
#> 5  Total unknown    0   FALSE      FALSE
#> 6     k1   Total    4   FALSE       TRUE
#> 7     k1   light    0   FALSE      FALSE
#> 8     k1    none    0   FALSE      FALSE
#> 9     k1 serious    4   FALSE       TRUE
#> 10    k1 unknown    0   FALSE      FALSE
#> 11    k2   Total    6   FALSE      FALSE
#> 12    k2   light    0   FALSE      FALSE
#> 13    k2    none    1    TRUE       TRUE
#> 14    k2 serious    5   FALSE       TRUE
#> 15    k2 unknown    0   FALSE      FALSE
#> 16    k3   Total    6   FALSE      FALSE
#> 17    k3   light    2    TRUE       TRUE
#> 18    k3    none    1    TRUE       TRUE
#> 19    k3 serious    3    TRUE       TRUE
#> 20    k3 unknown    0   FALSE      FALSE
#> 21    k4   Total    9   FALSE      FALSE
#> 22    k4   light    1    TRUE       TRUE
#> 23    k4    none    4   FALSE       TRUE
#> 24    k4 serious    4   FALSE      FALSE
#> 25    k4 unknown    0   FALSE      FALSE
#> 26    k5   Total    1    TRUE       TRUE
#> 27    k5   light    0   FALSE      FALSE
#> 28    k5    none    0   FALSE      FALSE
#> 29    k5 serious    1    TRUE       TRUE
#> 30    k5 unknown    0   FALSE      FALSE
#> 31    k6   Total    6   FALSE      FALSE
#> 32    k6   light    0   FALSE      FALSE
#> 33    k6    none    0   FALSE      FALSE
#> 34    k6 serious    6   FALSE      FALSE
#> 35    k6 unknown    0   FALSE      FALSE
                    
                    
d2 <- SSBtoolsData("d2")
d2$f <- round(d2$freq/10)  # tenth as frequency in examples

# Hierarchical region variables are detected automatically -> same output column
SuppressSmallCounts(data = d2, maxN = 2, freqVar = "f", 
                    dimVar = c("region", "county", "k_group"))
#> [preAggregate 44*6->11*4]
#> [extend0 11*4->11*4]
#> GaussSuppression_anySum: ..................
#>    region  f primary suppressed
#> 1       1 13   FALSE      FALSE
#> 2      10  9   FALSE       TRUE
#> 3     300 59   FALSE      FALSE
#> 4       4  6   FALSE       TRUE
#> 5     400 10   FALSE      FALSE
#> 6       5 11   FALSE      FALSE
#> 7       6 19   FALSE      FALSE
#> 8       8 11   FALSE      FALSE
#> 9   Total 69   FALSE      FALSE
#> 10      A 12   FALSE       TRUE
#> 11      B  6   FALSE       TRUE
#> 12      C  7   FALSE      FALSE
#> 13      D  4   FALSE      FALSE
#> 14      E 13   FALSE      FALSE
#> 15      F  6   FALSE      FALSE
#> 16      G  4   FALSE      FALSE
#> 17      H  7   FALSE      FALSE
#> 18      I  1    TRUE       TRUE
#> 19      J  6   FALSE      FALSE
#> 20      K  3   FALSE       TRUE

# Formula. Hierarchical variables still detected automatically.
SuppressSmallCounts(data = d2, maxN = 3, freqVar = "f", 
                    formula = ~main_income * k_group + region + county - k_group)
#> [extend0 44*5->44*5]
#> GaussSuppression_anySum: ........................
#>    main_income region  f primary suppressed
#> 1        Total  Total 69   FALSE      FALSE
#> 2   assistance  Total 35   FALSE      FALSE
#> 3        other  Total  8   FALSE      FALSE
#> 4     pensions  Total 22   FALSE      FALSE
#> 5        wages  Total  4   FALSE      FALSE
#> 6        Total      A 12   FALSE       TRUE
#> 7        Total      B  6   FALSE      FALSE
#> 8        Total      C  7   FALSE      FALSE
#> 9        Total      D  4   FALSE      FALSE
#> 10       Total      E 13   FALSE      FALSE
#> 11       Total      F  6   FALSE      FALSE
#> 12       Total      G  4   FALSE      FALSE
#> 13       Total      H  7   FALSE      FALSE
#> 14       Total      I  1    TRUE       TRUE
#> 15       Total      J  6   FALSE       TRUE
#> 16       Total      K  3    TRUE       TRUE
#> 17       Total      1 13   FALSE      FALSE
#> 18       Total      4  6   FALSE      FALSE
#> 19       Total      5 11   FALSE      FALSE
#> 20       Total      6 19   FALSE      FALSE
#> 21       Total      8 11   FALSE      FALSE
#> 22       Total     10  9   FALSE      FALSE
#> 23  assistance    300 29   FALSE      FALSE
#> 24  assistance    400  6   FALSE      FALSE
#> 25       other    300  7   FALSE       TRUE
#> 26       other    400  1    TRUE       TRUE
#> 27    pensions    300 19   FALSE       TRUE
#> 28    pensions    400  3    TRUE       TRUE
#> 29       wages    300  4   FALSE       TRUE
#> 30       wages    400  0    TRUE       TRUE

# With hierarchies created manually
ml <- data.frame(levels = c("@", "@@", "@@@", "@@@", "@@@", "@@"), 
        codes = c("Total", "not_assistance", "other", "pensions", "wages", "assistance"))
SuppressSmallCounts(data = d2, maxN = 2, freqVar = "f", 
                    hierarchies = list(main_income = ml, k_group = "Total_Norway"))
#> [preAggregate 44*6->8*3]
#> [extend0 8*3->8*3]
#> GaussSuppression_anySum: ................
#>       main_income      k_group  f primary suppressed
#> 1           Total Total_Norway 69   FALSE      FALSE
#> 2           Total          300 59   FALSE      FALSE
#> 3           Total          400 10   FALSE      FALSE
#> 4  not_assistance Total_Norway 34   FALSE      FALSE
#> 5  not_assistance          300 30   FALSE      FALSE
#> 6  not_assistance          400  4   FALSE      FALSE
#> 7      assistance Total_Norway 35   FALSE      FALSE
#> 8      assistance          300 29   FALSE      FALSE
#> 9      assistance          400  6   FALSE      FALSE
#> 10          other Total_Norway  8   FALSE      FALSE
#> 11          other          300  7   FALSE       TRUE
#> 12          other          400  1    TRUE       TRUE
#> 13       pensions Total_Norway 22   FALSE      FALSE
#> 14       pensions          300 19   FALSE      FALSE
#> 15       pensions          400  3   FALSE      FALSE
#> 16          wages Total_Norway  4   FALSE      FALSE
#> 17          wages          300  4   FALSE       TRUE
#> 18          wages          400  0    TRUE       TRUE


# Data without pensions in k_group 400 
# And assume these are structural zeros (will not be suppressed)
SuppressSmallCounts(data = d2[1:41, ], maxN = 3, freqVar = "f", 
                    hierarchies = list(main_income = ml, k_group = "Total_Norway"), 
                    extend0 = FALSE, structuralEmpty = TRUE)
#> [preAggregate 41*6->7*3]
#> GaussSuppression_anySum: ..............
#>       main_income      k_group  f primary suppressed
#> 1           Total Total_Norway 66   FALSE      FALSE
#> 2           Total          300 59   FALSE      FALSE
#> 3           Total          400  7   FALSE      FALSE
#> 4  not_assistance Total_Norway 31   FALSE      FALSE
#> 5  not_assistance          300 30   FALSE       TRUE
#> 6  not_assistance          400  1    TRUE       TRUE
#> 7      assistance Total_Norway 35   FALSE      FALSE
#> 8      assistance          300 29   FALSE       TRUE
#> 9      assistance          400  6   FALSE       TRUE
#> 10          other Total_Norway  8   FALSE      FALSE
#> 11          other          300  7   FALSE       TRUE
#> 12          other          400  1    TRUE       TRUE
#> 13       pensions Total_Norway 19   FALSE      FALSE
#> 14       pensions          300 19   FALSE      FALSE
#> 15       pensions          400  0   FALSE      FALSE
#> 16          wages Total_Norway  4   FALSE      FALSE
#> 17          wages          300  4   FALSE       TRUE
#> 18          wages          400  0    TRUE       TRUE
# -- Note for the example above -- 
# With protectZeros = FALSE 
#   - No zeros suppressed
# With extend0 = FALSE and structuralEmpty = FALSE 
#   - Primary suppression without protection (with warning) 
# With extend0 = TRUE and structuralEmpty = TRUE 
#   - As default behavior. Suppression/protection of all zeros (since nothing empty)
# With formula instead of hierarchies: Extra parameter needed when extend0 = FALSE.
#   - removeEmpty = FALSE,  to include empty zeros in output.       


# Using formula followed by FormulaSelection 
output <- SuppressSmallCounts(data = SSBtoolsData("example1"), 
                              formula = ~age * geo * year + eu * year, 
                              freqVar = "freq", 
                              maxN = 1)
#> [extend0 18*5->18*5]
#> GaussSuppression_anySum: ..............................................
FormulaSelection(output, ~(age + eu) * year)
#>      age   geo  year freq primary suppressed
#> 1  Total Total Total   59   FALSE      FALSE
#> 2    old Total Total   38   FALSE      FALSE
#> 3  young Total Total   21   FALSE      FALSE
#> 7  Total Total  2014   20   FALSE      FALSE
#> 8  Total Total  2015   18   FALSE      FALSE
#> 9  Total Total  2016   21   FALSE      FALSE
#> 10 Total    EU Total   46   FALSE      FALSE
#> 11 Total nonEU Total   13   FALSE      FALSE
#> 18   old Total  2014   13   FALSE      FALSE
#> 19   old Total  2015   13   FALSE      FALSE
#> 20   old Total  2016   12   FALSE      FALSE
#> 21 young Total  2014    7   FALSE      FALSE
#> 22 young Total  2015    5   FALSE      FALSE
#> 23 young Total  2016    9   FALSE      FALSE
#> 33 Total    EU  2014   15   FALSE      FALSE
#> 34 Total nonEU  2014    5   FALSE      FALSE
#> 35 Total    EU  2015   15   FALSE      FALSE
#> 36 Total nonEU  2015    3   FALSE      FALSE
#> 37 Total    EU  2016   16   FALSE      FALSE
#> 38 Total nonEU  2016    5   FALSE      FALSE


# To illustrate hierarchical_extend0 
# (parameter to underlying function, SSBtools::Extend0fromModelMatrixInput)
SuppressSmallCounts(data = SSBtoolsData("example1"), 
                    formula = ~age * geo * eu, freqVar = "freq", 
                    maxN = 0,  avoidHierarchical = TRUE)
#> [preAggregate 18*5->6*4]
#> [extend0 6*4->12*4]
#> GaussSuppression_anySum: ...........................
#>      age      geo    eu freq primary suppressed
#> 1  Total    Total Total   59   FALSE      FALSE
#> 2    old    Total Total   38   FALSE      FALSE
#> 3  young    Total Total   21   FALSE      FALSE
#> 4  Total  Iceland Total   13   FALSE      FALSE
#> 5  Total Portugal Total   12   FALSE      FALSE
#> 6  Total    Spain Total   34   FALSE      FALSE
#> 7  Total    Total    EU   46   FALSE      FALSE
#> 8  Total    Total nonEU   13   FALSE      FALSE
#> 9    old  Iceland Total   10   FALSE       TRUE
#> 10   old Portugal Total   11   FALSE       TRUE
#> 11   old    Spain Total   17   FALSE      FALSE
#> 12 young  Iceland Total    3   FALSE       TRUE
#> 13 young Portugal Total    1   FALSE       TRUE
#> 14 young    Spain Total   17   FALSE      FALSE
#> 15   old    Total    EU   28   FALSE      FALSE
#> 16   old    Total nonEU   10   FALSE      FALSE
#> 17 young    Total    EU   18   FALSE      FALSE
#> 18 young    Total nonEU    3   FALSE      FALSE
#> 19 Total  Iceland    EU    0    TRUE       TRUE
#> 20 Total  Iceland nonEU   13   FALSE       TRUE
#> 21 Total Portugal    EU   12   FALSE       TRUE
#> 22 Total Portugal nonEU    0    TRUE       TRUE
#> 23 Total    Spain    EU   34   FALSE       TRUE
#> 24 Total    Spain nonEU    0    TRUE       TRUE
#> 25   old  Iceland    EU    0    TRUE       TRUE
#> 26   old  Iceland nonEU   10   FALSE       TRUE
#> 27   old Portugal    EU   11   FALSE       TRUE
#> 28   old Portugal nonEU    0    TRUE       TRUE
#> 29   old    Spain    EU   17   FALSE       TRUE
#> 30   old    Spain nonEU    0    TRUE       TRUE
#> 31 young  Iceland    EU    0    TRUE       TRUE
#> 32 young  Iceland nonEU    3   FALSE       TRUE
#> 33 young Portugal    EU    1   FALSE       TRUE
#> 34 young Portugal nonEU    0    TRUE       TRUE
#> 35 young    Spain    EU   17   FALSE       TRUE
#> 36 young    Spain nonEU    0    TRUE       TRUE
SuppressSmallCounts(data = SSBtoolsData("example1"), 
                    formula = ~age * geo * eu, freqVar = "freq", 
                    maxN = 0,  avoidHierarchical = TRUE,
                    hierarchical_extend0 = TRUE) 
#> [preAggregate 18*5->6*4]
#> [extend0 6*4->6*4]
#>      age      geo    eu freq primary suppressed
#> 1  Total    Total Total   59   FALSE      FALSE
#> 2    old    Total Total   38   FALSE      FALSE
#> 3  young    Total Total   21   FALSE      FALSE
#> 4  Total  Iceland Total   13   FALSE      FALSE
#> 5  Total Portugal Total   12   FALSE      FALSE
#> 6  Total    Spain Total   34   FALSE      FALSE
#> 7  Total    Total    EU   46   FALSE      FALSE
#> 8  Total    Total nonEU   13   FALSE      FALSE
#> 9    old  Iceland Total   10   FALSE      FALSE
#> 10   old Portugal Total   11   FALSE      FALSE
#> 11   old    Spain Total   17   FALSE      FALSE
#> 12 young  Iceland Total    3   FALSE      FALSE
#> 13 young Portugal Total    1   FALSE      FALSE
#> 14 young    Spain Total   17   FALSE      FALSE
#> 15   old    Total    EU   28   FALSE      FALSE
#> 16   old    Total nonEU   10   FALSE      FALSE
#> 17 young    Total    EU   18   FALSE      FALSE
#> 18 young    Total nonEU    3   FALSE      FALSE
#> 19 Total  Iceland nonEU   13   FALSE      FALSE
#> 20 Total Portugal    EU   12   FALSE      FALSE
#> 21 Total    Spain    EU   34   FALSE      FALSE
#> 22   old  Iceland nonEU   10   FALSE      FALSE
#> 23   old Portugal    EU   11   FALSE      FALSE
#> 24   old    Spain    EU   17   FALSE      FALSE
#> 25 young  Iceland nonEU    3   FALSE      FALSE
#> 26 young Portugal    EU    1   FALSE      FALSE
#> 27 young    Spain    EU   17   FALSE      FALSE
               
               
# This example is similar to the one in the documentation of tables_by_formulas,  
# but it uses SuppressSmallCounts, and the input data (SSBtoolsData("magnitude1"))  
# is used to generate a frequency table by excluding the "value" variable. 
tables_by_formulas(SSBtoolsData("magnitude1"), 
                   table_fun = SuppressSmallCounts, 
                   table_formulas = list(table_1 = ~region * sector2, 
                                         table_2 = ~region1:sector4 - 1, 
                                         table_3 = ~region + sector4 - 1), 
                   substitute_vars = list(region = c("geo", "eu"), region1 = "eu"), 
                   collapse_vars = list(sector = c("sector2", "sector4")), 
                   maxN = 2)                 
#> [preAggregate 20*6->10*5]
#> [extend0 10*5->12*5]
#> GaussSuppression_anySum: ...................
#>      region        sector freq primary suppressed table_1 table_2 table_3
#> 1     Total         Total   20   FALSE      FALSE    TRUE   FALSE   FALSE
#> 2   Iceland         Total    4   FALSE      FALSE    TRUE   FALSE    TRUE
#> 3  Portugal         Total    8   FALSE      FALSE    TRUE   FALSE    TRUE
#> 4     Spain         Total    8   FALSE      FALSE    TRUE   FALSE    TRUE
#> 5        EU         Total   16   FALSE      FALSE    TRUE   FALSE    TRUE
#> 6     nonEU         Total    4   FALSE      FALSE    TRUE   FALSE    TRUE
#> 7     Total       private   16   FALSE      FALSE    TRUE   FALSE   FALSE
#> 8     Total        public    4   FALSE      FALSE    TRUE   FALSE   FALSE
#> 9     Total   Agriculture    4   FALSE      FALSE   FALSE   FALSE    TRUE
#> 10    Total Entertainment    6   FALSE      FALSE   FALSE   FALSE    TRUE
#> 11    Total  Governmental    4   FALSE      FALSE   FALSE   FALSE    TRUE
#> 12    Total      Industry    6   FALSE      FALSE   FALSE   FALSE    TRUE
#> 13  Iceland       private    4   FALSE       TRUE    TRUE   FALSE   FALSE
#> 14  Iceland        public    0    TRUE       TRUE    TRUE   FALSE   FALSE
#> 15 Portugal       private    6   FALSE       TRUE    TRUE   FALSE   FALSE
#> 16 Portugal        public    2    TRUE       TRUE    TRUE   FALSE   FALSE
#> 17    Spain       private    6   FALSE       TRUE    TRUE   FALSE   FALSE
#> 18    Spain        public    2    TRUE       TRUE    TRUE   FALSE   FALSE
#> 19       EU       private   12   FALSE       TRUE    TRUE   FALSE   FALSE
#> 20       EU        public    4   FALSE       TRUE    TRUE   FALSE   FALSE
#> 21    nonEU       private    4   FALSE       TRUE    TRUE   FALSE   FALSE
#> 22    nonEU        public    0    TRUE       TRUE    TRUE   FALSE   FALSE
#> 23       EU   Agriculture    4   FALSE       TRUE   FALSE    TRUE   FALSE
#> 24       EU Entertainment    5   FALSE       TRUE   FALSE    TRUE   FALSE
#> 25       EU  Governmental    4   FALSE       TRUE   FALSE    TRUE   FALSE
#> 26       EU      Industry    3   FALSE      FALSE   FALSE    TRUE   FALSE
#> 27    nonEU   Agriculture    0    TRUE       TRUE   FALSE    TRUE   FALSE
#> 28    nonEU Entertainment    1    TRUE       TRUE   FALSE    TRUE   FALSE
#> 29    nonEU  Governmental    0    TRUE       TRUE   FALSE    TRUE   FALSE
#> 30    nonEU      Industry    3   FALSE      FALSE   FALSE    TRUE   FALSE
                   
```
