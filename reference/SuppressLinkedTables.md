# Consistent Suppression of Linked Tables

Provides alternatives to global protection for linked tables through
methods that may reduce the computational burden.

## Usage

``` r
SuppressLinkedTables(
  data = NULL,
  fun,
  ...,
  withinArg = NULL,
  linkedGauss = "super-consistent",
  linkedIntervals = ifelse(linkedGauss == "local-bdiag", "local-bdiag",
    "super-consistent"),
  lpPackage = NULL,
  recordAware = TRUE,
  iterBackTracking = Inf,
  whenEmptyUnsuppressed = NULL
)
```

## Arguments

- data:

  The `data` argument to `fun`. When NULL `data` must be included in
  `withinArg`.

- fun:

  A function:
  [`GaussSuppressionFromData`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/GaussSuppressionFromData.md)
  or one of its wrappers such as
  [`SuppressSmallCounts`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/SuppressSmallCounts.md)
  and
  [`SuppressDominantCells`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/SuppressDominantCells.md).

- ...:

  Arguments to `fun` that are kept constant.

- withinArg:

  A list of named lists. Arguments to `fun` that are not kept constant.
  If `withinArg` is named, the names will be used as names in the output
  list.

- linkedGauss:

  Specifies the strategy for protecting linked tables. The
  `"super-consistent"`, `"consistent"`, and `"local-bdiag"` methods
  protect all linked tables together in a single call to
  [`GaussSuppression()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/GaussSuppression.html)
  using an internally constructed block-diagonal model matrix.

  - `"super-consistent"` (default): Shares the key property of
    `"consistent"` that common cells are suppressed equally across
    tables, but also exploits the fact that these cells have identical
    values in all tables. The coordination is therefore stronger. If
    intervals are calculated using such coordination, common cells will
    have identical interval bounds in each table.

  - `"consistent"`: Common cells are suppressed equally across tables.

  - `"local"`: Each table is protected independently by a separate call
    to
    [`GaussSuppression()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/GaussSuppression.html).

  - `"back-tracking"`: Iterative approach where each table is protected
    via
    [`GaussSuppression()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/GaussSuppression.html),
    and primary suppressions are adjusted based on secondary
    suppressions from other tables across iterations.

  - `"local-bdiag"`: Produces the same result as `"local"`, but uses a
    single call to
    [`GaussSuppression()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/GaussSuppression.html).
    It does not apply the linked-table methodology.

- linkedIntervals:

  This parameter controls how interval calculations, triggered by the
  `lpPackage` parameter, are performed.

  - **Default:** `"local-bdiag"` if `linkedGauss` is set to
    `"local-bdiag"`, and `"super-consistent"` in all other cases.

  - Possible values of **`linkedIntervals`** are `"local-bdiag"` and
    `"super-consistent"`.

  - Interval calculations can be performed when **`linkedGauss`** is
    `"super-consistent"`, `"consistent"`, or `"local-bdiag"`.

  - When `linkedGauss` is `"local-bdiag"`, `"local-bdiag"` is the only
    allowed value in `linkedIntervals` (except that, with the
    alternative approaches, `"global"` may appear as a later element;
    `"super-consistent"` is never allowed).

  - It is possible to request multiple types of intervals by supplying
    `linkedIntervals` as a vector. Only the first value affects the
    additional suppression defined by `rangePercent` and/or `rangeMin`.

  - With the alternative approaches (see the note below), `"global"` may
    also appear in `linkedIntervals`, provided it is not the first
    element.

- lpPackage:

  See
  [`GaussSuppressionFromData()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/GaussSuppressionFromData.md).

- recordAware:

  If `TRUE` (default), the suppression procedure will ensure consistency
  across cells that aggregate the same underlying records, even when
  their variable combinations differ. When `TRUE`, `data` cannot be
  included in `withinArg`.

- iterBackTracking:

  Maximum number of back-tracking iterations.

- whenEmptyUnsuppressed:

  Parameter to
  [`GaussSuppression`](https://statisticsnorway.github.io/ssb-ssbtools/reference/GaussSuppression.html).
  This is about a helpful message *"Cells with empty input will never be
  secondary suppressed. Extend input data with zeros?"* Here, the
  default is set to `NULL` (no message), since preprocessing of the
  model matrix may invalidate the assumptions behind this message.

## Value

A list of data frames, or, if `withinArg` is `NULL`, the ordinary output
from `fun`.

## Details

The reason for introducing the new method `"consistent"`, which has not
yet been extensively tested in practice, is to provide something that
works better than `"back-tracking"`, while still offering equally strong
protection.

Note that for singleton methods of the *elimination* type (see
[`SSBtools::NumSingleton()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/NumSingleton.html)),
`"back-tracking"` may lead to the creation of a large number of
redundant secondary cells. This is because, during the method's
iterations, all secondary cells are eventually treated as primary. As a
result, protection is applied to prevent a singleton contributor from
inferring a secondary cell that was only included to protect that same
contributor.

Note that the frequency singleton methods `"subSpace"`, `"anySum0"`, and
`"anySumNOTprimary"` are currently not implemented and will result in an
error. As a result, the `singletonZeros` parameter in the
[`SuppressDominantCells()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/SuppressDominantCells.md)
function cannot be set to `TRUE`, and the
[`SuppressKDisclosure()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/SuppressKDisclosure.md)
function is not available for use. Also note that automatic forcing of
`"anySumNOTprimary"` is disabled. That is,
[`SSBtools::GaussSuppression()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/GaussSuppression.html)
is called with `auto_anySumNOTprimary = FALSE`. See the parameter
documentation for an explanation of why `FALSE` is required.

## Note

Note on differences between `SuppressLinkedTables()` and alternative
approaches. By *alternatives*, we refer to using the `linkedGauss`
parameter via
[`GaussSuppressionFromData()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/GaussSuppressionFromData.md),
its wrappers, or through
[`tables_by_formulas()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/tables_by_formulas.html),
as shown in the examples below.

- Alternatives can be used when only the `formula` parameter varies
  between the linked tables.

- `SuppressLinkedTables()` creates several smaller model matrices, which
  may be combined into a single block-diagonal matrix. A large overall
  matrix is never created.

- With the alternatives, a large overall matrix is created first.
  Smaller matrices are then derived from it. If the size of the full
  matrix is a bottleneck, `SuppressLinkedTables()` is the better choice.

- The `"global"` method is available with the alternatives, but not with
  `SuppressLinkedTables()`.

- The `collapseAware` parameter is supported by the alternatives, but
  not by `SuppressLinkedTables()`. This option may improve coordination
  across tables. See
  [`GaussSuppressionFromData()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/GaussSuppressionFromData.md).

- Due to differences in candidate ordering, the two methods may not
  always produce identical results. With the alternatives, candidate
  order is constructed globally across all cells (as with the global
  method). In contrast, `SuppressLinkedTables()` uses a locally
  determined candidate order within each table. The ordering across
  tables is coordinated to ensure the method works, but it is not based
  on a strictly defined global order. This may lead to some differences.

- With the alternatives, `linkedIntervals` may also contain `"global"`.
  See the documentaion of the `linkedIntervals` parameter above and in
  [`GaussSuppressionFromData()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/GaussSuppressionFromData.md).

## Examples

``` r
### The first example can be performed in three ways
### Alternatives are possible since only the formula parameter varies between the linked tables
 
a <- SuppressLinkedTables(data = SSBtoolsData("magnitude1"), # With trick "sector4 - sector4" and 
                 fun = SuppressDominantCells,        # "geo - geo" to ensure same names in output
                 withinArg = list(list(formula = ~(geo + eu) * sector2 + sector4 - sector4), 
                                  list(formula = ~eu:sector4 - 1 + geo - geo), 
                                  list(formula = ~geo + eu + sector4 - 1)), 
                 dominanceVar  = "value", 
                 pPercent = 10, 
                 contributorVar = "company",
                 linkedGauss = "consistent")
#> [preAggregate 20*13->20*14]
#> [extraAggregate 20*14->10*14] Checking .....
#> [preAggregate 20*13->20*13]
#> [extraAggregate 20*13->10*13] Checking .....
#> [preAggregate 20*13->20*13]
#> [extraAggregate 20*13->10*13] Checking .....
#> 
#> ====== Linked GaussSuppression by "consistent" algorithm:
#> 
#> GaussSuppression_numttHTT: ................
print(a)  
#> [[1]]
#>         geo sector4 freq value primary suppressed
#> 1     Total   Total   20 462.3   FALSE      FALSE
#> 2   Iceland   Total    4  37.1    TRUE       TRUE
#> 3  Portugal   Total    8 162.5    TRUE       TRUE
#> 4     Spain   Total    8 262.7   FALSE      FALSE
#> 5        EU   Total   16 425.2   FALSE       TRUE
#> 6     nonEU   Total    4  37.1    TRUE       TRUE
#> 7     Total private   16 429.5   FALSE      FALSE
#> 8     Total  public    4  32.8   FALSE      FALSE
#> 9   Iceland private    4  37.1    TRUE       TRUE
#> 10 Portugal private    6 138.9    TRUE       TRUE
#> 11 Portugal  public    2  23.6    TRUE       TRUE
#> 12    Spain private    6 253.5   FALSE       TRUE
#> 13    Spain  public    2   9.2    TRUE       TRUE
#> 14       EU private   12 392.4   FALSE       TRUE
#> 15       EU  public    4  32.8   FALSE      FALSE
#> 16    nonEU private    4  37.1    TRUE       TRUE
#> 
#> [[2]]
#>         sector4   geo freq value primary suppressed
#> 1   Agriculture    EU    4 240.2    TRUE       TRUE
#> 2 Entertainment    EU    5 114.7   FALSE      FALSE
#> 3  Governmental    EU    4  32.8   FALSE      FALSE
#> 4      Industry    EU    3  37.5   FALSE      FALSE
#> 5 Entertainment nonEU    1  16.8    TRUE       TRUE
#> 6      Industry nonEU    3  20.3   FALSE      FALSE
#> 
#> [[3]]
#>        geo       sector4 freq value primary suppressed
#> 1  Iceland         Total    4  37.1    TRUE       TRUE
#> 2 Portugal         Total    8 162.5    TRUE       TRUE
#> 3    Spain         Total    8 262.7   FALSE      FALSE
#> 4       EU         Total   16 425.2   FALSE       TRUE
#> 5    nonEU         Total    4  37.1    TRUE       TRUE
#> 6    Total   Agriculture    4 240.2    TRUE       TRUE
#> 7    Total Entertainment    6 131.5   FALSE      FALSE
#> 8    Total  Governmental    4  32.8   FALSE      FALSE
#> 9    Total      Industry    6  57.8   FALSE      FALSE
#> 

# Alternatively, SuppressDominantCells() can be run directly using the linkedGauss parameter  
a1 <- SuppressDominantCells(SSBtoolsData("magnitude1"), 
               formula = list(table_1 = ~(geo + eu) * sector2, 
                              table_2 = ~eu:sector4 - 1,
                              table_3 = ~(geo + eu) + sector4 - 1), 
               dominanceVar = "value", 
               pPercent = 10, 
               contributorVar = "company", 
               linkedGauss = "consistent")
#> [preAggregate 20*6->20*7]
#> [extraAggregate 20*7->10*7] Checking .....
#> 
#> ====== Linked GaussSuppression by "consistent" algorithm:
#> 
#> GaussSuppression_numttHTT: ................
print(a1)
#>         geo       sector4 freq value primary suppressed
#> 1     Total         Total   20 462.3   FALSE      FALSE
#> 2   Iceland         Total    4  37.1    TRUE       TRUE
#> 3  Portugal         Total    8 162.5    TRUE       TRUE
#> 4     Spain         Total    8 262.7   FALSE      FALSE
#> 5        EU         Total   16 425.2   FALSE       TRUE
#> 6     nonEU         Total    4  37.1    TRUE       TRUE
#> 7     Total       private   16 429.5   FALSE      FALSE
#> 8     Total        public    4  32.8   FALSE      FALSE
#> 9     Total   Agriculture    4 240.2    TRUE       TRUE
#> 10    Total Entertainment    6 131.5   FALSE      FALSE
#> 11    Total  Governmental    4  32.8   FALSE      FALSE
#> 12    Total      Industry    6  57.8   FALSE      FALSE
#> 13  Iceland       private    4  37.1    TRUE       TRUE
#> 14 Portugal       private    6 138.9    TRUE       TRUE
#> 15 Portugal        public    2  23.6    TRUE       TRUE
#> 16    Spain       private    6 253.5   FALSE       TRUE
#> 17    Spain        public    2   9.2    TRUE       TRUE
#> 18       EU       private   12 392.4   FALSE       TRUE
#> 19       EU        public    4  32.8   FALSE      FALSE
#> 20    nonEU       private    4  37.1    TRUE       TRUE
#> 21       EU   Agriculture    4 240.2    TRUE       TRUE
#> 22       EU Entertainment    5 114.7   FALSE      FALSE
#> 23       EU  Governmental    4  32.8   FALSE      FALSE
#> 24       EU      Industry    3  37.5   FALSE      FALSE
#> 25    nonEU Entertainment    1  16.8    TRUE       TRUE
#> 26    nonEU      Industry    3  20.3   FALSE      FALSE

# In fact, tables_by_formulas() is also a possibility
a2 <- tables_by_formulas(SSBtoolsData("magnitude1"),
               table_fun = SuppressDominantCells, 
               table_formulas = list(table_1 = ~region * sector2, 
                                    table_2 = ~region1:sector4 - 1, 
                                    table_3 = ~region + sector4 - 1), 
               substitute_vars = list(region = c("geo", "eu"), region1 = "eu"), 
               collapse_vars = list(sector = c("sector2", "sector4")), 
               dominanceVar  = "value", 
               pPercent = 10, 
               contributorVar = "company",
               linkedGauss = "consistent") 
#> [preAggregate 20*6->20*7]
#> [extraAggregate 20*7->10*7] Checking .....
#> 
#> ====== Linked GaussSuppression by "consistent" algorithm:
#> 
#> GaussSuppression_numttHTT: ................
print(a2)                 
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
#> 12    Total      Industry    6  57.8   FALSE      FALSE   FALSE   FALSE    TRUE
#> 13  Iceland       private    4  37.1    TRUE       TRUE    TRUE   FALSE   FALSE
#> 14 Portugal       private    6 138.9    TRUE       TRUE    TRUE   FALSE   FALSE
#> 15 Portugal        public    2  23.6    TRUE       TRUE    TRUE   FALSE   FALSE
#> 16    Spain       private    6 253.5   FALSE       TRUE    TRUE   FALSE   FALSE
#> 17    Spain        public    2   9.2    TRUE       TRUE    TRUE   FALSE   FALSE
#> 18       EU       private   12 392.4   FALSE       TRUE    TRUE   FALSE   FALSE
#> 19       EU        public    4  32.8   FALSE      FALSE    TRUE   FALSE   FALSE
#> 20    nonEU       private    4  37.1    TRUE       TRUE    TRUE   FALSE   FALSE
#> 21       EU   Agriculture    4 240.2    TRUE       TRUE   FALSE    TRUE   FALSE
#> 22       EU Entertainment    5 114.7   FALSE      FALSE   FALSE    TRUE   FALSE
#> 23       EU  Governmental    4  32.8   FALSE      FALSE   FALSE    TRUE   FALSE
#> 24       EU      Industry    3  37.5   FALSE      FALSE   FALSE    TRUE   FALSE
#> 25    nonEU Entertainment    1  16.8    TRUE       TRUE   FALSE    TRUE   FALSE
#> 26    nonEU      Industry    3  20.3   FALSE      FALSE   FALSE    TRUE   FALSE
               
               
               
               
####  The second example cannot be handled using the alternative methods.
####  This is similar to the (old) LazyLinkedTables() example.

z1 <- SSBtoolsData("z1")
z2 <- SSBtoolsData("z2")
z2b <- z2[3:5]  # As in ChainedSuppression example 
names(z2b)[1] <- "region" 
# As 'f' and 'e' in ChainedSuppression example. 
# 'A' 'annet'/'arbeid' suppressed in b[[1]], since suppressed in b[[3]].
b <- SuppressLinkedTables(fun = SuppressSmallCounts,
              linkedGauss = "consistent",  
              recordAware = FALSE,
              withinArg = list(
                list(data = z1, dimVar = 1:2, freqVar = 3, maxN = 5), 
                list(data = z2b, dimVar = 1:2, freqVar = 3, maxN = 5), 
                list(data = z2, dimVar = 1:4, freqVar = 5, maxN = 1)))
#> [extend0 32*3->32*3]
#> [preAggregate 44*3->8*3]
#> [extend0 8*3->8*3]
#> [extend0 44*5->44*5]
#> 
#> ====== Linked GaussSuppression by "consistent" algorithm:
#> 
#> GaussSuppression_anySum: ............................
print(b)        
#> [[1]]
#>    region hovedint ant primary suppressed
#> 1   Total    Total 596   FALSE      FALSE
#> 2   Total    annet  72   FALSE      FALSE
#> 3   Total   arbeid  52   FALSE      FALSE
#> 4   Total soshjelp 283   FALSE      FALSE
#> 5   Total    trygd 189   FALSE      FALSE
#> 6       A    Total 113   FALSE      FALSE
#> 7       A    annet  11   FALSE       TRUE
#> 8       A   arbeid  11   FALSE       TRUE
#> 9       A soshjelp  55   FALSE      FALSE
#> 10      A    trygd  36   FALSE      FALSE
#> 11      B    Total  55   FALSE      FALSE
#> 12      B    annet   7   FALSE       TRUE
#> 13      B   arbeid   1    TRUE       TRUE
#> 14      B soshjelp  29   FALSE      FALSE
#> 15      B    trygd  18   FALSE      FALSE
#> 16      C    Total  73   FALSE      FALSE
#> 17      C    annet   5    TRUE       TRUE
#> 18      C   arbeid   8   FALSE       TRUE
#> 19      C soshjelp  35   FALSE      FALSE
#> 20      C    trygd  25   FALSE      FALSE
#> 21      D    Total  45   FALSE      FALSE
#> 22      D    annet  13   FALSE       TRUE
#> 23      D   arbeid   2    TRUE       TRUE
#> 24      D soshjelp  17   FALSE      FALSE
#> 25      D    trygd  13   FALSE      FALSE
#> 26      E    Total 138   FALSE      FALSE
#> 27      E    annet   9   FALSE      FALSE
#> 28      E   arbeid  14   FALSE      FALSE
#> 29      E soshjelp  63   FALSE      FALSE
#> 30      E    trygd  52   FALSE      FALSE
#> 31      F    Total  67   FALSE      FALSE
#> 32      F    annet  12   FALSE      FALSE
#> 33      F   arbeid   9   FALSE      FALSE
#> 34      F soshjelp  24   FALSE      FALSE
#> 35      F    trygd  22   FALSE      FALSE
#> 36      G    Total  40   FALSE      FALSE
#> 37      G    annet   6   FALSE       TRUE
#> 38      G   arbeid   4    TRUE       TRUE
#> 39      G soshjelp  22   FALSE      FALSE
#> 40      G    trygd   8   FALSE      FALSE
#> 41      H    Total  65   FALSE      FALSE
#> 42      H    annet   9   FALSE       TRUE
#> 43      H   arbeid   3    TRUE       TRUE
#> 44      H soshjelp  38   FALSE      FALSE
#> 45      H    trygd  15   FALSE      FALSE
#> 
#> [[2]]
#>    region hovedint ant primary suppressed
#> 1   Total    Total 706   FALSE      FALSE
#> 2   Total    annet  88   FALSE      FALSE
#> 3   Total   arbeid  54   FALSE      FALSE
#> 4   Total soshjelp 342   FALSE      FALSE
#> 5   Total    trygd 222   FALSE      FALSE
#> 6     300    Total 596   FALSE      FALSE
#> 7     300    annet  72   FALSE       TRUE
#> 8     300   arbeid  52   FALSE       TRUE
#> 9     300 soshjelp 283   FALSE      FALSE
#> 10    300    trygd 189   FALSE      FALSE
#> 11    400    Total 110   FALSE      FALSE
#> 12    400    annet  16   FALSE       TRUE
#> 13    400   arbeid   2    TRUE       TRUE
#> 14    400 soshjelp  59   FALSE      FALSE
#> 15    400    trygd  33   FALSE      FALSE
#> 
#> [[3]]
#>     region hovedint ant primary suppressed
#> 1        1    Total 127   FALSE      FALSE
#> 2        1    annet  14   FALSE      FALSE
#> 3        1   arbeid  11   FALSE      FALSE
#> 4        1 soshjelp  64   FALSE      FALSE
#> 5        1    trygd  38   FALSE      FALSE
#> 6       10    Total  96   FALSE      FALSE
#> 7       10    annet  13   FALSE       TRUE
#> 8       10   arbeid   2   FALSE       TRUE
#> 9       10 soshjelp  50   FALSE      FALSE
#> 10      10    trygd  31   FALSE      FALSE
#> 11     300    Total 596   FALSE      FALSE
#> 12     300    annet  72   FALSE       TRUE
#> 13     300   arbeid  52   FALSE       TRUE
#> 14     300 soshjelp 283   FALSE      FALSE
#> 15     300    trygd 189   FALSE      FALSE
#> 16       4    Total  55   FALSE      FALSE
#> 17       4    annet   7   FALSE       TRUE
#> 18       4   arbeid   1    TRUE       TRUE
#> 19       4 soshjelp  29   FALSE      FALSE
#> 20       4    trygd  18   FALSE      FALSE
#> 21     400    Total 110   FALSE      FALSE
#> 22     400    annet  16   FALSE       TRUE
#> 23     400   arbeid   2   FALSE       TRUE
#> 24     400 soshjelp  59   FALSE      FALSE
#> 25     400    trygd  33   FALSE      FALSE
#> 26       5    Total 118   FALSE      FALSE
#> 27       5    annet  18   FALSE      FALSE
#> 28       5   arbeid  10   FALSE      FALSE
#> 29       5 soshjelp  52   FALSE      FALSE
#> 30       5    trygd  38   FALSE      FALSE
#> 31       6    Total 205   FALSE      FALSE
#> 32       6    annet  21   FALSE      FALSE
#> 33       6   arbeid  23   FALSE      FALSE
#> 34       6 soshjelp  87   FALSE      FALSE
#> 35       6    trygd  74   FALSE      FALSE
#> 36       8    Total 105   FALSE      FALSE
#> 37       8    annet  15   FALSE      FALSE
#> 38       8   arbeid   7   FALSE      FALSE
#> 39       8 soshjelp  60   FALSE      FALSE
#> 40       8    trygd  23   FALSE      FALSE
#> 41   Total    Total 706   FALSE      FALSE
#> 42   Total    annet  88   FALSE      FALSE
#> 43   Total   arbeid  54   FALSE      FALSE
#> 44   Total soshjelp 342   FALSE      FALSE
#> 45   Total    trygd 222   FALSE      FALSE
#> 46       A    Total 113   FALSE      FALSE
#> 47       A    annet  11   FALSE       TRUE
#> 48       A   arbeid  11   FALSE       TRUE
#> 49       A soshjelp  55   FALSE      FALSE
#> 50       A    trygd  36   FALSE      FALSE
#> 51       B    Total  55   FALSE      FALSE
#> 52       B    annet   7   FALSE       TRUE
#> 53       B   arbeid   1    TRUE       TRUE
#> 54       B soshjelp  29   FALSE      FALSE
#> 55       B    trygd  18   FALSE      FALSE
#> 56       C    Total  73   FALSE      FALSE
#> 57       C    annet   5   FALSE       TRUE
#> 58       C   arbeid   8   FALSE       TRUE
#> 59       C soshjelp  35   FALSE      FALSE
#> 60       C    trygd  25   FALSE      FALSE
#> 61       D    Total  45   FALSE      FALSE
#> 62       D    annet  13   FALSE       TRUE
#> 63       D   arbeid   2   FALSE       TRUE
#> 64       D soshjelp  17   FALSE      FALSE
#> 65       D    trygd  13   FALSE      FALSE
#> 66       E    Total 138   FALSE      FALSE
#> 67       E    annet   9   FALSE      FALSE
#> 68       E   arbeid  14   FALSE      FALSE
#> 69       E soshjelp  63   FALSE      FALSE
#> 70       E    trygd  52   FALSE      FALSE
#> 71       F    Total  67   FALSE      FALSE
#> 72       F    annet  12   FALSE      FALSE
#> 73       F   arbeid   9   FALSE      FALSE
#> 74       F soshjelp  24   FALSE      FALSE
#> 75       F    trygd  22   FALSE      FALSE
#> 76       G    Total  40   FALSE      FALSE
#> 77       G    annet   6   FALSE       TRUE
#> 78       G   arbeid   4   FALSE       TRUE
#> 79       G soshjelp  22   FALSE      FALSE
#> 80       G    trygd   8   FALSE      FALSE
#> 81       H    Total  65   FALSE      FALSE
#> 82       H    annet   9   FALSE       TRUE
#> 83       H   arbeid   3   FALSE       TRUE
#> 84       H soshjelp  38   FALSE      FALSE
#> 85       H    trygd  15   FALSE      FALSE
#> 86       I    Total  14   FALSE      FALSE
#> 87       I    annet   3   FALSE       TRUE
#> 88       I   arbeid   0    TRUE       TRUE
#> 89       I soshjelp   9   FALSE      FALSE
#> 90       I    trygd   2   FALSE      FALSE
#> 91       J    Total  61   FALSE      FALSE
#> 92       J    annet   9   FALSE       TRUE
#> 93       J   arbeid   0    TRUE       TRUE
#> 94       J soshjelp  32   FALSE      FALSE
#> 95       J    trygd  20   FALSE      FALSE
#> 96       K    Total  35   FALSE      FALSE
#> 97       K    annet   4   FALSE      FALSE
#> 98       K   arbeid   2   FALSE      FALSE
#> 99       K soshjelp  18   FALSE      FALSE
#> 100      K    trygd  11   FALSE      FALSE
#> 
       
    
    
##################################       
####  Examples with intervals     
################################## 
  
lpPackage <- "highs" 
if (requireNamespace(lpPackage, quietly = TRUE)) {

  # Common cells occur because the default for recordAware is TRUE
  out1 <- SuppressLinkedTables(data = SSBtoolsData("magnitude1"), 
               fun = SuppressDominantCells, 
               withinArg = list(table_1 = list(dimVar = c("geo", "sector2")), 
                                table_2 = list(dimVar = c("eu", "sector4"))), 
               dominanceVar = "value", k = 90, contributorVar = "company", 
               lpPackage = lpPackage, rangeMin = 50)
  print(out1)
                      
 
 # In the algorithm, common cells occur because recordAware is TRUE, 
 # although this is not reflected in the output variables table_1 and table_2
 out2 <- tables_by_formulas(data = SSBtoolsData("magnitude1"), 
               table_fun = SuppressDominantCells, 
               table_formulas = list(table_1 = ~geo * sector2, 
                                     table_2 = ~eu * sector4), 
               substitute_vars = list(region = c("geo", "eu"), 
                                      sector = c("sector2", "sector4")), 
               dominanceVar = "value", k = 90, contributorVar = "company", 
               linkedGauss = "super-consistent", 
               lpPackage = lpPackage, rangeMin = 50, 
               linkedIntervals = c("super-consistent", "local-bdiag", "global"))
 print(out2)
                       
} else {
  message(paste0("Examples skipped because the '", lpPackage, "' package is not installed."))
}  
#> [preAggregate 20*13->13*12]
#> [extraAggregate 13*12->5*12] Checking .....
#> [preAggregate 20*13->16*12]
#> [extraAggregate 16*12->6*12] Checking .....
#> 
#> ====== Linked GaussSuppression by "super-consistent" algorithm:
#> 
#> GaussSuppression_numttHTT: ..................:::::::
#> (11*13-DDcol->11*11-0exact->9*8-GaussI->9*6)
#> 
#> Using highs for intervals...
#> --
#> (11*13-DDcol->11*11)
#> ...........
#> 7-10+8-9-
#>   9: 1 new, (425.2) 8+7-
#>   7: 2 new, (429.5) 1-5-6+
#>   5: 3 new, (162.5) 2-4-
#>   4: 4 new, (37.10) 3+2-
#>   2: 5 new, (57.80) 1-
#>   1: 6 new, (32.80) 1+
#> GaussSuppression_none: .............
#> (11*7-DDcol->11*5-0exact->10*3-GaussI->10*3)
#> 
#> Using highs for intervals...
#> --
#> $table_1
#>         geo sector2 freq value rlim_value lo_1 up_1 lo   up suppressed_integer
#> 1     Total   Total   20 462.3         NA   NA   NA NA   NA                  0
#> 2     Total private   16 429.5         NA   NA   NA NA   NA                  3
#> 3     Total  public    4  32.8         NA   NA   NA NA   NA                  3
#> 4   Iceland   Total    4  37.1         NA   NA   NA NA   NA                  3
#> 5   Iceland private    4  37.1         NA   NA   NA NA   NA                  4
#> 6   Iceland  public    0   0.0         NA   NA   NA NA   NA                  0
#> 7  Portugal   Total    8 162.5         NA   NA   NA NA   NA                  3
#> 8  Portugal private    6 138.9         NA   NA   NA NA   NA                  2
#> 9  Portugal  public    2  23.6         50    0 32.8  0 90.6                  1
#> 10    Spain   Total    8 262.7         NA   NA   NA NA   NA                  0
#> 11    Spain private    6 253.5         NA   NA   NA NA   NA                  2
#> 12    Spain  public    2   9.2         NA   NA   NA NA   NA                  2
#>    primary suppressed
#> 1    FALSE      FALSE
#> 2    FALSE       TRUE
#> 3    FALSE       TRUE
#> 4    FALSE       TRUE
#> 5    FALSE       TRUE
#> 6    FALSE      FALSE
#> 7    FALSE       TRUE
#> 8    FALSE       TRUE
#> 9     TRUE       TRUE
#> 10   FALSE      FALSE
#> 11   FALSE       TRUE
#> 12   FALSE       TRUE
#> 
#> $table_2
#>       eu       sector4 freq value rlim_value lo_1 up_1 lo    up
#> 1  Total         Total   20 462.3         NA   NA   NA NA    NA
#> 2  Total   Agriculture    4 240.2         NA   NA   NA NA    NA
#> 3  Total Entertainment    6 131.5         NA   NA   NA NA    NA
#> 4  Total  Governmental    4  32.8         NA   NA   NA NA    NA
#> 5  Total      Industry    6  57.8         NA   NA   NA NA    NA
#> 6     EU         Total   16 425.2         NA   NA   NA NA    NA
#> 7     EU   Agriculture    4 240.2         NA   NA   NA NA    NA
#> 8     EU Entertainment    5 114.7         NA   NA   NA NA    NA
#> 9     EU  Governmental    4  32.8         NA   NA   NA NA    NA
#> 10    EU      Industry    3  37.5         NA   NA   NA NA    NA
#> 11 nonEU         Total    4  37.1         NA   NA   NA NA    NA
#> 12 nonEU   Agriculture    0   0.0         NA   NA   NA NA    NA
#> 13 nonEU Entertainment    1  16.8         50    0 37.1  0 131.5
#> 14 nonEU  Governmental    0   0.0         NA   NA   NA NA    NA
#> 15 nonEU      Industry    3  20.3         NA   NA   NA NA    NA
#>    suppressed_integer primary suppressed
#> 1                   0   FALSE      FALSE
#> 2                   0   FALSE      FALSE
#> 3                   0   FALSE      FALSE
#> 4                   4   FALSE       TRUE
#> 5                   3   FALSE       TRUE
#> 6                   3   FALSE       TRUE
#> 7                   0   FALSE      FALSE
#> 8                   2   FALSE       TRUE
#> 9                   4   FALSE       TRUE
#> 10                  2   FALSE       TRUE
#> 11                  4   FALSE       TRUE
#> 12                  0   FALSE      FALSE
#> 13                  1    TRUE       TRUE
#> 14                  0   FALSE      FALSE
#> 15                  2   FALSE       TRUE
#> 
#> [preAggregate 20*6->20*7]
#> [extraAggregate 20*7->10*7] Checking .....
#> 
#> ====== Linked GaussSuppression by "super-consistent" algorithm:
#> 
#> GaussSuppression_numttHTT: ..............::::::::
#> (14*15-DDcol->14*10-DDrow->11*10->-0exact->9*8-GaussI->9*6)
#> 
#> Using highs for intervals...
#> --
#> (14*15-DDcol->14*10-DDrow->11*10->)
#> ..........
#> 6-9-10+
#>   9: 1 new, (429.5) 7-8-
#>   8: 2 new, (425.2) 7+6+1-4-5-
#>   5: 3 new, (162.5) 4+2-3-
#>   3: 4 new, (57.80) 2-
#>   2: 5 new, (37.10) 1-
#>   1: 6 new, (32.80) 1+
#> GaussSuppression_none: ..........
#> (14*5-DDcol->14*4-DDrow->11*4->-0exact->10*3-GaussI->10*3)
#> 
#> Using highs for intervals...
#> --
#> (14*6-DDcol->14*5-DDrow->7*5->-0exact->5*3-GaussI->5*3)
#> 
#> Using highs for intervals...
#> --
#> (10*5-DDcol->10*4-DDrow->8*4->-0exact->8*4-GaussI->8*4)
#> 
#> Using highs for intervals...
#> --
#>      region        sector freq value rlim_value lo_1 up_1 lo    up
#> 1     Total         Total   20 462.3         NA   NA   NA NA    NA
#> 2   Iceland         Total    4  37.1         NA   NA   NA NA    NA
#> 3  Portugal         Total    8 162.5         NA   NA   NA NA    NA
#> 4     Spain         Total    8 262.7         NA   NA   NA NA    NA
#> 5     Total       private   16 429.5         NA   NA   NA NA    NA
#> 6     Total        public    4  32.8         NA   NA   NA NA    NA
#> 7        EU         Total   16 425.2         NA   NA   NA NA    NA
#> 8     nonEU         Total    4  37.1         NA   NA   NA NA    NA
#> 9     Total   Agriculture    4 240.2         NA   NA   NA NA    NA
#> 10    Total Entertainment    6 131.5         NA   NA   NA NA    NA
#> 11    Total  Governmental    4  32.8         NA   NA   NA NA    NA
#> 12    Total      Industry    6  57.8         NA   NA   NA NA    NA
#> 13  Iceland       private    4  37.1         NA   NA   NA NA    NA
#> 14 Portugal       private    6 138.9         NA   NA   NA NA    NA
#> 15 Portugal        public    2  23.6         50    0 32.8  0  90.6
#> 16    Spain       private    6 253.5         NA   NA   NA NA    NA
#> 17    Spain        public    2   9.2         NA   NA   NA NA    NA
#> 18       EU   Agriculture    4 240.2         NA   NA   NA NA    NA
#> 19       EU Entertainment    5 114.7         NA   NA   NA NA    NA
#> 20       EU  Governmental    4  32.8         NA   NA   NA NA    NA
#> 21       EU      Industry    3  37.5         NA   NA   NA NA    NA
#> 22    nonEU Entertainment    1  16.8         50    0 37.1  0 131.5
#> 23    nonEU      Industry    3  20.3         NA   NA   NA NA    NA
#>    suppressed_integer lo_lb up_lb lo_global up_global primary suppressed
#> 1                   0    NA    NA        NA        NA   FALSE      FALSE
#> 2                   3    NA    NA        NA        NA   FALSE       TRUE
#> 3                   3    NA    NA        NA        NA   FALSE       TRUE
#> 4                   0    NA    NA        NA        NA   FALSE      FALSE
#> 5                   3    NA    NA        NA        NA   FALSE       TRUE
#> 6                   3    NA    NA        NA        NA   FALSE       TRUE
#> 7                   3    NA    NA        NA        NA   FALSE       TRUE
#> 8                   4    NA    NA        NA        NA   FALSE       TRUE
#> 9                   0    NA    NA        NA        NA   FALSE      FALSE
#> 10                  0    NA    NA        NA        NA   FALSE      FALSE
#> 11                  4    NA    NA        NA        NA   FALSE       TRUE
#> 12                  3    NA    NA        NA        NA   FALSE       TRUE
#> 13                  4    NA    NA        NA        NA   FALSE       TRUE
#> 14                  2    NA    NA        NA        NA   FALSE       TRUE
#> 15                  1     0 199.6         0      90.6    TRUE       TRUE
#> 16                  2    NA    NA        NA        NA   FALSE       TRUE
#> 17                  2    NA    NA        NA        NA   FALSE       TRUE
#> 18                  0    NA    NA        NA        NA   FALSE      FALSE
#> 19                  2    NA    NA        NA        NA   FALSE       TRUE
#> 20                  4    NA    NA        NA        NA   FALSE       TRUE
#> 21                  2    NA    NA        NA        NA   FALSE       TRUE
#> 22                  1     0 131.5         0     131.5    TRUE       TRUE
#> 23                  2    NA    NA        NA        NA   FALSE       TRUE
#>    table_1 table_2
#> 1     TRUE    TRUE
#> 2     TRUE   FALSE
#> 3     TRUE   FALSE
#> 4     TRUE   FALSE
#> 5     TRUE   FALSE
#> 6     TRUE   FALSE
#> 7    FALSE    TRUE
#> 8    FALSE    TRUE
#> 9    FALSE    TRUE
#> 10   FALSE    TRUE
#> 11   FALSE    TRUE
#> 12   FALSE    TRUE
#> 13    TRUE   FALSE
#> 14    TRUE   FALSE
#> 15    TRUE   FALSE
#> 16    TRUE   FALSE
#> 17    TRUE   FALSE
#> 18   FALSE    TRUE
#> 19   FALSE    TRUE
#> 20   FALSE    TRUE
#> 21   FALSE    TRUE
#> 22   FALSE    TRUE
#> 23   FALSE    TRUE
```
