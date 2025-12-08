# Cell suppression from input data containing inner cells

Aggregates are generated followed by primary suppression followed by
secondary suppression by Gaussian elimination by
[`GaussSuppression`](https://statisticsnorway.github.io/ssb-ssbtools/reference/GaussSuppression.html)

## Usage

``` r
GaussSuppressionFromData(
  data,
  dimVar = NULL,
  freqVar = NULL,
  ...,
  numVar = NULL,
  weightVar = NULL,
  charVar = NULL,
  hierarchies = NULL,
  formula = NULL,
  maxN = suppressWarnings(formals(c(primary)[[1]])$maxN),
  protectZeros = suppressWarnings(formals(c(primary)[[1]])$protectZeros),
  secondaryZeros = suppressWarnings(formals(candidates)$secondaryZeros),
  candidates = CandidatesDefault,
  primary = PrimaryDefault,
  forced = NULL,
  hidden = NULL,
  singleton = SingletonDefault,
  singletonMethod = ifelse(secondaryZeros, "anySumNOTprimary", "anySum"),
  printInc = TRUE,
  output = "publish",
  x = NULL,
  crossTable = NULL,
  preAggregate = is.null(freqVar),
  extraAggregate = preAggregate & !is.null(charVar),
  structuralEmpty = FALSE,
  extend0 = FALSE,
  spec = NULL,
  specLock = FALSE,
  freqVarNew = rev(make.unique(c(names(data), "freq")))[1],
  nUniqueVar = rev(make.unique(c(names(data), "nUnique")))[1],
  forcedInOutput = "ifNonNULL",
  unsafeInOutput = "ifForcedInOutput",
  lpPackage = NULL,
  intervalSuppression = TRUE,
  aggregatePackage = "base",
  aggregateNA = TRUE,
  aggregateBaseOrder = FALSE,
  rowGroupsPackage = aggregatePackage,
  linkedGauss = NULL,
  linkedIntervals = ifelse(linkedGauss == "local-bdiag", "local-bdiag",
    "super-consistent"),
  recordAware = TRUE,
  collapseAware = FALSE,
  linkedTables = NULL,
  da_vars = NULL,
  da_fun = NULL,
  da_args = NULL,
  action_unused_dots = getOption("GaussSuppression.action_unused_dots", "warn"),
  allowed_unused_dots = getOption("GaussSuppression.allowed_unused_dots", character(0))
)
```

## Arguments

- data:

  Input data, typically a data frame, tibble, or data.table. If `data`
  is not a classic data frame, it will be coerced to one internally
  unless `preAggregate` is `TRUE` and `aggregatePackage` is
  `"data.table"`.

- dimVar:

  The main dimensional variables and additional aggregating variables.
  This parameter can be useful when hierarchies and formula are
  unspecified.

- freqVar:

  A single variable holding counts (name or number).

- ...:

  Further arguments to be passed to the supplied functions and to
  [`ModelMatrix`](https://statisticsnorway.github.io/ssb-ssbtools/reference/ModelMatrix.html)
  (such as `inputInOutput` and `removeEmpty`).

- numVar:

  Other numerical variables to be aggregated

- weightVar:

  weightVar Weights (costs) to be used to order candidates for secondary
  suppression

- charVar:

  Other variables possibly to be used within the supplied functions

- hierarchies:

  List of hierarchies, which can be converted by
  [`AutoHierarchies`](https://statisticsnorway.github.io/ssb-ssbtools/reference/AutoHierarchies.html).
  Thus, the variables can also be coded by `"rowFactor"` or `""`, which
  correspond to using the categories in the data.

- formula:

  A model formula

- maxN:

  Suppression parameter forwarded to the supplied functions. With the
  default `primary` function,
  [`PrimaryDefault()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/PrimaryDefault.md),
  cells with frequency `<= maxN` are marked as primary suppressed, and
  the default value of `maxN` is `3`. See details below. The parameter
  is also used by
  [`NContributorsRule()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/NContributorsRule.md).
  For advanced use cases, including setups with multiple primary
  functions, `maxN` can be specified as a named list or vector. See each
  primary function’s documentation for details.

- protectZeros:

  Suppression parameter. When `TRUE`, cells with zero frequency or value
  are set as primary suppressed. Using the default `primary` function,
  `protectZeros` is by default set to `TRUE`. See details.

- secondaryZeros:

  Suppression parameter. When `TRUE`, cells with zero frequency or value
  are prioritized to be published so that they are not secondary
  suppressed. Using the default `candidates` function, `secondaryZeros`
  is by default set to `FALSE`. See details.

- candidates:

  GaussSuppression input or a function generating it (see details)
  Default:
  [`CandidatesDefault`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/CandidatesDefault.md)

- primary:

  GaussSuppression input or a function generating it (see details)
  Default:
  [`PrimaryDefault`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/PrimaryDefault.md)

- forced:

  GaussSuppression input or a function generating it (see details)

- hidden:

  GaussSuppression input or a function generating it (see details)

- singleton:

  GaussSuppression input or a function generating it (see details)
  Default:
  [`SingletonDefault`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/SingletonDefault.md)

- singletonMethod:

  [`GaussSuppression`](https://statisticsnorway.github.io/ssb-ssbtools/reference/GaussSuppression.html)
  input. The default value depends on parameter `secondaryZeros` which
  depends on `candidates` (see details).

- printInc:

  [`GaussSuppression`](https://statisticsnorway.github.io/ssb-ssbtools/reference/GaussSuppression.html)
  input

- output:

  One of `"publish"` (default), `"inner"`, `"publish_inner"`,
  `"publish_inner_x"`, `"publish_x"`, `"inner_x"`, `"input2functions"`
  (input to supplied functions), `"inputGaussSuppression"`,
  `"inputGaussSuppression_x"`, `"outputGaussSuppression"`
  `"outputGaussSuppression_x"`, `"primary"`, `"secondary"` and `"all"`.
  Here "inner" means input data (possibly pre-aggregated) and "x" means
  dummy matrix (as input parameter x). All input to and output from
  [`GaussSuppression`](https://statisticsnorway.github.io/ssb-ssbtools/reference/GaussSuppression.html),
  except `...`, are returned when `"outputGaussSuppression_x"`.
  Excluding x and only input are also possible. The code `"all"` means
  all relevant output after all the calculations. Currently, this means
  the same as `"publish_inner_x"` extended with the matrices (or NULL)
  `xExtraPrimary` and `unsafe`. The former matrix is usually made by
  [`KDisclosurePrimary`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/KDisclosurePrimary.md).
  This latter matrix contains the columns representing unsafe primary
  suppressions. In addition to `x` columns corresponding to unsafe in
  ordinary output (see parameter `unsafeInOutput` below), possible
  columns from `xExtraPrimary` may also be included in the unsafe matrix
  (see details).

- x:

  `x` (`modelMatrix`) and `crossTable` can be supplied as input instead
  of generating it from
  [`ModelMatrix`](https://statisticsnorway.github.io/ssb-ssbtools/reference/ModelMatrix.html)

- crossTable:

  See above.

- preAggregate:

  When `TRUE`, the data will be aggregated within the function to an
  appropriate level. This is defined by the dimensional variables
  according to `dimVar`, `hierarchies` or `formula` and in addition
  `charVar`. When `FALSE`, no aggregation is performed. When `NA`, the
  function will automatically decide whether to aggregate: aggregation
  is applied unless `freqVar` is present and the data contain no
  duplicated rows with respect to the dimensional variables and
  `charVar`. Exception: if a non-`NULL` `x` (the model matrix) is
  supplied, `NA` is treated as `FALSE`.

- extraAggregate:

  When `TRUE`, the data will be aggregated by the dimensional variables
  according to `dimVar`, `hierarchies` or `formula`. The aggregated data
  and the corresponding x-matrix will only be used as input to the
  singleton function and
  [`GaussSuppression`](https://statisticsnorway.github.io/ssb-ssbtools/reference/GaussSuppression.html).
  This extra aggregation is useful when parameter `charVar` is used.
  Supply `"publish_inner"`, `"publish_inner_x"`, `"publish_x"` or
  `"inner_x"` as `output` to obtain extra aggregated results. Supply
  `"inner"` or `"input2functions"` to obtain other results.

- structuralEmpty:

  When `TRUE`, output cells with no contributing inner cells (only zeros
  in column of `x`) are forced to be not primary suppressed. Thus, these
  cells are considered as structural zeros. When `structuralEmpty` is
  `TRUE`, the following error message is avoided: `Suppressed` `cells`
  `with` `empty` `input` `will` `not` `be` `protected.` `Extend` `input`
  `data` `with` `zeros?`. When `removeEmpty` is `TRUE` (see "`...`"
  below), `structuralEmpty` is superfluous

- extend0:

  Data is automatically extended by `Extend0` when `TRUE`. Can also be
  set to `"all"` which means that input codes in hierarchies are
  considered in addition to those in data. Parameter `extend0` can also
  be specified as a list meaning parameter `varGroups` to `Extend0`.

- spec:

  `NULL` or a named list of arguments that will act as default values.

- specLock:

  When `TRUE`, arguments in `spec` cannot be changed.

- freqVarNew:

  Name of new frequency variable generated when input `freqVar` is NULL
  and `preAggregate` is TRUE. Default is `"freq"` provided this is not
  found in `names(data)`.

- nUniqueVar:

  Name of variable holding the number of unique contributors. This
  variable will be generated in the `extraAggregate` step. Default is
  `"nUnique"` provided this is not found in `names(data)`. If an
  existing variable is passed as input, this variable will apply only
  when `preAggregate`/`extraAggregate` is not done.

- forcedInOutput:

  Whether to include `forced` as an output column. One of `"ifNonNULL"`
  (default), `"always"`, `"ifany"` and `"no"`. In addition, `TRUE` and
  `FALSE` are allowed as alternatives to `"always"` and `"no"`.

- unsafeInOutput:

  Whether to include `usafe` as an output column. One of
  `"ifForcedInOutput"` (default), `"always"`, `"ifany"` and `"no"`. In
  addition, `TRUE` and `FALSE` are allowed as alternatives to `"always"`
  and `"no"`. see details.

- lpPackage:

  - When non-NULL, intervals computed by
    [`ComputeIntervals()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/ComputeIntervals.md)
    will be included in the output. Valid values are the names of
    supported R packages for linear programming backends: `"highs"`,
    `"Rsymphony"`, `"Rglpk"`, or `"lpSolve"`.

  - If interval requirements are specified, additional suppression will
    be performed to satisfy those requirements. Interval requirements
    can be set either through arguments of
    [`IntervalLimits()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/IntervalLimits.md)
    or by enabling `protectionIntervals = TRUE` in the primary
    suppression functions. See
    [`IntervalLimits()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/IntervalLimits.md)
    for a full description of the parameters (`protectionPercent`,
    `protectionLimit`, `loProtectionPercent`, `loProtectionLimit`,
    `rangePercent`, `rangeMin`) and how interval requirements are
    calculated.

    - In the output variable `suppressed_integer`, suppression status is
      coded as: 0 = no suppression, 1 = primary suppression, 2 =
      secondary suppression, 3 = additional suppression applied by an
      interval algorithm limited to linearly independent cells, 4 =
      further suppression according to the final gauss algorithm.

    - Intervals `[lo_1, up_1]` are calculated prior to additional
      suppression.

    - To disable additional suppression, set
      `intervalSuppression = FALSE`.

                   Please note that additional suppression based on parameters other than
                   rangePercent and rangeMin is currently considered experimental.
                   In particular, the names of the newer parameters may still change.

- intervalSuppression:

  Logical. If `FALSE`, additional suppression to satisfy interval
  requirements is disabled (default is `TRUE`). See description of
  `lpPackage` above.

- aggregatePackage:

  Package used to preAggregate/extraAggregate. Parameter `pkg` to
  [`aggregate_by_pkg`](https://statisticsnorway.github.io/ssb-ssbtools/reference/aggregate_by_pkg.html).

- aggregateNA:

  Whether to include NAs in the grouping variables while
  preAggregate/extraAggregate. Parameter `include_na` to
  [`aggregate_by_pkg`](https://statisticsnorway.github.io/ssb-ssbtools/reference/aggregate_by_pkg.html).
  Note that NAs will not be present in the output table's dimensions
  regardless of the value of `aggregateNA`. When using the formula
  interface, this is controlled by the `NAomit` parameter (default
  `TRUE`), which is passed to the function
  [`SSBtools::Formula2ModelMatrix()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/FormulaSums.html).
  It is through this use of the formula interface that NAs in the input
  data make sense. Note that under normal circumstances, grouping
  variables should not use NA to represent a category. As such, if NAs
  are present in the grouping variables, using the `dimVar` or
  `hierarchies` interfaces will result in errors.

- aggregateBaseOrder:

  Parameter `base_order` to
  [`aggregate_by_pkg`](https://statisticsnorway.github.io/ssb-ssbtools/reference/aggregate_by_pkg.html),
  used when preAggregate/extraAggregate. The parameter does not affect
  the ordering of ordinary output. Therefore, the default is set to
  `FALSE` to avoid unnecessary sorting operations. The parameter will
  have impact when, e.g `output = "inner"`.

- rowGroupsPackage:

  Parameter `pkg` to
  [`RowGroups`](https://statisticsnorway.github.io/ssb-ssbtools/reference/RowGroups.html).
  The parameter is input to
  [`Formula2ModelMatrix`](https://statisticsnorway.github.io/ssb-ssbtools/reference/FormulaSums.html)
  via
  [`ModelMatrix`](https://statisticsnorway.github.io/ssb-ssbtools/reference/ModelMatrix.html).

- linkedGauss:

  Controls linked table suppression. Accepted values are described in
  the documentation for
  [`SuppressLinkedTables()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/SuppressLinkedTables.md).
  See also the note and the corresponding examples, which demonstrate
  usage with alternative function interfaces. In addition,
  `linkedGauss = "global"` is allowed and corresponds to standard
  execution (i.e., when `linkedGauss` is not specified). When
  `linkedGauss` is used, the `formula` parameter should be provided as a
  list of formulas. Alternatively, `formula` may have an attribute
  `"table_formulas"` containing such a list. See also the `linkedTables`
  parameter below.

- linkedIntervals:

  Determines how interval calculations, triggered by the `lpPackage`
  parameter, are performed when `linkedGauss` is not `"global"`. When
  `linkedGauss = "global"`, interval settings in `linkedIntervals` are
  ignored. For allowed values and detailed behaviour, see the
  documentation of
  [`SuppressLinkedTables()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/SuppressLinkedTables.md).

  - Note: With `linkedIntervals = "local-bdiag"`, common cells may have
    different table-specific intervals. Since the output shows one
    interval per cell, it is constructed using the maximum lower bound
    and minimum upper bound across the tables.

- recordAware:

  Parameter associated with `linkedGauss`. See
  [`SuppressLinkedTables()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/SuppressLinkedTables.md).

- collapseAware:

  Parameter associated with `linkedGauss`. In the linked‑tables
  algorithm, the model matrix is first *collapsed* by removing duplicate
  rows. When `collapseAware = TRUE`, every cell that remains numerically
  derivable after a pre‑aggregation corresponding to this row reduction
  will be treated as a common cell. This maximizes coordination across
  tables, given the duplicate‑row removal, while adding limited
  additional computational overhead. In particular, the suppression
  algorithm automatically accounts for cells in one table that are sums
  of cells in another table. Note that any cell that
  `recordAware = TRUE` would introduce is already included automatically
  when `collapseAware = TRUE`.

- linkedTables:

  A list specifying how the tables referenced in the `formula` parameter
  should be combined for use in the linked-tables algorithm. Each
  element in the list contains one or more names of the tables in
  `formula`. The corresponding tables will be combined and treated as a
  single table by the algorithm. For example:
  `linkedTables = list(c("table_1", "table_3"), "table_2")`. If `NULL`
  (default), each table in `formula` is used individually.

- da_vars:

  The `vars` argument passed to
  [`SSBtools::dummy_aggregate()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/dummy_aggregate.html).
  Together with the two parameters below, this enables computing results
  via
  [`SSBtools::aggregate_multiple_fun()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/aggregate_multiple_fun.html)
  in the same way as when using
  [`SSBtools::model_aggregate()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/model_aggregate.html).
  The calculations are performed by calling
  [`SSBtools::dummy_aggregate()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/dummy_aggregate.html)
  with the model matrix (`x`) before any potential use of
  `extraAggregate`. Internally, the result is stored in a data frame
  named `da_out`, which is available to the supplied functions in the
  same manner as `num`. The columns of `da_out` are added to the final
  output. See
  [`SuppressDominantCells()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/SuppressDominantCells.md)
  examples.

- da_fun:

  The `fun` argument passed to
  [`SSBtools::dummy_aggregate()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/dummy_aggregate.html).

- da_args:

  A list of additional arguments passed to
  [`SSBtools::dummy_aggregate()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/dummy_aggregate.html).

- action_unused_dots:

  Character string controlling how unused arguments in `...` are
  handled. Internally uses
  [`ellipsis::check_dots_used()`](https://rlang.r-lib.org/reference/check_dots_used.html)
  with a custom action. One of "warn", "abort", "inform", or "none". The
  value "none" disables the check entirely. The default is taken from
  `getOption("GaussSuppression.action_unused_dots")`, falling back to
  "warn" if the option is not set. Users can change the default globally
  with e.g. `options(GaussSuppression.action_unused_dots = "abort")`.

- allowed_unused_dots:

  Character vector of argument names ignored by the unused-argument
  check. May be useful when this function is wrapped by another
  function, or in other cases where a correctly spelled argument is
  nevertheless not registered as used. The default is taken from
  `getOption("GaussSuppression.allowed_unused_dots")`, falling back to
  `character(0)` if the option is not set. Users can change the default
  globally with e.g.
  `options(GaussSuppression.allowed_unused_dots = c("plotColor", "lineType"))`.

## Value

Aggregated data with suppression information

## Details

The supplied functions for generating
[`GaussSuppression`](https://statisticsnorway.github.io/ssb-ssbtools/reference/GaussSuppression.html)
input takes the following arguments: `crossTable`, `x`, `freq`, `num`,
`weight`, `maxN`, `protectZeros`, `secondaryZeros`, `data`, `freqVar`,
`numVar`, `weightVar`, `charVar`, `dimVar` `aggregatePackage`,
`aggregateNA`, `aggregateBaseOrder`, `rowGroupsPackage`,
`structuralEmpty`, `da_out`, and `...`. where the two first are
[`ModelMatrix`](https://statisticsnorway.github.io/ssb-ssbtools/reference/ModelMatrix.html)
outputs (`modelMatrix` renamed to `x`). The vector, `freq`, is
aggregated counts (`t(x) %*% data[[freqVar]]`). In addition, the
supplied `singleton` function also takes `nUniqueVar` and (output from)
`primary` as input.

Similarly, `num`, is a data frame of aggregated numerical variables. It
is possible to supply several primary functions joined by `c`, e.g.
(`c(FunPrim1, FunPrim2)`). All `NA`s returned from any of the functions
force the corresponding cells not to be primary suppressed.

The effect of `maxN` , `protectZeros` and `secondaryZeros` depends on
the supplied functions where these parameters are used. Their default
values are inherited from the default values of the first `primary`
function (several possible) or, in the case of `secondaryZeros`, the
`candidates` function. When defaults cannot be inherited, they are set
to `NULL`. In practice the function `formals` are still used to generate
the defaults when `primary` and/or `candidates` are not functions. Then
`NULL` is correctly returned, but `suppressWarnings` are needed.

Singleton handling can be turned off by `singleton = NULL` or
`singletonMethod = "none"`. Both of these choices are identical in the
sense that `singletonMethod` is set to `"none"` whenever `singleton` is
`NULL` and vice versa.

Information about uncertain primary suppressions due to forced cells can
be found as described by parameters `unsafeInOutput` and `output`
(`= "all"`). When forced cells affect singleton problems, this is not
implemented. Some information can be seen from warnings. This can also
be seen by choosing `output = "secondary"` together with
`unsafeInOutput = "ifany"` or `unsafeInOutput = "always"`. Then,
negative indices from
[`GaussSuppression`](https://statisticsnorway.github.io/ssb-ssbtools/reference/GaussSuppression.html)
using `unsafeAsNegative = TRUE` will be included in the output.
Singleton problems may, however, be present even if it cannot be seen as
warning/output. In some cases, the problems can be detected by
[`GaussSuppressDec`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/GaussSuppressDec.md).

In some cases, cells that are forced, hidden, or primary suppressed can
overlap. For these situations, forced has precedence over hidden and
primary. That is, if a cell is both forced and hidden, it will be
treated as a forced cell and thus published. Similarly, any primary
suppression of a forced cell will be ignored (see parameter
`whenPrimaryForced` to
[`GaussSuppression`](https://statisticsnorway.github.io/ssb-ssbtools/reference/GaussSuppression.html)).
It is, however, meaningful to combine primary and hidden. Such cells
will be protected while also being assigned the `NA` value in the
`suppressed` output variable.

## Author

Øyvind Langsrud and Daniel Lupp

## Examples

``` r
z1 <- SSBtoolsData("z1")
GaussSuppressionFromData(z1, 1:2, 3)
#> GaussSuppression_anySum: ..........................................
#>    region hovedint ant primary suppressed
#> 1   Total    Total 596   FALSE      FALSE
#> 2   Total    annet  72   FALSE      FALSE
#> 3   Total   arbeid  52   FALSE      FALSE
#> 4   Total soshjelp 283   FALSE      FALSE
#> 5   Total    trygd 189   FALSE      FALSE
#> 6       A    Total 113   FALSE      FALSE
#> 7       A    annet  11   FALSE      FALSE
#> 8       A   arbeid  11   FALSE      FALSE
#> 9       A soshjelp  55   FALSE      FALSE
#> 10      A    trygd  36   FALSE      FALSE
#> 11      B    Total  55   FALSE      FALSE
#> 12      B    annet   7   FALSE       TRUE
#> 13      B   arbeid   1    TRUE       TRUE
#> 14      B soshjelp  29   FALSE      FALSE
#> 15      B    trygd  18   FALSE      FALSE
#> 16      C    Total  73   FALSE      FALSE
#> 17      C    annet   5   FALSE      FALSE
#> 18      C   arbeid   8   FALSE      FALSE
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
#> 37      G    annet   6   FALSE      FALSE
#> 38      G   arbeid   4   FALSE      FALSE
#> 39      G soshjelp  22   FALSE      FALSE
#> 40      G    trygd   8   FALSE      FALSE
#> 41      H    Total  65   FALSE      FALSE
#> 42      H    annet   9   FALSE       TRUE
#> 43      H   arbeid   3    TRUE       TRUE
#> 44      H soshjelp  38   FALSE      FALSE
#> 45      H    trygd  15   FALSE      FALSE

z2 <- SSBtoolsData("z2")
GaussSuppressionFromData(z2, 1:4, 5, protectZeros = FALSE)
#> GaussSuppression_anySum: .............................
#>     region hovedint ant primary suppressed
#> 1        1    Total 127   FALSE      FALSE
#> 2        1    annet  14   FALSE      FALSE
#> 3        1   arbeid  11   FALSE      FALSE
#> 4        1 soshjelp  64   FALSE      FALSE
#> 5        1    trygd  38   FALSE      FALSE
#> 6       10    Total  96   FALSE      FALSE
#> 7       10    annet  13   FALSE      FALSE
#> 8       10   arbeid   2    TRUE       TRUE
#> 9       10 soshjelp  50   FALSE      FALSE
#> 10      10    trygd  31   FALSE       TRUE
#> 11     300    Total 596   FALSE      FALSE
#> 12     300    annet  72   FALSE       TRUE
#> 13     300   arbeid  52   FALSE       TRUE
#> 14     300 soshjelp 283   FALSE      FALSE
#> 15     300    trygd 189   FALSE      FALSE
#> 16       4    Total  55   FALSE      FALSE
#> 17       4    annet   7   FALSE      FALSE
#> 18       4   arbeid   1    TRUE       TRUE
#> 19       4 soshjelp  29   FALSE      FALSE
#> 20       4    trygd  18   FALSE       TRUE
#> 21     400    Total 110   FALSE      FALSE
#> 22     400    annet  16   FALSE       TRUE
#> 23     400   arbeid   2    TRUE       TRUE
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
#> 48       A   arbeid  11   FALSE      FALSE
#> 49       A soshjelp  55   FALSE      FALSE
#> 50       A    trygd  36   FALSE       TRUE
#> 51       B    Total  55   FALSE      FALSE
#> 52       B    annet   7   FALSE      FALSE
#> 53       B   arbeid   1    TRUE       TRUE
#> 54       B soshjelp  29   FALSE      FALSE
#> 55       B    trygd  18   FALSE       TRUE
#> 56       C    Total  73   FALSE      FALSE
#> 57       C    annet   5   FALSE       TRUE
#> 58       C   arbeid   8   FALSE       TRUE
#> 59       C soshjelp  35   FALSE      FALSE
#> 60       C    trygd  25   FALSE      FALSE
#> 61       D    Total  45   FALSE      FALSE
#> 62       D    annet  13   FALSE       TRUE
#> 63       D   arbeid   2    TRUE       TRUE
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
#> 83       H   arbeid   3    TRUE       TRUE
#> 84       H soshjelp  38   FALSE      FALSE
#> 85       H    trygd  15   FALSE      FALSE
#> 86       I    Total  14   FALSE      FALSE
#> 87       I    annet   3    TRUE       TRUE
#> 88       I   arbeid   0   FALSE      FALSE
#> 89       I soshjelp   9   FALSE      FALSE
#> 90       I    trygd   2    TRUE       TRUE
#> 91       J    Total  61   FALSE      FALSE
#> 92       J    annet   9   FALSE      FALSE
#> 93       J   arbeid   0   FALSE      FALSE
#> 94       J soshjelp  32   FALSE      FALSE
#> 95       J    trygd  20   FALSE      FALSE
#> 96       K    Total  35   FALSE      FALSE
#> 97       K    annet   4   FALSE      FALSE
#> 98       K   arbeid   2    TRUE       TRUE
#> 99       K soshjelp  18   FALSE      FALSE
#> 100      K    trygd  11   FALSE       TRUE


# Data as in GaussSuppression examples
df <- data.frame(values = c(1, 1, 1, 5, 5, 9, 9, 9, 9, 9, 0, 0, 0, 7, 7), 
                 var1 = rep(1:3, each = 5), var2 = c("A", "B", "C", "D", "E"))

GaussSuppressionFromData(df, c("var1", "var2"), "values")
#> GaussSuppression_anySum: ..................
#>     var1  var2 values primary suppressed
#> 1  Total Total     72   FALSE      FALSE
#> 2  Total     A     10   FALSE      FALSE
#> 3  Total     B     10   FALSE      FALSE
#> 4  Total     C     10   FALSE      FALSE
#> 5  Total     D     21   FALSE      FALSE
#> 6  Total     E     21   FALSE      FALSE
#> 7      1 Total     13   FALSE      FALSE
#> 8      1     A      1    TRUE       TRUE
#> 9      1     B      1    TRUE       TRUE
#> 10     1     C      1    TRUE       TRUE
#> 11     1     D      5   FALSE      FALSE
#> 12     1     E      5   FALSE       TRUE
#> 13     2 Total     45   FALSE      FALSE
#> 14     2     A      9   FALSE      FALSE
#> 15     2     B      9   FALSE      FALSE
#> 16     2     C      9   FALSE      FALSE
#> 17     2     D      9   FALSE      FALSE
#> 18     2     E      9   FALSE      FALSE
#> 19     3 Total     14   FALSE      FALSE
#> 20     3     A      0    TRUE       TRUE
#> 21     3     B      0    TRUE       TRUE
#> 22     3     C      0    TRUE       TRUE
#> 23     3     D      7   FALSE      FALSE
#> 24     3     E      7   FALSE       TRUE
GaussSuppressionFromData(df, c("var1", "var2"), "values", formula = ~var1 + var2, maxN = 10)
#> GaussSuppression_anySum: ......
#>    var1  var2 values primary suppressed
#> 1 Total Total     72   FALSE      FALSE
#> 2     1 Total     13   FALSE      FALSE
#> 3     2 Total     45   FALSE      FALSE
#> 4     3 Total     14   FALSE      FALSE
#> 5 Total     A     10    TRUE       TRUE
#> 6 Total     B     10    TRUE       TRUE
#> 7 Total     C     10    TRUE       TRUE
#> 8 Total     D     21   FALSE      FALSE
#> 9 Total     E     21   FALSE      FALSE
GaussSuppressionFromData(df, c("var1", "var2"), "values", formula = ~var1 + var2, maxN = 10,
      protectZeros = TRUE, # Parameter needed by SingletonDefault and default not in primary  
      primary = function(freq, crossTable, maxN, ...) 
                   which(freq <= maxN & crossTable[[2]] != "A" & crossTable[, 2] != "C"))
#> GaussSuppression_anySum: ........
#>    var1  var2 values primary suppressed
#> 1 Total Total     72   FALSE      FALSE
#> 2     1 Total     13   FALSE      FALSE
#> 3     2 Total     45   FALSE      FALSE
#> 4     3 Total     14   FALSE      FALSE
#> 5 Total     A     10   FALSE      FALSE
#> 6 Total     B     10    TRUE       TRUE
#> 7 Total     C     10   FALSE       TRUE
#> 8 Total     D     21   FALSE      FALSE
#> 9 Total     E     21   FALSE      FALSE
                   
# Combining several primary functions 
# Note that NA & c(TRUE, FALSE) equals c(NA, FALSE)                      
GaussSuppressionFromData(df, c("var1", "var2"), "values", formula = ~var1 + var2, maxN = 10, 
       primary = c(function(freq, maxN, protectZeros = TRUE, ...) freq >= 45,
                   function(freq, maxN, ...) freq <= maxN,
                   function(crossTable, ...) NA & crossTable[[2]] == "C",  
                   function(crossTable, ...) NA & crossTable[[1]]== "Total" 
                                                & crossTable[[2]]== "Total"))                    
#> GaussSuppression_anySum: ......
#>    var1  var2 values primary suppressed
#> 1 Total Total     72   FALSE      FALSE
#> 2     1 Total     13   FALSE       TRUE
#> 3     2 Total     45    TRUE       TRUE
#> 4     3 Total     14   FALSE      FALSE
#> 5 Total     A     10    TRUE       TRUE
#> 6 Total     B     10    TRUE       TRUE
#> 7 Total     C     10   FALSE      FALSE
#> 8 Total     D     21   FALSE      FALSE
#> 9 Total     E     21   FALSE      FALSE
                   
# Similar to GaussSuppression examples
GaussSuppressionFromData(df, c("var1", "var2"), "values", formula = ~var1 * var2, 
       candidates = NULL, singleton = NULL, protectZeros = FALSE, secondaryZeros = TRUE)
#> GaussSuppression_none: .....................
#>     var1  var2 values primary suppressed
#> 1  Total Total     72   FALSE      FALSE
#> 2      1 Total     13   FALSE      FALSE
#> 3      2 Total     45   FALSE      FALSE
#> 4      3 Total     14   FALSE      FALSE
#> 5  Total     A     10   FALSE      FALSE
#> 6  Total     B     10   FALSE      FALSE
#> 7  Total     C     10   FALSE      FALSE
#> 8  Total     D     21   FALSE      FALSE
#> 9  Total     E     21   FALSE      FALSE
#> 10     1     A      1    TRUE       TRUE
#> 11     1     B      1    TRUE       TRUE
#> 12     1     C      1    TRUE       TRUE
#> 13     1     D      5   FALSE      FALSE
#> 14     1     E      5   FALSE      FALSE
#> 15     2     A      9   FALSE      FALSE
#> 16     2     B      9   FALSE      FALSE
#> 17     2     C      9   FALSE      FALSE
#> 18     2     D      9   FALSE      FALSE
#> 19     2     E      9   FALSE      FALSE
#> 20     3     A      0   FALSE       TRUE
#> 21     3     B      0   FALSE       TRUE
#> 22     3     C      0   FALSE       TRUE
#> 23     3     D      7   FALSE      FALSE
#> 24     3     E      7   FALSE      FALSE
GaussSuppressionFromData(df, c("var1", "var2"), "values", formula = ~var1 * var2, 
       singleton = NULL, protectZeros = FALSE, secondaryZeros = FALSE)
#> GaussSuppression_none: .....................
#>     var1  var2 values primary suppressed
#> 1  Total Total     72   FALSE      FALSE
#> 2      1 Total     13   FALSE      FALSE
#> 3      2 Total     45   FALSE      FALSE
#> 4      3 Total     14   FALSE      FALSE
#> 5  Total     A     10   FALSE      FALSE
#> 6  Total     B     10   FALSE      FALSE
#> 7  Total     C     10   FALSE      FALSE
#> 8  Total     D     21   FALSE      FALSE
#> 9  Total     E     21   FALSE      FALSE
#> 10     1     A      1    TRUE       TRUE
#> 11     1     B      1    TRUE       TRUE
#> 12     1     C      1    TRUE       TRUE
#> 13     1     D      5   FALSE      FALSE
#> 14     1     E      5   FALSE      FALSE
#> 15     2     A      9   FALSE       TRUE
#> 16     2     B      9   FALSE       TRUE
#> 17     2     C      9   FALSE       TRUE
#> 18     2     D      9   FALSE      FALSE
#> 19     2     E      9   FALSE      FALSE
#> 20     3     A      0   FALSE      FALSE
#> 21     3     B      0   FALSE      FALSE
#> 22     3     C      0   FALSE      FALSE
#> 23     3     D      7   FALSE      FALSE
#> 24     3     E      7   FALSE      FALSE
GaussSuppressionFromData(df, c("var1", "var2"), "values", formula = ~var1 * var2, 
       protectZeros = FALSE, secondaryZeros = FALSE)
#> GaussSuppression_anySum: .....................
#>     var1  var2 values primary suppressed
#> 1  Total Total     72   FALSE      FALSE
#> 2      1 Total     13   FALSE      FALSE
#> 3      2 Total     45   FALSE      FALSE
#> 4      3 Total     14   FALSE      FALSE
#> 5  Total     A     10   FALSE      FALSE
#> 6  Total     B     10   FALSE      FALSE
#> 7  Total     C     10   FALSE      FALSE
#> 8  Total     D     21   FALSE      FALSE
#> 9  Total     E     21   FALSE      FALSE
#> 10     1     A      1    TRUE       TRUE
#> 11     1     B      1    TRUE       TRUE
#> 12     1     C      1    TRUE       TRUE
#> 13     1     D      5   FALSE      FALSE
#> 14     1     E      5   FALSE       TRUE
#> 15     2     A      9   FALSE       TRUE
#> 16     2     B      9   FALSE       TRUE
#> 17     2     C      9   FALSE       TRUE
#> 18     2     D      9   FALSE      FALSE
#> 19     2     E      9   FALSE       TRUE
#> 20     3     A      0   FALSE      FALSE
#> 21     3     B      0   FALSE      FALSE
#> 22     3     C      0   FALSE      FALSE
#> 23     3     D      7   FALSE      FALSE
#> 24     3     E      7   FALSE      FALSE

              
# Examples with zeros as singletons
z <- data.frame(row = rep(1:3, each = 3), col = 1:3, freq = c(0, 2, 5, 0, 0, 6:9))
GaussSuppressionFromData(z, 1:2, 3, singleton = NULL) 
#> GaussSuppression_none: ............
#>      row   col freq primary suppressed
#> 1  Total Total   37   FALSE      FALSE
#> 2  Total     1    7   FALSE      FALSE
#> 3  Total     2   10   FALSE      FALSE
#> 4  Total     3   20   FALSE      FALSE
#> 5      1 Total    7   FALSE      FALSE
#> 6      1     1    0    TRUE       TRUE
#> 7      1     2    2    TRUE       TRUE
#> 8      1     3    5   FALSE      FALSE
#> 9      2 Total    6   FALSE      FALSE
#> 10     2     1    0    TRUE       TRUE
#> 11     2     2    0    TRUE       TRUE
#> 12     2     3    6   FALSE      FALSE
#> 13     3 Total   24   FALSE      FALSE
#> 14     3     1    7   FALSE      FALSE
#> 15     3     2    8   FALSE      FALSE
#> 16     3     3    9   FALSE      FALSE
GaussSuppressionFromData(z, 1:2, 3, singletonMethod = "none") # as above 
#> GaussSuppression_none: ............
#>      row   col freq primary suppressed
#> 1  Total Total   37   FALSE      FALSE
#> 2  Total     1    7   FALSE      FALSE
#> 3  Total     2   10   FALSE      FALSE
#> 4  Total     3   20   FALSE      FALSE
#> 5      1 Total    7   FALSE      FALSE
#> 6      1     1    0    TRUE       TRUE
#> 7      1     2    2    TRUE       TRUE
#> 8      1     3    5   FALSE      FALSE
#> 9      2 Total    6   FALSE      FALSE
#> 10     2     1    0    TRUE       TRUE
#> 11     2     2    0    TRUE       TRUE
#> 12     2     3    6   FALSE      FALSE
#> 13     3 Total   24   FALSE      FALSE
#> 14     3     1    7   FALSE      FALSE
#> 15     3     2    8   FALSE      FALSE
#> 16     3     3    9   FALSE      FALSE
GaussSuppressionFromData(z, 1:2, 3)
#> GaussSuppression_anySum: ............
#>      row   col freq primary suppressed
#> 1  Total Total   37   FALSE      FALSE
#> 2  Total     1    7   FALSE      FALSE
#> 3  Total     2   10   FALSE      FALSE
#> 4  Total     3   20   FALSE      FALSE
#> 5      1 Total    7   FALSE      FALSE
#> 6      1     1    0    TRUE       TRUE
#> 7      1     2    2    TRUE       TRUE
#> 8      1     3    5   FALSE       TRUE
#> 9      2 Total    6   FALSE      FALSE
#> 10     2     1    0    TRUE       TRUE
#> 11     2     2    0    TRUE       TRUE
#> 12     2     3    6   FALSE       TRUE
#> 13     3 Total   24   FALSE      FALSE
#> 14     3     1    7   FALSE       TRUE
#> 15     3     2    8   FALSE       TRUE
#> 16     3     3    9   FALSE      FALSE
GaussSuppressionFromData(z, 1:2, 3, protectZeros = FALSE, secondaryZeros = TRUE, singleton = NULL)
#> GaussSuppression_none: ...............
#>      row   col freq primary suppressed
#> 1  Total Total   37   FALSE      FALSE
#> 2  Total     1    7   FALSE      FALSE
#> 3  Total     2   10   FALSE      FALSE
#> 4  Total     3   20   FALSE      FALSE
#> 5      1 Total    7   FALSE      FALSE
#> 6      1     1    0   FALSE       TRUE
#> 7      1     2    2    TRUE       TRUE
#> 8      1     3    5   FALSE      FALSE
#> 9      2 Total    6   FALSE      FALSE
#> 10     2     1    0   FALSE       TRUE
#> 11     2     2    0   FALSE       TRUE
#> 12     2     3    6   FALSE      FALSE
#> 13     3 Total   24   FALSE      FALSE
#> 14     3     1    7   FALSE      FALSE
#> 15     3     2    8   FALSE      FALSE
#> 16     3     3    9   FALSE      FALSE
GaussSuppressionFromData(z, 1:2, 3, protectZeros = FALSE, secondaryZeros = TRUE)      
#> GaussSuppression_anySumNOTprimary: ...............
#>      row   col freq primary suppressed
#> 1  Total Total   37   FALSE      FALSE
#> 2  Total     1    7   FALSE      FALSE
#> 3  Total     2   10   FALSE      FALSE
#> 4  Total     3   20   FALSE      FALSE
#> 5      1 Total    7   FALSE      FALSE
#> 6      1     1    0   FALSE      FALSE
#> 7      1     2    2    TRUE       TRUE
#> 8      1     3    5   FALSE       TRUE
#> 9      2 Total    6   FALSE      FALSE
#> 10     2     1    0   FALSE      FALSE
#> 11     2     2    0   FALSE       TRUE
#> 12     2     3    6   FALSE       TRUE
#> 13     3 Total   24   FALSE      FALSE
#> 14     3     1    7   FALSE      FALSE
#> 15     3     2    8   FALSE      FALSE
#> 16     3     3    9   FALSE      FALSE
```
