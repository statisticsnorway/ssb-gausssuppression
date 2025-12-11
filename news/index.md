# Changelog

## GaussSuppression 1.2.2

- Added four new parameters (`min_n_contr`, `min_n_non0_contr`,
  `min_n_contr_all`, `min_n_non0_contr_all`) to
  [`MagnitudeRule()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/MagnitudeRule.md),
  the primary-suppression function used by
  [`SuppressDominantCells()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/SuppressDominantCells.md).
  - These parameters can now be used directly when calling
    [`SuppressDominantCells()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/SuppressDominantCells.md).
  - It is now also possible to call
    [`SuppressDominantCells()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/SuppressDominantCells.md)
    without specifying either `pPercent` or `k`.
  - Together, these changes make it possible to replace typical uses of
    [`SuppressFewContributors()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/SuppressFewContributors.md)
    with corresponding calls to
    [`SuppressDominantCells()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/SuppressDominantCells.md)
    using the new parameters.
  - The new parameters are not yet mentioned in the documentation or
    examples for
    [`SuppressDominantCells()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/SuppressDominantCells.md).
- Added three new parameters (`da_vars`, `da_fun`, and `da_args`) to
  enable the use of
  [`SSBtools::dummy_aggregate()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/dummy_aggregate.html)
  within
  [`GaussSuppressionFromData()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/GaussSuppressionFromData.md)
  and its wrappers.
  - This makes it possible to compute derived variables via
    [`SSBtools::aggregate_multiple_fun()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/aggregate_multiple_fun.html)
    in the same way as when using
    [`SSBtools::model_aggregate()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/model_aggregate.html).
  - The intermediate result is stored in an internal data frame
    (`da_out`), which is available to user-supplied functions and whose
    columns are added to the final output.
- Improved handling of unused dots
  - Default for `action_unused_dots` changed from `"inform"` to `"warn"`
    to make messages easier for users to recognise and interpret as
    non-critical warnings.
  - Internal list of allowed unused dots expanded to avoid unnecessary
    warnings in additional special cases.
- Multiple total codes can now be specified due to updates in the [the
  SSBtools package](https://CRAN.R-project.org/package=SSBtools)
  (version 1.8.6).
  - The `total` parameter can be specified as a named vector or named
    list.
  - The reexported function
    [`tables_by_formulas()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/tables_by_formulas.html)
    has been updated so that named total vectors are handled flexibly
    with both `substitute_vars` and `collapse_vars`.
  - See
    [issue](https://github.com/statisticsnorway/ssb-gausssuppression/issues/118)
    [\#118](https://github.com/statisticsnorway/ssb-gausssuppression/issues/118)
    for an example, which also inspired this enhancement. There was no
    actual bug: previous behavior was only a consequence of how R
    coerces a named list to a character vector when used with
    [`names()`](https://rdrr.io/r/base/names.html), and the list names
    were not used.
- Fixed bug in how `removeCodes` without `contributorVar` (rare case)
  affects singleton
  - Affected both
    [`SuppressDominantCells()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/SuppressDominantCells.md)
    and
    [`SuppressFewContributors()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/SuppressFewContributors.md).
  - The bug most likely caused errors due to indexes being outside the
    allowed range.
  - The solution uses a new variable, `origIdxVar`, which is also
    introduced as a new
    [`GaussSuppressionFromData()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/GaussSuppressionFromData.md)
    parameter and generated in the `extraAggregate` step.

## GaussSuppression 1.2.0

CRAN release: 2025-09-25

- New checks for unused arguments in `...` using
  [ellipsis::check_dots_used()](https://ellipsis.r-lib.org/reference/check_dots_used.html),
  so that misspelled or irrelevant arguments are not silently ignored.
  - Introduces new arguments in
    [`GaussSuppressionFromData()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/GaussSuppressionFromData.md),
    available through all wrappers:
    - `action_unused_dots` controls how unused arguments are handled.
    - `allowed_unused_dots` specifies argument names to ignore in the
      unused-argument check.
  - Users can configure global defaults via options
    `GaussSuppression.action_unused_dots` and
    `GaussSuppression.allowed_unused_dots`.
  - Note: The default for `action_unused_dots` is `"inform"` as a
    cautious starting point. This may change to `"warn"` in a future
    release.
  - Thanks to Jonas Lindblad for
    [issue](https://github.com/statisticsnorway/ssb-gausssuppression/issues/124)
    [\#124](https://github.com/statisticsnorway/ssb-gausssuppression/issues/124).
- New duplicate checking method to decide `preAggregate` in
  [`SuppressSmallCounts()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/SuppressSmallCounts.md).
  - Same method added to the other frequency table wrapper,
    [`SuppressKDisclosure()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/SuppressKDisclosure.md).  
  - Implemented by adding support for `NA` in
    [`GaussSuppressionFromData()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/GaussSuppressionFromData.md)
    and by updating the specs to use `NA` as default.
    - When `preAggregate = NA`, the function now decides automatically:
      aggregation is applied unless `freqVar` is present and the data
      contain no duplicated rows with respect to the relevant
      variables.  
    - Previously, duplicate rows were not checked.
- Extended experimental support for interval protection with additional
  parameters.
  - In particular, by specifying `lpPackage` together with
    `protectionIntervals = TRUE` in
    [`SuppressDominantCells()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/SuppressDominantCells.md),
    tables can now be protected according to the protection levels
    described in *Handbook on Statistical Disclosure Control* (2nd ed.,
    Ch. 4.2.2, 2025).
  - A similar experimental extension is available in
    [`SuppressSmallCounts()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/SuppressSmallCounts.md),
    where the `protectionIntervals` parameter can be used to include
    some interval requirements.
  - See
    [`?IntervalLimits`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/IntervalLimits.md)
    and the parameter description of `lpPackage` in
    [`?GaussSuppressionFromData`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/GaussSuppressionFromData.md)
    for details.
  - Addresses
    [issue](https://github.com/statisticsnorway/ssb-gausssuppression/issues/131)
    [\#131](https://github.com/statisticsnorway/ssb-gausssuppression/issues/131).
- New parameter `intervalSuppression` to
  [`GaussSuppressionFromData()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/GaussSuppressionFromData.md)
  and its wrappers.
  - Controls whether interval requirements are only calculated and
    reported, or also used for suppression (if `lpPackage` is
    specified).
- Bug fix implemented for interval calculations with lpSolve so that
  constraint matrices with empty rows do not produce an error.
  - This fix also prepares for possible future lp-solver packages that
    may require non-empty rows.
  - Thanks to Jonas Lindblad for
    [issue](https://github.com/statisticsnorway/ssb-gausssuppression/issues/130)
    [\#130](https://github.com/statisticsnorway/ssb-gausssuppression/issues/130).

## GaussSuppression 1.1.5

CRAN release: 2025-08-25

- Major update to the functionality via
  **[`SuppressLinkedTables()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/SuppressLinkedTables.md)**
  and **`linkedGauss`**:
  - New method `"super-consistent"` is now the `linkedGauss` default in
    [`SuppressLinkedTables()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/SuppressLinkedTables.md),
    providing stronger coordination across tables than `"consistent"`.  
  - Added parameter `collapseAware` for stronger coordination across
    tables than `recordAware`.  
  - Intervals are now supported, with the option to apply further
    suppression to meet interval width requirements.
    - Existing parameters `lpPackage`, `rangePercent`, and `rangeMin`
      can now be used with this functionality.  
    - Added parameter `linkedIntervals` to determine how interval
      calculations are performed.  
- A vignette, “Linked table suppression”, is now included.  
- Improved documentation for the `aggregateNA` parameter in
  [`GaussSuppressionFromData()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/GaussSuppressionFromData.md):
  - Description about NA handling.  
  - With contribution from Jonas Lindblad, in line with
    [issue](https://github.com/statisticsnorway/ssb-gausssuppression/issues/125)
    [\#125](https://github.com/statisticsnorway/ssb-gausssuppression/issues/125).

## GaussSuppression 1.1.0

CRAN release: 2025-06-26

- Significant update: new functionality via
  **[`SuppressLinkedTables()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/SuppressLinkedTables.md)**
  and **`linkedGauss`** for consistent suppression of linked tables
  - Introduces alternatives to global protection, potentially reducing
    computational burden.  
  - Available via the new function
    [`SuppressLinkedTables()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/SuppressLinkedTables.md),
    or alternatively by using the new parameter `linkedGauss` in
    [`GaussSuppressionFromData()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/GaussSuppressionFromData.md)
    and its wrappers.  
  - Also note the related new parameters to
    [`GaussSuppressionFromData()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/GaussSuppressionFromData.md):
    `recordAware` and `linkedTables`.
- Improved support for named `maxN` with variable-specific values.
  - Enables setups with multiple primary functions using different
    `maxN` values.  
  - See the documentation for
    [`GaussSuppressionFromData()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/GaussSuppressionFromData.md),
    [`PrimaryDefault()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/PrimaryDefault.md),
    and
    [`NContributorsRule()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/NContributorsRule.md).  
  - Thanks to Jonas Lindblad for
    [issue](https://github.com/statisticsnorway/ssb-gausssuppression/issues/120)
    [\#120](https://github.com/statisticsnorway/ssb-gausssuppression/issues/120).

## GaussSuppression 1.0.0

CRAN release: 2025-04-24

- The package has been stable and in practical use for a long time. The
  release of version 1.0.0 is therefore appropriate.
- Interval examples are now included in the vignettes.
- Progress information during extra suppression to meet interval width
  requirements now counts down instead of up, making it easier to
  interpret.
- Updated to support the latest
  [highs](https://CRAN.R-project.org/package=highs) version (1.9.0-1)
  for interval computation.
- The warning about interval calculations being experimental has been
  removed.

## GaussSuppression 0.9.6

CRAN release: 2025-03-12

- Misleading warnings in
  [`SuppressionFromDecimals()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/SuppressionFromDecimals.md)
  are now prevented.
  - The update to version 0.9.5 could cause the warning  
    *“Mismatch between aggregated frequencies and decimals aggregated to
    whole numbers”*  
    to appear incorrectly with small datasets. This has now been fixed.
- Preparation for a slight improvement to a singleton method in the next
  version of SSBtools (1.7.5).
  - The method described as `elimination` (4th character) in
    [`?SSBtools::NumSingleton`](https://statisticsnorway.github.io/ssb-ssbtools/reference/NumSingleton.html)
    will be slightly improved.  
  - Tests in the GaussSuppression package have been updated to
    accommodate this change.
- A hexagon sticker logo is now visible on the [pkgdown
  website](https://statisticsnorway.github.io/ssb-gausssuppression/) and
  the [GitHub
  repository](https://github.com/statisticsnorway/ssb-gausssuppression).

## GaussSuppression 0.9.5

CRAN release: 2025-02-07

- New pkgdown website for the package
  - This package now has a documentation site at
    <https://statisticsnorway.github.io/ssb-gausssuppression/>.
- Matrix are moved from Depends to Imports
  - To follow best practices for R packages
- Utilizing updates in [the SSBtools
  package](https://CRAN.R-project.org/package=SSBtools) (version 1.7.0).
  - The new function
    [`tables_by_formulas()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/tables_by_formulas.html),
    which is now reexported, is demonstrated in a
    [`SuppressSmallCounts()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/SuppressSmallCounts.md)
    example and a
    [`SuppressDominantCells()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/SuppressDominantCells.md)
    example.  
  - The new function
    [`Extend0fromModelMatrixInput()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/Extend0fromModelMatrixInput.html)
    is now used in data pre-processing. As a result,
    `hierarchical_extend0` is now a possible parameter, as illustrated
    in a
    [`SuppressSmallCounts()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/SuppressSmallCounts.md)
    example.
- Improvements to GaussSuppressDec() and SuppressionFromDecimals()
  - GaussSuppressDec() now accepts wrappers as input, such as
    SuppressSmallCounts() and SuppressDominantCells().
  - Added new parameter `use_freqVar` to allow decimal numbers to be
    generated solely from 0s, improving stability.
  - All output variables from the underlying function are now included
    in the default output of GaussSuppressDec().
  - FormulaSelection() now works correctly with output from
    GaussSuppressDec().
  - SuppressionFromDecimals() has been updated accordingly. In
    particular, automation is now implemented to detect whether decimal
    numbers are generated from 0s or from frequencies.

## GaussSuppression 0.9.2

CRAN release: 2024-12-09

- Added a check to ensure that at least one of `dimVar`, `hierarchies`,
  or `formula` is specified.
  - This is a breaking change that may affect previous code.
  - Previously, if all were unspecified, `dimVar` was automatically
    generated from the remaining columns.
  - While this behavior was correctly implemented, it often stemmed from
    user input errors and could lead to unexpected behavior or crashes.
  - This change now requires explicit input, making the function more
    robust and reducing the risk of user errors.
- [`SuppressDominantCells()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/SuppressDominantCells.md)
  and the underlying function
  [`MagnitudeRule()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/MagnitudeRule.md)
  have been improved:
  - `contributorVar` (`charVar`) can now be combined with `sWeightVar`.
  - Improved handling of `protectZeros`. See this parameter’s
    documentation in
    [`?MagnitudeRule`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/MagnitudeRule.md).
  - New parameter `removeCodesFraction` allows adjustment of the effect
    of the `removeCodes` parameter.
  - New parameter `apply_abs_directly` determines how negative values
    are treated in the rules:
    - When `apply_abs_directly = FALSE` (default), absolute values are
      taken after summing contributions, as performed by
      [`max_contribution()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/max_contribution.html)
      in [the SSBtools
      package](https://CRAN.R-project.org/package=SSBtools).
    - When `apply_abs_directly = TRUE`, absolute values are computed
      directly on the input values, prior to any summation \[beyond
      `preAggregate`\]. This corresponds to the old behavior of the
      function.
  - Enhanced output when `allDominance = TRUE`:
    - Renaming: The variable previously called `primary.2:80` (*(2,80)
      dominance)*) is now `dominant2`.
    - IDs associated with the largest contributions are now included.
    - The number of contributors is also included.
    - Additional outputs are available. See the documentation of the
      `allDominance` parameter.
  - The new functionality has been enabled by replacing
    [`MaxContribution()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/MaxContribution.md)
    with the improved
    [`max_contribution()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/max_contribution.html)
    from [SSBtools](https://CRAN.R-project.org/package=SSBtools).
- Improved support for `tibble` and `data.table` input (parameter
  `data`).
  - Input is now explicitly coerced to a data frame using
    [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) where
    necessary to ensure consistent behavior.
  - When `preAggregate` is `TRUE` and `aggregatePackage` is
    `"data.table"`, the use of
    [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) is
    skipped to avoid unnecessary back-and-forth conversion of
    `data.table` objects, preserving efficiency.
  - Applies to
    [`GaussSuppressionFromData()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/GaussSuppressionFromData.md)
    and its wrappers.
- The `SSBtools` functions
  [`FormulaSelection()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/FormulaSelection.html)
  and its identical wrapper
  [`formula_selection()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/FormulaSelection.html)
  are now re-exported.
  - These functions are useful for extracting smaller datasets from the
    output.  
  - With this change, using
    [`library(SSBtools)`](https://github.com/statisticsnorway/ssb-ssbtools)
    is no longer necessary to access them.  
- Note new hierarchy possibilities due to the new version of [the
  SSBtools package](https://CRAN.R-project.org/package=SSBtools)
  (version 1.6.0).
  - Output from functions like `get_klass()` in the [klassR
    package](https://cran.r-project.org/package=klassR) or
    `hier_create()` in the [sdcHierarchies
    package](https://cran.r-project.org/package=sdcHierarchies) can now
    be used directly as input. Example of usage:

    ``` r
     a <- get_klass(classification = "24")
     b <- hier_create(root = "Total", nodes = LETTERS[1:5])
     mydata <- data.frame(tree = sample(a$code[nchar(a$code) > 1], 200, replace = TRUE), 
                          letter = LETTERS[1:5])
     SuppressSmallCounts(mydata, maxN = 3, hierarchies = list(tree = a, letter = b)) 
    ```

  - New possibilities for working with both formulas and hierarchies are
    now available through the
    [`map_hierarchies_to_data()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/map_hierarchies_to_data.html)
    function.

  - Improved functionality for combining formulas with the
    [`Formula2ModelMatrix()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/FormulaSums.html)
    parameter `avoidHierarchical = TRUE`, thanks to the new
    [`total_collapse()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/total_collapse.html)
    function which can be applied to output.

## GaussSuppression 0.9.0

CRAN release: 2024-09-24

- The Gaussian elimination secondary suppression algorithm has now been
  documented in a *“Privacy in Statistical Databases 2024”* paper.
  - The package description has been updated with this reference
    [(Langsrud, 2024)](https://doi.org/10.1007/978-3-031-69651-0_6).
- Due to updates in [the SSBtools
  package](https://CRAN.R-project.org/package=SSBtools) (version 1.5.4),
  it is now meaningful to include NA’s in the grouping variables.
  - Note the parameter `NAomit` to
    [`SSBtools::Formula2ModelMatrix()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/FormulaSums.html):
    - When `TRUE`, NAs in the grouping variables are omitted in output
      and not included as a separate category.
    - This parameter can be input to
      [`GaussSuppressionFromData()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/GaussSuppressionFromData.md)
      and its wrappers.
  - `aggregateNA` is new parameter to
    [`GaussSuppressionFromData()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/GaussSuppressionFromData.md):
    - Whether to include NAs in the grouping variables while
      preAggregate/extraAggregate.
    - Needs to be `TRUE` (default) to utilize the above `NAomit`
      parameter.
- Due to updates in [the SSBtools
  package](https://CRAN.R-project.org/package=SSBtools) (version 1.5.4),
  where [data.table](https://cran.r-project.org/package=data.table) is
  now listed under *Suggests*, some functionality can be speeded up.
  - Set the new parameter `aggregatePackage` to `"data.table"` to
    utilize this possibility.
    - `aggregatePackage` is parameter to
      [`GaussSuppressionFromData()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/GaussSuppressionFromData.md)
      and its wrappers.
    - Also note the related new parameters `aggregateBaseOrder` and
      `rowGroupsPackage`.

## GaussSuppression 0.8.8

CRAN release: 2024-06-28

- A bug related to the remove0 parameter is now fixed
  - There was a bug related to the `remove0` parameter in
    `SuppressFewContributors/NContributorsRule` introduced in version
    0.8.0. When a single `numVar` was used as input, the `remove0`
    functionality failed.

## GaussSuppression 0.8.5

CRAN release: 2024-05-22

- [`SuppressDominantCells()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/SuppressDominantCells.md)
  is now considered a common function for both the nk-dominance rule and
  the p-percent rule.
  - The `pPercent` parameter is now exposed in the
    [`SuppressDominantCells()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/SuppressDominantCells.md)
    documentation.
- The `n` parameter in
  [`SuppressDominantCells()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/SuppressDominantCells.md)
  now defaults to `1:length(k)`.
  - To simplify common use.
- A problem in experimental interval suppression is now fixed.
  - It was a bug occurring in cases where a response value of zero was
    secondary suppressed.
- Improvements due to updates in [the SSBtools
  package](https://CRAN.R-project.org/package=SSBtools) (version 1.5.2).
  - Fix for a rare problem in
    [`GaussSuppression()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/GaussSuppression.html),
    - Could happen with parallel eliminations combined with integer
      overflow. Then warning message: *longer object length is not a
      multiple of shorter object length*
  - Minor change to the singleton method `"anySum"` in
    [`GaussSuppression()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/GaussSuppression.html)
    to align with best theory.
    - In practice, this rarely makes a difference.
    - The previous behavior can be ensured by setting `singletonMethod`
      to either `"anySumOld"` or `"anySumNOTprimaryOld"`.

## GaussSuppression 0.8.3

CRAN release: 2024-03-22

- Experimental functionality to meet interval width requirements has
  been incorporated
  - If at least one of the two parameters below is specified, in
    addition to the `lpPackage` parameter, further suppression will be
    performed to satisfy the interval width requirements.
    - `rangePercent`: Required interval width expressed as a percentage
    - `rangeMin`: Minimum required width of the interval

## GaussSuppression 0.8.0

CRAN release: 2024-02-02

- Improved singleton methods for magnitude tables
  - A new default setting, `singletonMethod = "numttHTT"`, has been
    introduced in the wrappers
    [`SuppressDominantCells()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/SuppressDominantCells.md)
    and `SuppressFewContribitors()`. This setting represents the method
    that offers the highest level of protection. However, it should be
    noted that with this setting, the computational load of the
    suppression algorithm may double, which could potentially lead to a
    doubling of the execution time as well. During these computations,
    “:::” will be displayed instead of “….”.
    - To prevent this doubling, set `singletonMethod = "numttHtT"`.
    - The behavior of version 0.7.0 can be restored by setting
      `singletonMethod = "numttH"`.
    - A simpler and faster method is achievable with
      `singletonMethod = "numttT"`.
    - As in previous versions, singleton handling can be disabled by
      setting `singletonMethod = "none"`.
  - Additional information can be found by
    [`?SSBtools::NumSingleton`](https://statisticsnorway.github.io/ssb-ssbtools/reference/NumSingleton.html).
  - Some explanation is provided at the bottom of the magnitude
    vignette.
  - This will later be documented in a more comprehensive manner
    (paper).
- Improved functionality of the
  [`SuppressDominantCells()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/SuppressDominantCells.md)
  and
  [`SuppressFewContributors()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/SuppressFewContributors.md)
  wrappers.
  - Improved support for handling multiple numerical variables,
    introducing new parameters: `dominanceVar` and `candidatesVar`.
  - The `removeCodes` parameter is now also available in the
    [`DominanceRule()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/MagnitudeRule.md)
    and
    [`SuppressDominantCells()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/SuppressDominantCells.md)
    functions.
  - Support for multiple `contributorVar` (`charVar`) in the
    [`SuppressFewContributors()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/SuppressFewContributors.md)
    and
    [`NContributorsRule()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/NContributorsRule.md)
    functions.
  - Now,
    [`SuppressDominantCells()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/SuppressDominantCells.md)
    includes special functionality to prevent zero cells, which have
    been suppressed, from being revealable in cases where negative
    values cannot occur. See the parameter `singletonZeros`.
  - The update described below enables the specification of the
    `pPercent` parameter directly through
    [`SuppressDominantCells()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/SuppressDominantCells.md).
- The p% rule for magnitude tables has been implemented through the
  introduction of a new primary suppression function,
  [`PPercentRule()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/MagnitudeRule.md).
  - Technically,
    [`PPercentRule()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/MagnitudeRule.md)
    and
    [`DominanceRule()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/MagnitudeRule.md)
    now serve as wrappers for the newly introduced. general function
    [`MagnitudeRule()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/MagnitudeRule.md).
- [`AdditionalSuppression()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/AdditionalSuppression.md)
  generalized to take a wrappers as input.
- New special functions for the avoidance of suppression
  - [`PrimaryRemoveWg()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/PrimaryRemoveWg.md),
    [`CandidatesNumWg()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/PrimaryRemoveWg.md)
    and
    [`ForcedWg()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/PrimaryRemoveWg.md)
- Experimental functionality for interval calculations has been included
  - Intervals can now be calculated using the new function
    [`ComputeIntervals()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/ComputeIntervals.md).
  - When the `lpPackage` parameter is specified in
    [`GaussSuppressionFromData()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/GaussSuppressionFromData.md)
    or in any of its wrappers, intervals for primary suppressed cells
    will be computed and included in the output.

## GaussSuppression 0.7.0

CRAN release: 2023-06-07

- More vignettes are included.
- Better singleton handling for magnitude tables when using
  [`SuppressDominantCells()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/SuppressDominantCells.md)
  and
  [`SuppressFewContributors()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/SuppressFewContributors.md).
  - Due to improvements in the SSBtools package (version 1.4.6).
  - Due to new default `extraAggregate = TRUE` in the specs,
    `dominanceSpec` and `fewContributorsSpec`.
- More default values are explicitly included in the in specs so that
  they are easier seen.
  - See
    [`PackageSpecs()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/PackageSpecs.md).

## GaussSuppression 0.6.0

CRAN release: 2023-03-31

- A vignette entitled *“Defining Tables for GaussSuppression”* is now
  included.
- Now, easy-to-use wrapper functions are included.
  - [`SuppressSmallCounts()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/SuppressSmallCounts.md),
    [`SuppressDominantCells()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/SuppressDominantCells.md),
    and
    [`SuppressFewContributors()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/SuppressFewContributors.md),
    along with
    [`SuppressKDisclosure()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/SuppressKDisclosure.md)
    (which was available in the previous version).
- Built-in specs that contribute to a simpler interface have been
  adopted.
  - See
    [`PackageSpecs()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/PackageSpecs.md).
- Sampling weights are now possible in the dominance rule.
  - See
    [`DominanceRule()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/MagnitudeRule.md).
- More advanced singleton handling that makes use of new functionality
  in the SSBtools package (version 1.4.4).
  - See
    [`SSBtools::GaussSuppression()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/GaussSuppression.html).
  - See
    [`SingletonUniqueContributor()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/SingletonUniqueContributor.md).
- Now `forced` and `usafe` are possible output columns.
  - Where unsafe means unsafe primary suppressions due to forced cells.
    That, is the unsafe primarily suppressed values can be re-calculated
    from the values of the cells forced to be not suppressed.
  - See parameters `forcedInOutput` and `unsafeInOutput` to
    [`GaussSuppressionFromData()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/GaussSuppressionFromData.md).

## GaussSuppression 0.5.0

CRAN release: 2022-08-30

- Now the original variable names, as specified by `freqVar` and
  `weightVar`, are kept in the output.
  - In previous versions these names were standardized to `"freq"` and
    `"weight"`.
  - Code relying on previous behavior with other `freqVar`/`weightVar`
    than `"freq"`/`"weight"` needs to be updated.
  - `"freq"` is still default when data is aggregated from microdata
    without `freqVar` specified (see new parameter `freqVarNew`).
- Adaption needed after Matrix ver. 1.4-2 (not a user-visible change)

## GaussSuppression 0.4.0

CRAN release: 2022-06-28

- Last version before any news
