
## GaussSuppression	x.x.x
* `SuppressDominantCells()` and the underlying function `MagnitudeRule()` have been improved:
  - `contributorVar` (`charVar`) can now be combined with `sWeightVar`.
  - Improved handling of `protectZeros`. See this parameter's documentation in `?MagnitudeRule`.
  - New parameter `removeCodesFraction` allows adjustment of the effect of the `removeCodes` parameter.
  - New parameter `apply_abs_directly` determines how negative values are treated in the rules:
    - When `apply_abs_directly = FALSE` (default), absolute values are taken after summing 
      contributions, as performed by `max_contribution()` in 
         [the SSBtools package](https://CRAN.R-project.org/package=SSBtools). 
    - When `apply_abs_directly = TRUE`, absolute values are computed directly on the input values, 
      prior to any summation [beyond `preAggregate`]. This corresponds to the old behavior of the function.
  - Enhanced output when `allDominance = TRUE`:
    - Renaming: The variable previously called `primary.2:80` (*(2,80) dominance)*) is now `dominant2`.
    - IDs associated with the largest contributions are now included.
    - The number of contributors is also included.
    - Additional outputs are available. See the documentation of the `allDominance` parameter.
  - The new functionality has been enabled by replacing `MaxContribution()` with the improved 
    `max_contribution()` from 
    [SSBtools](https://CRAN.R-project.org/package=SSBtools).
* Added a check to ensure that at least one of `dimVar`, `hierarchies`, or `formula` is specified.
  - Previously, if all were unspecified, `dimVar` was automatically generated from the remaining columns.
  - While this behavior was correctly implemented, it often stemmed from user input errors and could lead to unexpected behavior or crashes.
  - This change now requires explicit input, making the function more robust and reducing the risk of user errors.
* Improved support for `tibble` and `data.table` input (parameter `data`).
  - Input is now explicitly coerced to a data frame using `as.data.frame()` where necessary to ensure consistent behavior.
  - When `preAggregate` is `TRUE` and `aggregatePackage` is `"data.table"`, the use of `as.data.frame()` is skipped to avoid unnecessary back-and-forth conversion of `data.table` objects, preserving efficiency.
  - Applies to `GaussSuppressionFromData()` and its wrappers.


## GaussSuppression	0.9.0
* The Gaussian elimination secondary suppression algorithm has now been documented in a *"Privacy in Statistical Databases 2024"* paper. 
  - The package description has been updated with this reference [(Langsrud, 2024)](https://doi.org/10.1007/978-3-031-69651-0_6).
* Due to updates in [the SSBtools package](https://CRAN.R-project.org/package=SSBtools) (version 1.5.4), 
  it is now meaningful to include NA’s in the grouping variables. 
  - Note the parameter `NAomit` to `SSBtools::Formula2ModelMatrix()`: 
    * When `TRUE`, NAs in the grouping variables are omitted in output and not included as a separate category.
    * This parameter can be input to `GaussSuppressionFromData()` and its wrappers.
  - `aggregateNA` is new parameter to `GaussSuppressionFromData()`:
    * Whether to include NAs in the grouping variables while preAggregate/extraAggregate.
    * Needs to be `TRUE` (default) to utilize the above `NAomit` parameter.
* Due to updates in [the SSBtools package](https://CRAN.R-project.org/package=SSBtools) (version 1.5.4), 
  where [data.table](https://cran.r-project.org/package=data.table) is now listed under *Suggests*, 
  some functionality can be speeded up. 
  - Set the new parameter `aggregatePackage` to  `"data.table"` to utilize this possibility.
    * `aggregatePackage` is parameter to `GaussSuppressionFromData()` and its wrappers.
    * Also note the related new parameters `aggregateBaseOrder`	and `rowGroupsPackage`. 


## GaussSuppression	0.8.8
* A bug related to the remove0 parameter is now fixed
  - There was a bug related to the `remove0` parameter in `SuppressFewContributors/NContributorsRule` introduced in version 0.8.0. 
    When a single `numVar` was used as input, the `remove0` functionality failed.


## GaussSuppression	0.8.5
* `SuppressDominantCells()` is now considered a common function for both the nk-dominance rule and the p-percent rule.
  - The `pPercent` parameter is now exposed in the `SuppressDominantCells()` documentation.
* The `n` parameter in `SuppressDominantCells()` now defaults to `1:length(k)`.
  - To simplify common use.
* A problem in experimental interval suppression is now fixed. 
  - It was a bug occurring in cases where a response value of zero was secondary suppressed.
* Improvements due to updates in  [the SSBtools package](https://CRAN.R-project.org/package=SSBtools) (version 1.5.2).
  - Fix for a rare problem in `GaussSuppression()`, 
    * Could happen with parallel eliminations combined with integer overflow.
    Then warning message:  *longer object length is not a multiple of shorter object length*
  - Minor change to the singleton method `"anySum"` in `GaussSuppression()` to align with best theory.
    * In practice, this rarely makes a difference.
    * The previous behavior can be ensured by setting `singletonMethod` to either `"anySumOld"` or `"anySumNOTprimaryOld"`.



## GaussSuppression	0.8.3
* Experimental functionality to meet interval width requirements has been incorporated
  - If at least one of the two parameters below is specified, in addition to the
    `lpPackage` parameter, further suppression will be performed to satisfy the interval width requirements.
    * `rangePercent`: Required interval width expressed as a percentage
    * `rangeMin`: Minimum required width of the interval


## GaussSuppression	0.8.0
* Improved singleton methods for magnitude tables
  - A new default setting, `singletonMethod = "numttHTT"`, has been introduced in the 
   wrappers `SuppressDominantCells()` and `SuppressFewContribitors()`. 
   This setting represents the method that offers the highest level of protection. 
   However, it should be noted that with this setting, the computational load of 
   the suppression algorithm may double, which could potentially lead to a doubling of the 
   execution time as well. During these computations, ":::"  will be displayed instead of "....".
    * To prevent this doubling, set `singletonMethod = "numttHtT"`.
    * The behavior of version 0.7.0 can be restored by setting `singletonMethod = "numttH"`.
    * A simpler and faster method is achievable with `singletonMethod = "numttT"`.
    * As in previous versions, singleton handling can be disabled by setting `singletonMethod = "none"`.
  - Additional information can be found by `?SSBtools::NumSingleton`.
  - Some explanation is provided at the bottom of the magnitude vignette.
  - This will later be documented in a more comprehensive manner (paper).
* Improved functionality of the `SuppressDominantCells()` and `SuppressFewContributors()` wrappers.
  -  Improved support for handling multiple numerical variables, introducing new parameters: `dominanceVar` and `candidatesVar`.
  - The `removeCodes` parameter is now also available in the `DominanceRule()` and `SuppressDominantCells()` functions.
  - Support for multiple `contributorVar` (`charVar`) in the  `SuppressFewContributors()` and `NContributorsRule()` functions.
  - Now, `SuppressDominantCells()` includes special functionality to prevent zero cells, which have been suppressed, 
    from being revealable in cases where negative values cannot occur. See the parameter `singletonZeros`.
  - The update described below enables the specification of the `pPercent` parameter directly through `SuppressDominantCells()`.
* The p% rule for magnitude tables has been implemented through the introduction of a 
  new primary suppression function, `PPercentRule()`.
  - Technically, `PPercentRule()` and `DominanceRule()` now serve as wrappers for the newly introduced. 
    general function `MagnitudeRule()`.
* `AdditionalSuppression()` generalized to take a wrappers as input.
* New special functions for the avoidance of suppression
  - `PrimaryRemoveWg()`, `CandidatesNumWg()` and `ForcedWg()`
* Experimental functionality for interval calculations has been included
  - Intervals can now be calculated using the new function `ComputeIntervals()`.
  - When the `lpPackage` parameter is specified in `GaussSuppressionFromData()` or in any of its wrappers, 
    intervals for primary suppressed cells will be computed and included in the output.
    

## GaussSuppression	0.7.0
* More vignettes are included.
* Better singleton handling for magnitude tables when using `SuppressDominantCells()` and `SuppressFewContributors()`. 
  - Due to improvements in the SSBtools package (version 1.4.6).
  - Due to new default `extraAggregate = TRUE` in the specs, `dominanceSpec` and `fewContributorsSpec`.
* More default values are explicitly included in the in specs so that they are easier seen.
  - See `PackageSpecs()`.
  

## GaussSuppression	0.6.0
* A vignette entitled _"Defining Tables for GaussSuppression"_ is now included.
* Now, easy-to-use wrapper functions are included.
  - `SuppressSmallCounts()`, `SuppressDominantCells()`, and `SuppressFewContributors()`, 
    along with `SuppressKDisclosure()` (which was available in the previous version).
* Built-in specs that contribute to a simpler interface have been adopted.
  - See `PackageSpecs()`.
* Sampling weights are now possible in the dominance rule.
  - See `DominanceRule()`.
* More advanced singleton handling that makes use of new functionality in the SSBtools package (version 1.4.4).
  - See `SSBtools::GaussSuppression()`.
  - See `SingletonUniqueContributor()`.
* Now `forced` and `usafe` are possible output columns.
  - Where unsafe means unsafe primary suppressions due to forced cells.
    That, is the unsafe primarily suppressed values can be re-calculated from the 
    values of the cells forced to be not suppressed.
  - See parameters `forcedInOutput` and `unsafeInOutput` to `GaussSuppressionFromData()`.   

## GaussSuppression	0.5.0
* Now the original variable names, as specified by `freqVar` and `weightVar`, are kept in the output. 
  - In previous versions these names were standardized to `"freq"` and `"weight"`.
  - Code relying on previous behavior with other `freqVar`/`weightVar` than `"freq"`/`"weight"` needs to be updated.
  - `"freq"` is still default when data is aggregated from microdata without `freqVar` specified (see new parameter `freqVarNew`). 
* Adaption needed after Matrix ver. 1.4-2 (not a user-visible change)

## GaussSuppression	0.4.0

* Last version before any news
