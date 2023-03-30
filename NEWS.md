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

## GaussSuppression	0.5.0
* Now the original variable names, as specified by `freqVar` and `weightVar`, are kept in the output. 
  - In previous versions these names were standardized to `"freq"` and `"weight"`.
  - Code relying on previous behavior with other `freqVar`/`weightVar` than `"freq"`/`"weight"` needs to be updated.
  - `"freq"` is still default when data is aggregated from microdata without `freqVar` specified (see new parameter `freqVarNew`). 
* Adaption needed after Matrix ver. 1.4-2 (not a user-visible change)

## GaussSuppression	0.4.0

* Last version before any news
