# Function for viewing built-in GaussSuppression specs

Functions to retrieve the built-in specs. These can be retrieved using
either numerical indices or by specifying the spec name, see Details.

## Usage

``` r
PackageSpecs(x = NULL, printTable = FALSE)
```

## Arguments

- x:

  the character name or index of the spec to be returned. If `NULL`
  (default), returns list of all specs

- printTable:

  Logical value (default `FALSE`). If `TRUE`, prints a table description
  of all specs. Primarily used for documentation purposes.

## Value

returns a spec (if `!is.null(x)`), list of all specs (if `is.null(x)`
and `printTable = FALSE`), or markdown table describing all specs (if
`printTable = TRUE`).

## Details

The following table summarizes the built-in specs. Columns represent
different specs, and rows represent the parameter settings.

|                     |                    |                            |                            |                            |
|---------------------|--------------------|----------------------------|----------------------------|----------------------------|
|                     | **smallCountSpec** | **dominanceSpec**          | **fewContributorsSpec**    | **kDisclosureSpec**        |
| **primary**         | PrimaryDefault     | MagnitudeRule              | NContributorsRule          | KDisclosurePrimary         |
| **protectZeros**    | TRUE               | FALSE                      | FALSE                      | FALSE                      |
| **candidates**      | CandidatesDefault  | CandidatesNum              | CandidatesNum              | DirectDisclosureCandidates |
| **singleton**       | SingletonDefault   | SingletonUniqueContributor | SingletonUniqueContributor | SingletonDefault           |
| **extend0**         | TRUE               | FALSE                      | FALSE                      | TRUE                       |
| **preAggregate**    | NA                 | !is.null(charVar)          | !is.null(charVar)          | NA                         |
| **extraAggregate**  | FALSE              | TRUE                       | TRUE                       | FALSE                      |
| **secondaryZeros**  | FALSE              | FALSE                      | FALSE                      | 1                          |
| **domWeightMethod** |                    | "default"                  |                            |                            |
| **singletonMethod** |                    | "numttHTT"                 | "numttHTT"                 | "anySumNOTprimary"         |

## Examples

``` r
PackageSpecs()
#> $smallCountSpec
#> list(primary = PrimaryDefault, protectZeros = TRUE, candidates = CandidatesDefault, 
#>     singleton = SingletonDefault, extend0 = TRUE, preAggregate = NA, 
#>     extraAggregate = FALSE, secondaryZeros = FALSE)
#> 
#> $dominanceSpec
#> list(primary = MagnitudeRule, candidates = CandidatesNum, singleton = SingletonUniqueContributor, 
#>     preAggregate = !is.null(charVar), extraAggregate = TRUE, 
#>     domWeightMethod = "default", singletonMethod = "numttHTT", 
#>     protectZeros = FALSE, extend0 = FALSE, secondaryZeros = FALSE)
#> 
#> $fewContributorsSpec
#> list(primary = NContributorsRule, protectZeros = FALSE, extend0 = FALSE, 
#>     preAggregate = !is.null(charVar), extraAggregate = TRUE, 
#>     candidates = CandidatesNum, singleton = SingletonUniqueContributor, 
#>     singletonMethod = "numttHTT", secondaryZeros = FALSE)
#> 
#> $kDisclosureSpec
#> list(primary = KDisclosurePrimary, protectZeros = FALSE, secondaryZeros = 1, 
#>     candidates = DirectDisclosureCandidates, extend0 = TRUE, 
#>     singletonMethod = "anySumNOTprimary", singleton = SingletonDefault, 
#>     preAggregate = NA, extraAggregate = FALSE)
#> 
PackageSpecs(1)
#> list(primary = PrimaryDefault, protectZeros = TRUE, candidates = CandidatesDefault, 
#>     singleton = SingletonDefault, extend0 = TRUE, preAggregate = NA, 
#>     extraAggregate = FALSE, secondaryZeros = FALSE)
PackageSpecs("smallCountSpec")
#> list(primary = PrimaryDefault, protectZeros = TRUE, candidates = CandidatesDefault, 
#>     singleton = SingletonDefault, extend0 = TRUE, preAggregate = NA, 
#>     extraAggregate = FALSE, secondaryZeros = FALSE)
PackageSpecs(printTable = TRUE)
#> 
#> 
#> |                         |\strong{smallCountSpec} |\strong{dominanceSpec}     |\strong{fewContributorsSpec} |\strong{kDisclosureSpec}   |
#> |:------------------------|:-----------------------|:--------------------------|:----------------------------|:--------------------------|
#> |\strong{primary}         |PrimaryDefault          |MagnitudeRule              |NContributorsRule            |KDisclosurePrimary         |
#> |\strong{protectZeros}    |TRUE                    |FALSE                      |FALSE                        |FALSE                      |
#> |\strong{candidates}      |CandidatesDefault       |CandidatesNum              |CandidatesNum                |DirectDisclosureCandidates |
#> |\strong{singleton}       |SingletonDefault        |SingletonUniqueContributor |SingletonUniqueContributor   |SingletonDefault           |
#> |\strong{extend0}         |TRUE                    |FALSE                      |FALSE                        |TRUE                       |
#> |\strong{preAggregate}    |NA                      |!is.null(charVar)          |!is.null(charVar)            |NA                         |
#> |\strong{extraAggregate}  |FALSE                   |TRUE                       |TRUE                         |FALSE                      |
#> |\strong{secondaryZeros}  |FALSE                   |FALSE                      |FALSE                        |1                          |
#> |\strong{domWeightMethod} |                        |"default"                  |                             |                           |
#> |\strong{singletonMethod} |                        |"numttHTT"                 |"numttHTT"                   |"anySumNOTprimary"         |
```
