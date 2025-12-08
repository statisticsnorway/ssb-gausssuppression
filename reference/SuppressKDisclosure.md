# K-disclosure suppression

A function for suppressing frequency tables using the k-disclosure
method.

## Usage

``` r
SuppressKDisclosure(
  data,
  coalition = 0,
  mc_hierarchies = NULL,
  upper_bound = Inf,
  dimVar = NULL,
  formula = NULL,
  hierarchies = NULL,
  freqVar = NULL,
  ...,
  spec = PackageSpecs("kDisclosureSpec")
)
```

## Arguments

- data:

  a data.frame representing the data set

- coalition:

  numeric vector of length one, representing possible size of an
  attacking coalition. This parameter corresponds to the parameter k in
  the definition of k-disclosure.

- mc_hierarchies:

  a hierarchy representing meaningful combinations to be protected.
  Default value is `NULL`.

- upper_bound:

  numeric value representing minimum count considered safe. Default set
  to `Inf`

- dimVar:

  The main dimensional variables and additional aggregating variables.
  This parameter can be useful when hierarchies and formula are
  unspecified.

- formula:

  A model formula

- hierarchies:

  List of hierarchies, which can be converted by
  [`AutoHierarchies`](https://statisticsnorway.github.io/ssb-ssbtools/reference/AutoHierarchies.html).
  Thus, the variables can also be coded by `"rowFactor"` or `""`, which
  correspond to using the categories in the data.

- freqVar:

  name of the frequency variable in `data`

- ...:

  parameters passed to children functions

- spec:

  `NULL` or a named list of arguments that will act as default values.

## Value

A data.frame containing the publishable data set, with a boolean
variable `$suppressed` representing cell suppressions.

## Author

Daniel P. Lupp

## Examples

``` r
# data
data <- SSBtools::SSBtoolsData("mun_accidents")

# hierarchies as DimLists
mun <- data.frame(levels = c("@", rep("@@", 6)),
codes = c("Total", paste("k", 1:6, sep = "")))
inj <- data.frame(levels = c("@", "@@" ,"@@", "@@", "@@"),
codes = c("Total", "serious", "light", "none", "unknown"))
dimlists <- list(mun = mun, inj = inj)

inj2 <- data.frame(levels = c("@", "@@", "@@@" ,"@@@", "@@", "@@"),
codes = c("Total", "injured", "serious", "light", "none", "unknown"))
inj3 <- data.frame(levels = c("@", "@@", "@@" ,"@@", "@@"),
codes = c( "shadowtotal", "serious", "light", "none", "unknown"))
mc_dimlist <- list(inj = inj2)
mc_nomargs <- list(inj = inj3)

#' # Example with formula, no meaningful combination
out <- SuppressKDisclosure(data, coalition = 1, freqVar = "freq", formula = ~mun*inj)
#> [extend0 24*3->24*3]
#> GaussSuppression_anySumNOTprimary: ...................................

# Example with hierarchy and meaningful combination
out2 <- SuppressKDisclosure(data, coalition = 1, freqVar = "freq",
hierarchies = dimlists, mc_hierarchies = mc_dimlist)
#> [extend0 24*3->24*3]
#> GaussSuppression_anySumNOTprimary: ..............................

#' # Example of table without mariginals, and mc_hierarchies to protect
out3 <- SuppressKDisclosure(data, coalition = 1, freqVar = "freq",
formula = ~mun:inj, mc_hierarchies = mc_nomargs )
#> [extend0 24*3->24*3]
#> GaussSuppression_anySumNOTprimary: .........................
```
