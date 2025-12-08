# Linked tables by full [`GaussSuppressionFromData`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/GaussSuppressionFromData.md) iterations

[`AdditionalSuppression`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/AdditionalSuppression.md)
is called several times as in
[`ChainedSuppression`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/ChainedSuppression.md)

## Usage

``` r
LazyLinkedTables(..., withinArg = NULL, maxIterLinked = 1000)
```

## Arguments

- ...:

  Arguments to `GaussSuppressionFromData` that are kept constant.

- withinArg:

  A list of named lists. Arguments to `GaussSuppressionFromData` that
  are not kept constant.

- maxIterLinked:

  Maximum number of `GaussSuppressionFromData` calls for each table.

## Value

List of data frames

## Details

This function is created as a spin-off from `AdditionalSuppression` and
`ChainedSuppression`. The calculations run `GaussSuppressionFromData`
from the input each time. There is no doubt that this can be done more
efficiently.

A consequence of this lazy implementation is that, in output, `primary`
and `suppressed` are identical.

Note that there is a residual risk when suppression linked tables by
iterations.

## Note

In this function, the parameters `makeForced` and `forceNotPrimary` to
`AdditionalSuppression` are forced to be `FALSE`.

## Examples

``` r
z1 <- SSBtoolsData("z1")
z2 <- SSBtoolsData("z2")
z2b <- z2[3:5]  # As in ChainedSuppression example 
names(z2b)[1] <- "region"

# The two region hierarchies as two linked tables
a <- LazyLinkedTables(z2, freqVar = 5, withinArg = list(
       list(dimVar = c(1, 2, 4)), 
       list(dimVar = c(1, 3, 4))))
#> GaussSuppression_anySum: .........................
#> GaussSuppression_anySum: .................................................
#> GaussSuppression_anySum: ..............................
#> GaussSuppression_anySum: ..........................................
#> GaussSuppression_anySum: .............................

# As 'f' and 'e' in ChainedSuppression example. 
# 'A' 'annet'/'arbeid' suppressed in b[[1]], since suppressed in b[[3]].
b <- LazyLinkedTables(withinArg = list(
       list(data = z1, dimVar = 1:2, freqVar = 3, maxN = 5), 
       list(data = z2b, dimVar = 1:2, freqVar = 3, maxN = 5), 
       list(data = z2, dimVar = 1:4, freqVar = 5, maxN = 1)))
#> GaussSuppression_anySum: ........................................
#> GaussSuppression_anySum: ..............
#> GaussSuppression_anySum: ..........................
#> GaussSuppression_anySum: .................................
#> GaussSuppression_anySum: ...........
#> GaussSuppression_anySum: ....................................
```
