# [`Ncontributors`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/Ncontributors.md) with holding-indicator

The aggregates (columns of `x`) are grouped by a holding indicator.
Within each holding group, the number of unique groups (output) is set
to be equal.

## Usage

``` r
NcontributorsHolding(x, groups, holdingInd = NULL)
```

## Arguments

- x:

  A (sparse) dummy matrix

- groups:

  Vector of group categories

- holdingInd:

  Vector of holding group categories

## Value

Vector of numbers of unique groups

## Details

A representative within the holding group is used to calculate output by
[`Ncontributors`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/Ncontributors.md).
The one with maximal column sum of `x` is chosen as the representative.
Normally this will be an aggregate representing the holding group total.
When holdingInd is NULL (default), the function is equivalent to
[`Ncontributors`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/Ncontributors.md).

## Author

Øyvind Langsrud
