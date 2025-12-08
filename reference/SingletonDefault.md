# Default singleton function

Function for
[`GaussSuppressionFromData`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/GaussSuppressionFromData.md)

## Usage

``` r
SingletonDefault(data, freqVar, protectZeros, secondaryZeros, ...)
```

## Arguments

- data:

  Input data, possibly pre-aggregated within `GaussSuppressionFromData`

- freqVar:

  A single variable holding counts (input to `GaussSuppressionFromData`)

- protectZeros:

  Suppression parameter (see `GaussSuppressionFromData`)

- secondaryZeros:

  Suppression parameter (see `GaussSuppressionFromData`)

- ...:

  Unused parameters

## Value

singleton,
[`GaussSuppression`](https://statisticsnorway.github.io/ssb-ssbtools/reference/GaussSuppression.html)
input

## Details

This function marks input cells as singletons according to the input
frequencies (`freqVar`). Zero frequencies are set to singletons when
`protectZeros` or `secondaryZeros` is `TRUE`. Otherwise, ones are set to
singletons. Empty `freqVar` is treated as all frequencies being ones.
