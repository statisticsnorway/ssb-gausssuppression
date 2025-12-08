# Default primary function

Function for
[`GaussSuppressionFromData`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/GaussSuppressionFromData.md)

## Usage

``` r
PrimaryDefault(
  freq,
  maxN = 3,
  protectZeros = TRUE,
  protectionIntervals = FALSE,
  freqVar,
  ...
)
```

## Arguments

- freq:

  Vector of output frequencies

- maxN:

  Cells with frequency `<= maxN` are set as primary suppressed. Can also
  be a named list or vector, where the value corresponding to `freqVar`
  will be used if available. If not found, the name `"freq"` is tried as
  an alternative.

- protectZeros:

  When `TRUE`, cells with zero frequency are set as primary suppressed.

- protectionIntervals:

  Logical. When `TRUE`, some interval requirements are included in the
  output as a safeguard against obvious weaknesses with respect to
  attribute disclosure. The rule is that the upper bound must be at
  least 1 above the observed frequency, and the total interval width
  must be at least 2. The corresponding variables are added with names
  starting with `upmin_` and `rlim_`. See
  [`IntervalLimits()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/IntervalLimits.md)
  for setting interval limits in general.

- freqVar:

  Character string used to select the appropriate value from `maxN` if
  it is a named object. see `maxN` above.

- ...:

  Unused parameters

## Value

primary,
[`GaussSuppression`](https://statisticsnorway.github.io/ssb-ssbtools/reference/GaussSuppression.html)
input
