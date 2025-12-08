# Candidates functions

Function for
[`GaussSuppressionFromData`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/GaussSuppressionFromData.md)

## Usage

``` r
CandidatesDefault(freq, x, secondaryZeros = FALSE, weight, ...)

CandidatesNum(
  secondaryZeros = FALSE,
  freq = NULL,
  num,
  weight,
  x,
  candidatesVar = NULL,
  removeCodes = character(0),
  removeCodesForCandidates = TRUE,
  data,
  charVar,
  ...
)
```

## Arguments

- freq:

  Vector of output frequencies

- x:

  The model matrix

- secondaryZeros:

  When `TRUE`, cells with zero frequency or value are prioritized to be
  published so that they are not secondary suppressed. This is achieved
  by this function by having the zero frequency indices first in the
  retuned order.

- weight:

  Vector of output weights

- ...:

  Unused parameters

- num:

  Data frame of output aggregates calculated from `numVar`. When several
  variables, and without specifying `candidatesVar`, only first is used.

- candidatesVar:

  One of the variable names from `numVar` to be used in the
  calculations. Specifying `candidatesVar` helps avoid warnings when
  multiple `numVar` variables are present.

- removeCodes:

  Same parameter as used in suppression rules, e.g.
  [`NContributorsRule`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/NContributorsRule.md).
  It is often assumed that cells where all contributors (`charVar`) are
  present in `removeCodes` should be published. Here, such cells will be
  prioritized to achieve this. Note that this functionality is redundant
  if the same cells are specified by `forced`.

- removeCodesForCandidates:

  `removeCodes` ignored when set to `FALSE`.

- data:

  Input data as a data frame (needed for `removeCodes` calculations)

- charVar:

  Variable(s) with contributor codes (needed for `removeCodes`
  calculations)

## Value

candidates,
[`GaussSuppression`](https://statisticsnorway.github.io/ssb-ssbtools/reference/GaussSuppression.html)
input

## Details

`CandidatesDefault` orders the indices decreasingly according to `freq`
or, when `weight` is non-NULL, `(freq+1)*weight`. Ties are handled by
prioritizing output cells that are calculated from many input cells. In
addition, zeros are handled according to parameter `secondaryZeros`.
When `freq` is negative (special hierarchy), `abs(freq)*weight` is used.

`CandidatesNum` orders the indices decreasingly according to absolute
values of the numeric variable (according to `abs(num[[1]])`). In
practice this is done by running `CandidatesDefault` with manipulated
weights.
