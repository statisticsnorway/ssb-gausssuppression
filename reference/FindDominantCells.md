# Method for finding dominant cells according to (possibly multiple) n,k dominance rules.

Supports functionality for grouping contributions according to holding
variables, as well as calculating dominance in surveys with a given
sampling weight. Two methods are implemented, depending on whether the
sampling weights sum to total population. The parameter
`tauArgusDominance` determines this. If `FALSE`, unweighted
contributions are compared to weighted cell values. If `TRUE`, the
method described in in the book "Statistical Disclosure Control"
(Hundepool et al 2012, p. 151) is used.

## Usage

``` r
FindDominantCells(
  x,
  inputnum,
  num,
  n,
  k,
  charVar_groups,
  samplingWeight,
  tauArgusDominance = FALSE,
  returnContrib = FALSE,
  maxContribution = NULL
)
```

## Arguments

- x:

  model matrix describing relationship between input and published cells

- inputnum:

  vector of numeric contributions for each of the input records

- num:

  vector of numeric values for each of the published cells

- n:

  vector of integers describing n parameters in n,k rules. Must be same
  length as `k` parameter.

- k:

  vector of numeric values describing k parameters in n,k rules, where
  percentages are described as numbers less than 100. Must be same
  length as `n` parameter.

- charVar_groups:

  vector describing which input records should be grouped

- samplingWeight:

  vector of sampling weights associated to input records

- tauArgusDominance:

  logical value, default `FALSE`. determines how to handle sampling
  weights in the dominance rule (see details).

- returnContrib:

  logical value, default `FALSE`. If `TRUE` return value is the
  percentage of the first n contributors

- maxContribution:

  Possible precalculated output from `MaxContribution` as input. To
  speed up.

## Value

logical vector describing which publish-cells need to be suppressed.
