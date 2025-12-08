# New primary cells to fix risky intervals

Indices to new primary cells are returned

## Usage

``` r
FixRiskyIntervals(
  x,
  z,
  primary,
  suppressed,
  candidates = NULL,
  minVal = NULL,
  lpPackage = "lpSolve",
  gaussI = FALSE,
  allInt = FALSE,
  sparseConstraints = TRUE,
  intervalLimits,
  cell_grouping = rep(0, length(z))
)
```

## Arguments

- x:

  ModelMatrix, as output from SSBtools::ModelMatrix

- z:

  numerical vector with length ncol(x). Corresponds to table cell values

- primary:

  Vector indicating primary suppressed cells. Can be logical or integer.
  If integer vector, indicates the columns of x which are considered
  primary suppressed.

- suppressed:

  Vector indicating all suppressed cells. Can be logical or integer. If
  integer vector, indicates the columns of x which are considered
  suppressed.

- candidates:

  `candidates` as indices

- minVal:

  a known minimum value for table cells. Default NULL. Note that
  'minVal' is interpreted as the limiting value for all suppressed
  cells. Specifying 'minVal=0' would be redundant, as a minimum value of
  0 is anyway assumed for inner cells (see details).

- lpPackage:

  The name of the package used to solve linear programs. Currently,
  'lpSolve' (default), 'Rsymphony', 'Rglpk' and 'highs' are supported.

- gaussI:

  Boolean vector. If TRUE (default), GaussIndependent is used to reduce
  size of linear program.

- allInt:

  Integer variables when TRUE. See `all.int` parameter in `lpSolve` and
  `types` parameter in `Rsymphony` and `Rglpk`.

- sparseConstraints:

  When TRUE, a sparse constraint matrix will be input to the solver. In
  the case of `lpSolve`, the sparse matrix is represented in triplet
  form as a dense matrix with three columns, and the `dense.const`
  parameter is utilized.

- intervalLimits:

  As computed by
  [`IntervalLimits`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/IntervalLimits.md)

- cell_grouping:

  Numeric vector indicating group membership. Cells with the same value
  that is not 0 belong to the same group. A value of 0 indicates that
  the cell is not a member of any group. Members of the same group are
  assumed to have the same z-value and this is included as a condition
  when calculating intervals.

## Details

Code in this function started from a copy of
[`ComputeIntervals`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/ComputeIntervals.md)
