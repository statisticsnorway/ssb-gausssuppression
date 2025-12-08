# Function for calculating intervals for suppressed tables.

This function solves linear programs to determine interval boundaries
for suppressed cells.

## Usage

``` r
ComputeIntervals(
  x,
  z,
  primary,
  suppressed,
  minVal = NULL,
  lpPackage = "lpSolve",
  gaussI = TRUE,
  allInt = FALSE,
  sparseConstraints = TRUE,
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

- cell_grouping:

  Numeric vector indicating group membership. Cells with the same value
  that is not 0 belong to the same group. A value of 0 indicates that
  the cell is not a member of any group. Members of the same group are
  assumed to have the same z-value and this is included as a condition
  when calculating intervals.

## Details

Default in for `bounds` parameter in `Rsymphony_solve_LP` and
`Rglpk_solve_LP`: *The default for each variable is a bound between 0
and `Inf`.* Details in `lpSolve`: *Note that every variable is assumed
to be `>= 0`!*

## Author

Øyvind Langsrud and Daniel Lupp
