# Additional primary cells based on risky primary cells

The algorithm uses parent-child relationships found from the model
matrix (`x`)

## Usage

``` r
PrimaryFromRiskyDefault(x, y, risky, candidates, allDims = FALSE)
```

## Arguments

- x:

  The model matrix

- y:

  A vector of numeric values with a length equal to `nrow(x)`

- risky:

  Indices to columns in `x` corresponding to primary cells classified as
  risky (interval limits not reached)

- candidates:

  Indices to columns in `x` that are candidates for becoming additional
  primary cells. Higher order cells must be included so that
  parent-child relationships are seen.

- allDims:

  When TRUE, a primary cell is added for each dimension. can be
  specified as a vector of length `length(risky)`

## Value

Additional primary cells as indices to columns in `x`.

## Details

For a single `risky` cell, the algorithm can be formulated as:

- Consider this cell as a `child` and identify all `parents` that are
  present in `candidates`.

- Remove parents who are also parents of other parents (i.e., eliminate
  higher-level parents).

- Identify the children of these remaining parents that are included in
  `candidates`.

- Select the child that has the smallest value in the numeric variable
  (`y`).

For several `risky` cells, coordination takes place. See the comment
below the examples.

## Examples

``` r
# Example inspired by suppression with maxN = 5
d1 <- SSBtoolsData("d1")
mm <- SSBtools::ModelMatrix(d1, dimVar = 1:2, crossTable = TRUE)
x <- mm$modelMatrix
y <- Matrix::crossprod(x, d1$freq)

risky <- c(13, 15, 40, 45)
candidates <- c(1:12, 14, 16, 17, 19, 21, 21, 24, 26:37, 39, 42, 44)

info <- rep("", length(y))
info[risky ] <- "risky"
info[candidates] <- "c"
cbind(mm$crossTable, y=as.vector(y), info)
#>    region main_income   y  info
#> 1   Total       Total 596     c
#> 2   Total  assistance 283     c
#> 3   Total       other  72     c
#> 4   Total    pensions 189     c
#> 5   Total       wages  52     c
#> 6       A       Total 113     c
#> 7       A  assistance  55     c
#> 8       A       other  11     c
#> 9       A    pensions  36     c
#> 10      A       wages  11     c
#> 11      B       Total  55     c
#> 12      B  assistance  29     c
#> 13      B       other   7 risky
#> 14      B    pensions  18     c
#> 15      B       wages   1 risky
#> 16      C       Total  73     c
#> 17      C  assistance  35     c
#> 18      C       other   5      
#> 19      C    pensions  25     c
#> 20      C       wages   8      
#> 21      D       Total  45     c
#> 22      D  assistance  17      
#> 23      D       other  13      
#> 24      D    pensions  13     c
#> 25      D       wages   2      
#> 26      E       Total 138     c
#> 27      E  assistance  63     c
#> 28      E       other   9     c
#> 29      E    pensions  52     c
#> 30      E       wages  14     c
#> 31      F       Total  67     c
#> 32      F  assistance  24     c
#> 33      F       other  12     c
#> 34      F    pensions  22     c
#> 35      F       wages   9     c
#> 36      G       Total  40     c
#> 37      G  assistance  22     c
#> 38      G       other   6      
#> 39      G    pensions   8     c
#> 40      G       wages   4 risky
#> 41      H       Total  65      
#> 42      H  assistance  38     c
#> 43      H       other   9      
#> 44      H    pensions  15     c
#> 45      H       wages   3 risky

PrimaryFromRiskyDefault(x = x, y = y, risky = risky, candidates = candidates)
#> [1] 28 35
PrimaryFromRiskyDefault(x = x, y = y, risky = 40, candidates = candidates)
#> [1] 39

# The last solution (39) is not included in the first (28, 35). 
# This is because 39 is not needed when 35 is already included.
```
