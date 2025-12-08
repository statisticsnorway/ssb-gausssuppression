# Counts of input code contributions

Counts of input code contributions

## Usage

``` r
HierarchyContributors(data, x, crossTable, hierarchies, inputInOutput = TRUE)
```

## Arguments

- data:

  input data

- x:

  model matrix as created by `ModelMatrix` with `data`, `hierarchies`
  and `inputInOutput` as input

- crossTable:

  `crossTable` as created by `ModelMatrix` with `data`, `hierarchies`
  and `inputInOutput` as input

- hierarchies:

  Standardized hierarchies. That is, output from `AutoHierarchies`.

- inputInOutput:

  `ModelMatrix` input.

## Value

List of data frames of counts associated with `crossTable`

- **`min`:** Minimum number of times a contributing input code
  contributes

- **`max`:** Maximum number of times a contributing input code
  contributes

- **`n`:** Number of contributing input codes

- **`ac`:** Theoretical number of contributing input codes according to
  the hierarchy

## Examples

``` r
z <- SSBtoolsData("sprt_emp_withEU")[-(1:3), ]
z$age[z$age == "Y15-29"] <- "young"
z$age[z$age == "Y30-64"] <- "old"
hi <- SSBtools::FindHierarchies(z[, -4])
inputInOutput <- c(TRUE, FALSE, FALSE)
mm <- SSBtools::ModelMatrix(z, hi, crossTable = TRUE, inputInOutput = inputInOutput)

out <- HierarchyContributors(z, mm$modelMatrix, mm$crossTable, hi, inputInOutput)

# The nonzero values are caused by the removed three data rows
cbind(mm$crossTable, out$max - out$min)
#>     age   geo  year age geo year
#> 1 Total Total Total   3   0    3
#> 2 Total    EU Total   2   0    2
#> 3 Total nonEU Total   1   0    1
#> 4   old Total Total   0   0    0
#> 5   old    EU Total   0   0    0
#> 6   old nonEU Total   0   0    0
#> 7 young Total Total   0   0    0
#> 8 young    EU Total   0   0    0
#> 9 young nonEU Total   0   0    0
cbind(mm$crossTable, out$ac - out$n)
#>     age   geo  year age geo year
#> 1 Total Total Total   0   0    0
#> 2 Total    EU Total   0   0    0
#> 3 Total nonEU Total   0   0    0
#> 4   old Total Total   0   0    0
#> 5   old    EU Total   0   0    0
#> 6   old nonEU Total   0   0    0
#> 7 young Total Total   0   0    1
#> 8 young    EU Total   0   0    1
#> 9 young nonEU Total   0   0    1
```
