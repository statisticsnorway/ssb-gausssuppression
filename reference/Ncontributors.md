# Find the number of unique groups contributing to aggregates

Assuming aggregates are calculated via a dummy matrix by
`z = t(x) %*% y`, the the number of unique contributing groups,
according to a grouping variable, are found for each aggregate. The
missing group category is not counted.

## Usage

``` r
Ncontributors(x, groups)
```

## Arguments

- x:

  A (sparse) dummy matrix

- groups:

  Vector of group categories

## Value

Vector of numbers of unique groups

## See also

[`ModelMatrix`](https://statisticsnorway.github.io/ssb-ssbtools/reference/ModelMatrix.html)

## Author

Øyvind Langsrud

## Examples

``` r
library(SSBtools)

z <- SSBtoolsData("sprt_emp_withEU")
z$age[z$age == "Y15-29"] <- "young"
z$age[z$age == "Y30-64"] <- "old"
z$groups <- c("A", "A", "B", "A", "B", "C")

a <- ModelMatrix(z, formula = ~age*eu + geo + year, crossTable = TRUE)

cbind(as.data.frame(a$crossTable), nGroups = Ncontributors(a$modelMatrix, z$groups))
#>      age      geo  year nGroups
#> 1  Total    Total Total       3
#> 2    old    Total Total       3
#> 3  young    Total Total       2
#> 4  Total       EU Total       3
#> 5  Total    nonEU Total       2
#> 6  Total  Iceland Total       2
#> 7  Total Portugal Total       2
#> 8  Total    Spain Total       1
#> 9  Total    Total  2014       3
#> 10 Total    Total  2015       3
#> 11 Total    Total  2016       3
#> 12   old       EU Total       2
#> 13   old    nonEU Total       1
#> 14 young       EU Total       2
#> 15 young    nonEU Total       1
cbind(as.data.frame(a$crossTable), nYears = Ncontributors(a$modelMatrix, z$year))
#>      age      geo  year nYears
#> 1  Total    Total Total      3
#> 2    old    Total Total      3
#> 3  young    Total Total      3
#> 4  Total       EU Total      3
#> 5  Total    nonEU Total      3
#> 6  Total  Iceland Total      3
#> 7  Total Portugal Total      3
#> 8  Total    Spain Total      3
#> 9  Total    Total  2014      1
#> 10 Total    Total  2015      1
#> 11 Total    Total  2016      1
#> 12   old       EU Total      3
#> 13   old    nonEU Total      3
#> 14 young       EU Total      3
#> 15 young    nonEU Total      3
cbind(as.data.frame(a$crossTable), nUnique_ths_per = Ncontributors(a$modelMatrix, z$ths_per))
#>      age      geo  year nUnique_ths_per
#> 1  Total    Total Total              16
#> 2    old    Total Total               9
#> 3  young    Total Total               8
#> 4  Total       EU Total              12
#> 5  Total    nonEU Total               4
#> 6  Total  Iceland Total               4
#> 7  Total Portugal Total               6
#> 8  Total    Spain Total               6
#> 9  Total    Total  2014               6
#> 10 Total    Total  2015               6
#> 11 Total    Total  2016               5
#> 12   old       EU Total               6
#> 13   old    nonEU Total               3
#> 14 young       EU Total               6
#> 15 young    nonEU Total               2
```
