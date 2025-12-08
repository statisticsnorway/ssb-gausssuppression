# Find major contributions to aggregates

Assuming aggregates are calculated via a dummy matrix by
`z = t(x) %*% y`, the `n` largest contributions are found (value or
index) for each aggregate.

## Usage

``` r
MaxContribution(
  x,
  y,
  n = 1,
  decreasing = TRUE,
  index = FALSE,
  groups = NULL,
  return2 = FALSE
)
```

## Arguments

- x:

  A (sparse) dummy matrix

- y:

  Vector of input values (contributors)

- n:

  Number of contributors to be found

- decreasing:

  Ordering parameter. Smallest contributors found when `FALSE`.

- index:

  Indices to `y` returned when TRUE

- groups:

  When non-NULL, major contributions after aggregation within groups.
  Cannot be combined with `index = TRUE`. The missing group category is
  excluded.

- return2:

  When `TRUE`, two matrices are returned, `value` and `id`. The latter
  contains indices when `group` is `NULL` and otherwise a character
  matrix of groups.

## Value

Matrix with lagest contributions in first column, second largest in
second column and so on. Alternative output when using parameters
`index` or `return2`.

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

a <- ModelMatrix(z, formula = ~age + geo, crossTable = TRUE)

cbind(as.data.frame(a$crossTable), MaxContribution(a$modelMatrix, z$ths_per, 1))
#>     age      geo MaxContribution(a$modelMatrix, z$ths_per, 1)
#> 1 Total    Total                                        122.1
#> 2   old    Total                                        122.1
#> 3 young    Total                                         69.1
#> 4 Total  Iceland                                          1.9
#> 5 Total Portugal                                         25.8
#> 6 Total    Spain                                        122.1
cbind(a$crossTable, MaxContribution(a$modelMatrix, z$ths_per, 10))
#>     age      geo     1     2     3    4    5    6    7    8    9   10
#> 1 Total    Total 122.1 120.3 119.6 69.1 66.9 63.4 25.8 24.3 20.2 14.2
#> 2   old    Total 122.1 120.3 119.6 25.8 24.3 20.2  1.9  1.6  1.5   NA
#> 3 young    Total  69.1  66.9  63.4 14.2 12.7 11.6  1.9  1.9  1.8   NA
#> 4 Total  Iceland   1.9   1.9   1.9  1.8  1.6  1.5   NA   NA   NA   NA
#> 5 Total Portugal  25.8  24.3  20.2 14.2 12.7 11.6   NA   NA   NA   NA
#> 6 Total    Spain 122.1 120.3 119.6 69.1 66.9 63.4   NA   NA   NA   NA
cbind(a$crossTable, MaxContribution(a$modelMatrix, z$ths_per, 10, index = TRUE))
#>     age      geo  1  2  3  4  5 6  7  8  9 10
#> 1 Total    Total 16  4 10 13  1 7 18 12  6  9
#> 2   old    Total 16  4 10 18 12 6 17 11  5 NA
#> 3 young    Total 13  1  7  9 15 3  8 14  2 NA
#> 4 Total  Iceland  8 14 17  2 11 5 NA NA NA NA
#> 5 Total Portugal 18 12  6  9 15 3 NA NA NA NA
#> 6 Total    Spain 16  4 10 13  1 7 NA NA NA NA

# Both types of output can be achieved with return2 = TRUE)
identical(MaxContribution(a$modelMatrix, z$ths_per, 10, return2 = TRUE),
          list(value =  MaxContribution(a$modelMatrix, z$ths_per, 10), 
               id =  MaxContribution(a$modelMatrix, z$ths_per, 10, index = TRUE)))
#> [1] FALSE

b <- ModelMatrix(z[, -4], crossTable = TRUE, inputInOutput = c(TRUE, FALSE, TRUE))

k <- cbind(b$crossTable, MaxContribution(b$modelMatrix, z$ths_per, 10))

gr18 <- paste0("g", 1:18)                          # Each row is a group
k18 <- cbind(b$crossTable, MaxContribution(b$modelMatrix, z$ths_per, 10, groups = gr18))
identical(k, k18) # TRUE
#> [1] TRUE

gr9 <- paste0("g", as.integer(10 * z$ths_per)%%10) # 9 groups from decimal
k9 <- cbind(b$crossTable, MaxContribution(b$modelMatrix, z$ths_per, 10, groups = gr9))

k18[c(4, 13, 17, 33), ]
#>      age   geo  year     1     2     3    4    5    6   7   8   9 10
#> 4  Total Total  2016 122.1  69.1  25.8 12.7  1.9  1.9  NA  NA  NA NA
#> 13   old Total Total 122.1 120.3 119.6 25.8 24.3 20.2 1.9 1.6 1.5 NA
#> 17   old    EU Total 122.1 120.3 119.6 25.8 24.3 20.2  NA  NA  NA NA
#> 33 young nonEU Total   1.9   1.9   1.8   NA   NA   NA  NA  NA  NA NA
k9[c(4, 13, 17, 33), ]
#>      age   geo  year     1     2     3    4    5   6   7  8  9 10
#> 4  Total Total  2016 191.2  25.8  12.7  3.8   NA  NA  NA NA NA NA
#> 13   old Total Total 144.6 122.1 121.2 25.8 20.2 1.9 1.5 NA NA NA
#> 17   old    EU Total 144.6 122.1 119.6 25.8 20.2  NA  NA NA NA NA
#> 33 young nonEU Total   3.8   1.8    NA   NA   NA  NA  NA NA NA NA

# Group info obtained with return2 = TRUE
k9_id <- cbind(b$crossTable, MaxContribution(b$modelMatrix, z$ths_per, 10, groups = gr9, 
                                             return2 = TRUE)$id)
k9_id[c(4, 13, 17, 33), ]
#>      age   geo  year  1  2    3    4    5    6    7    8    9   10
#> 4  Total Total  2016 g1 g8   g7   g9 <NA> <NA> <NA> <NA> <NA> <NA>
#> 13   old Total Total g3 g1   g6   g8   g2   g9   g5 <NA> <NA> <NA>
#> 17   old    EU Total g3 g1   g6   g8   g2 <NA> <NA> <NA> <NA> <NA>
#> 33 young nonEU Total g9 g8 <NA> <NA> <NA> <NA> <NA> <NA> <NA> <NA>


# Verify similarity
z$y <- z$ths_per + (1:nrow(z))/100  # to avoid equal values
id1 <- MaxContribution(b$modelMatrix, z$y, 10, index = TRUE)
id1[!is.na(id1)] <- paste0("g", id1[!is.na(id1)])
mc2 <- MaxContribution(b$modelMatrix, z$y, 10, groups = gr18, return2 = TRUE)
id2 <- mc2$id
identical(id1, id2)
#> [1] TRUE
```
