# Linked table suppression

## Introduction

By using the formula parameter, it is already possible to protect linked
tables with the functions described in the other vignettes. The result
is the strictest form of protection, which we call global protection.

This vignette illustrates alternative methods for linked tables. A
common method for such protection is *back-tracking* where one iterates
until a consistent solution is found. In the functions described below,
such a method can be achieved by specifying
`linkedGauss = "back-tracking"`. With the GaussSuppression package, one
can find such a consistent solution using an improved approach, avoiding
the need for iteration.

Below we start with some examples of protected tables with alternative
methods. Then we show in more detail different function calls that
achieve this. We also discuss the parameters `recordAware` and
`collapseAware`.

Finally, an example with interval protection is also shown.

## Input data and examples

We use a modified version of the *example 1* dataset used elsewhere.

``` r
library(GaussSuppression)
dataset <- SSBtoolsData("example1")
dataset <- dataset[c(1, 2, 4, 6, 8, 10, 12, 13, 14, 15), ]
dataset$freq = c(6, 8, 9, 1, 2, 4, 3, 7, 2, 2)
print(dataset)
#>      age      geo    eu year freq
#> 1  young    Spain    EU 2014    6
#> 2  young  Iceland nonEU 2014    8
#> 4    old    Spain    EU 2014    9
#> 6    old Portugal    EU 2014    1
#> 8  young  Iceland nonEU 2015    2
#> 10   old    Spain    EU 2015    4
#> 12   old Portugal    EU 2015    3
#> 13 young    Spain    EU 2016    7
#> 14 young  Iceland nonEU 2016    2
#> 15 young Portugal    EU 2016    2
```

In the examples, we work with two linked tables:  
- a three-way table where `age`, `eu`, and `year` are crossed, and  
- a two-way table where `geo` and `year` are crossed.

In this example, small counts (1s and 2s) are protected. All zeros are
treated as known structural zeros and are omitted from both the input
and the output.

As in the other vignettes, primary suppressed cells are underlined and
labeled in red, while the secondary suppressed cells are labeled in
purple.

We first illustrate local protection, where the tables are protected
separately without any coordination between them.

  

**Table 1**: Linked suppressed tables by  
**`linkedGauss = "local"`**

|   age |  year | nonEU |  EU | Total |
|------:|------:|------:|----:|------:|
| young |  2014 |     8 |   6 |    14 |
| young |  2015 |     2 |     |     2 |
| young |  2016 |     2 |   9 |    11 |
| young | Total |    12 |  15 |    27 |
|   old |  2014 |       |  10 |    10 |
|   old |  2015 |       |   7 |     7 |
|   old |  2016 |       |     |       |
|   old | Total |       |  17 |    17 |
| Total |  2014 |     8 |  16 |    24 |
| Total |  2015 |     2 |   7 |     9 |
| Total |  2016 |     2 |   9 |    11 |
| Total | Total |    12 |  32 |    44 |

|  year | Iceland | Portugal | Spain | Total |
|------:|--------:|---------:|------:|------:|
|  2014 |       8 |        1 |    15 |    24 |
|  2015 |       2 |        3 |     4 |     9 |
|  2016 |       2 |        2 |     7 |    11 |
| Total |      12 |        6 |    26 |    44 |

  

Clearly, this is not a satisfactory solution. The totals for 2015 and
2016 are suppressed in one table, but not in the other. Furthermore,
there is also an inconsistency for *Iceland-2014*, which is the same as
*nonEU-2014*.

We continue with consistent protection.

  

**Table 2**: Linked suppressed tables by  
**`linkedGauss = "consistent"`**

|   age |  year | nonEU |  EU | Total |
|------:|------:|------:|----:|------:|
| young |  2014 |     8 |   6 |    14 |
| young |  2015 |     2 |     |     2 |
| young |  2016 |     2 |   9 |    11 |
| young | Total |    12 |  15 |    27 |
|   old |  2014 |       |  10 |    10 |
|   old |  2015 |       |   7 |     7 |
|   old |  2016 |       |     |       |
|   old | Total |       |  17 |    17 |
| Total |  2014 |     8 |  16 |    24 |
| Total |  2015 |     2 |   7 |     9 |
| Total |  2016 |     2 |   9 |    11 |
| Total | Total |    12 |  32 |    44 |

|  year | Iceland | Portugal | Spain | Total |
|------:|--------:|---------:|------:|------:|
|  2014 |       8 |        1 |    15 |    24 |
|  2015 |       2 |        3 |     4 |     9 |
|  2016 |       2 |        2 |     7 |    11 |
| Total |      12 |        6 |    26 |    44 |

  

The inconsistency problems are now avoided.

However, a remaining problem with this solution is that *Spain-2015* can
be derived from *EU-2015* and *Portugal-2015*.

Finally, we illustrate an improved form of consistent protection,
denoted as *super-consistent*, which also avoids this problem.

  

**Table 3**: Linked suppressed tables by  
**`linkedGauss = "super-consistent"`**

|   age |  year | nonEU |  EU | Total |
|------:|------:|------:|----:|------:|
| young |  2014 |     8 |   6 |    14 |
| young |  2015 |     2 |     |     2 |
| young |  2016 |     2 |   9 |    11 |
| young | Total |    12 |  15 |    27 |
|   old |  2014 |       |  10 |    10 |
|   old |  2015 |       |   7 |     7 |
|   old |  2016 |       |     |       |
|   old | Total |       |  17 |    17 |
| Total |  2014 |     8 |  16 |    24 |
| Total |  2015 |     2 |   7 |     9 |
| Total |  2016 |     2 |   9 |    11 |
| Total | Total |    12 |  32 |    44 |

|  year | Iceland | Portugal | Spain | Total |
|------:|--------:|---------:|------:|------:|
|  2014 |       8 |        1 |    15 |    24 |
|  2015 |       2 |        3 |     4 |     9 |
|  2016 |       2 |        2 |     7 |    11 |
| Total |      12 |        6 |    26 |    44 |

  

The suppressed cells in each table correspond to related equations that
cannot be solved. The *super-consistent* method makes use of the fact
that common cells across tables must have the same value. Thus, the
equations from the different tables can be combined when searching for
solutions. The *super-consistent* method ensures that suppressed cells
cannot be uniquely determined from the combined system of equations.
However, the coordination is not as strict as in the global method,
where the system of equations becomes even larger. In this particular
case, the *super-consistent* solution turns out to be the same as the
global one.

## Function calls and output

To achieve both treating zeros as known structural zeros and omitting
them from the output, we use the parameter settings `extend0 = FALSE`
and `removeEmpty = TRUE`.

In
[`SuppressLinkedTables()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/SuppressLinkedTables.md),
the argument `withinArg` specifies which parameters may differ between
the linked tables. In our examples, we choose this to be either
`dimVar`, `hierarchies`, or `formula`.

The output from
[`SuppressLinkedTables()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/SuppressLinkedTables.md)
is a list, with one element for each of the linked tables.

## `SuppressLinkedTables()` with `dimVar`

``` r
output <- SuppressLinkedTables(data = dataset,
              fun = SuppressSmallCounts, 
              withinArg = list(table_1 = list(dimVar = c("age", "eu", "year")), 
                               table_2 = list(dimVar = c("geo", "year"))),
              freqVar = "freq", 
              maxN = 2,
              extend0 = FALSE, 
              removeEmpty = TRUE,
              linkedGauss = "super-consistent")
#> [preAggregate 10*12->7*11]
#> [preAggregate 10*12->9*10]
#> 
#> ====== Linked GaussSuppression by "super-consistent" algorithm:
#> 
#> GaussSuppression_anySum: .....................................

print(output[["table_1"]])
#>      age    eu  year freq primary suppressed
#> 1  Total Total Total   44   FALSE      FALSE
#> 2  Total Total  2014   24   FALSE      FALSE
#> 3  Total Total  2015    9   FALSE       TRUE
#> 4  Total Total  2016   11   FALSE       TRUE
#> 5  Total    EU Total   32   FALSE      FALSE
#> 6  Total    EU  2014   16   FALSE      FALSE
#> 7  Total    EU  2015    7   FALSE      FALSE
#> 8  Total    EU  2016    9   FALSE      FALSE
#> 9  Total nonEU Total   12   FALSE      FALSE
#> 10 Total nonEU  2014    8   FALSE      FALSE
#> 11 Total nonEU  2015    2    TRUE       TRUE
#> 12 Total nonEU  2016    2    TRUE       TRUE
#> 13   old Total Total   17   FALSE      FALSE
#> 14   old Total  2014   10   FALSE      FALSE
#> 15   old Total  2015    7   FALSE      FALSE
#> 16   old    EU Total   17   FALSE      FALSE
#> 17   old    EU  2014   10   FALSE      FALSE
#> 18   old    EU  2015    7   FALSE      FALSE
#> 19 young Total Total   27   FALSE      FALSE
#> 20 young Total  2014   14   FALSE      FALSE
#> 21 young Total  2015    2    TRUE       TRUE
#> 22 young Total  2016   11   FALSE       TRUE
#> 23 young    EU Total   15   FALSE      FALSE
#> 24 young    EU  2014    6   FALSE      FALSE
#> 25 young    EU  2016    9   FALSE      FALSE
#> 26 young nonEU Total   12   FALSE      FALSE
#> 27 young nonEU  2014    8   FALSE      FALSE
#> 28 young nonEU  2015    2    TRUE       TRUE
#> 29 young nonEU  2016    2    TRUE       TRUE
print(output[["table_2"]])
#>         geo  year freq primary suppressed
#> 1     Total Total   44   FALSE      FALSE
#> 2     Total  2014   24   FALSE      FALSE
#> 3     Total  2015    9   FALSE       TRUE
#> 4     Total  2016   11   FALSE       TRUE
#> 5   Iceland Total   12   FALSE      FALSE
#> 6   Iceland  2014    8   FALSE      FALSE
#> 7   Iceland  2015    2    TRUE       TRUE
#> 8   Iceland  2016    2    TRUE       TRUE
#> 9  Portugal Total    6   FALSE      FALSE
#> 10 Portugal  2014    1    TRUE       TRUE
#> 11 Portugal  2015    3   FALSE      FALSE
#> 12 Portugal  2016    2    TRUE       TRUE
#> 13    Spain Total   26   FALSE      FALSE
#> 14    Spain  2014   15   FALSE       TRUE
#> 15    Spain  2015    4   FALSE      FALSE
#> 16    Spain  2016    7   FALSE       TRUE
```

## `SuppressLinkedTables()` with `hierarchies`

First, we need hierarchies for the input. Here, these are generated
separately with
[`SSBtools::FindDimLists()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/FindDimLists.html).

``` r
h_age  <- SSBtools::FindDimLists(dataset["age"])[[1]]
h_geo  <- SSBtools::FindDimLists(dataset["geo"])[[1]]
h_eu   <- SSBtools::FindDimLists(dataset["eu"])[[1]]
h_year <- SSBtools::FindDimLists(dataset["year"])[[1]]
  
print(h_age)
#>   levels codes
#> 1      @ Total
#> 2     @@   old
#> 3     @@ young
print(h_geo)
#>   levels    codes
#> 1      @    Total
#> 2     @@  Iceland
#> 3     @@ Portugal
#> 4     @@    Spain
print(h_eu)
#>   levels codes
#> 1      @ Total
#> 2     @@    EU
#> 3     @@ nonEU
print(h_year)
#>   levels codes
#> 1      @ Total
#> 2     @@  2014
#> 3     @@  2015
#> 4     @@  2016
```

The output is identical to using `dimVar`, so we only show the code.
Note that the only difference is the `withinArg` argument.

``` r
output <- SuppressLinkedTables(data = dataset,
              fun = SuppressSmallCounts, 
              withinArg = 
                list(table_1 = list(hierarchies = list(age = h_age, eu = h_eu, year = h_year)), 
                     table_2 = list(hierarchies = list(geo = h_geo, year = h_year))),
              freqVar = "freq", 
              maxN = 2,
              extend0 = FALSE, 
              removeEmpty = TRUE,
              linkedGauss = "super-consistent")
```

## `SuppressLinkedTables()` with `formula`

When using `formula`, the output is similar to that obtained with
`dimVar` or `hierarchies`. The only difference in the output is the
ordering of rows, so we only show the code.

Again, the only difference in the code is the `withinArg` argument.
However, note that we have omitted `removeEmpty = TRUE` here, since this
is the default when a formula is used as input.

``` r
output <- SuppressLinkedTables(data = dataset,
              fun = SuppressSmallCounts, 
              withinArg = list(table_1 = list(formula = ~age*eu*year), 
                               table_2 = list(formula = ~geo*year)),
              freqVar = "freq", 
              maxN = 2,
              extend0 = FALSE,
              linkedGauss = "super-consistent")
```

## `SuppressSmallCounts()` with `formula` and `linkedGauss`

Since only the `formula` parameter varies between the linked tables, one
option is to run
[`SuppressSmallCounts()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/SuppressSmallCounts.md)
directly with `formula` as a list and the `linkedGauss` parameter
specified. Here we show 10 output rows.

``` r
output <- SuppressSmallCounts(data = dataset,
              formula = list(table_1 = ~age*eu*year, table_2 = ~geo*year),   
              freqVar = "freq",
              maxN = 2,
              extend0 = FALSE,
              linkedGauss = "super-consistent") 
#> 
#> ====== Linked GaussSuppression by "super-consistent" algorithm:
#> 
#> GaussSuppression_anySum: ....................................
print(output[c(1, 6:7, 12, 19, 23, 25:28), ])
#>      age  year      geo freq primary suppressed
#> 1  Total Total    Total   44   FALSE      FALSE
#> 6  Total  2014    Total   24   FALSE      FALSE
#> 7  Total  2015    Total    9   FALSE       TRUE
#> 12   old Total       EU   17   FALSE      FALSE
#> 19 young  2016    Total   11   FALSE       TRUE
#> 23 Total  2014    nonEU    8   FALSE      FALSE
#> 25 Total  2016    nonEU    2    TRUE       TRUE
#> 26 Total  2014  Iceland    8   FALSE      FALSE
#> 27 Total  2014 Portugal    1    TRUE       TRUE
#> 28 Total  2014    Spain   15   FALSE       TRUE
```

## `tables_by_formulas()` with `formula` and `linkedGauss`

Similar output can be obtained by
[`tables_by_formulas()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/tables_by_formulas.html).
In this case, the region variable is specified manually, and table
membership variables are included in the output. Again, 10 output rows
are shown.

``` r
output <-  tables_by_formulas(data = dataset,
              table_fun = SuppressSmallCounts,                
              table_formulas = list(table_1 = ~age*eu*year, table_2 = ~geo*year),   
              freqVar = "freq",
              maxN = 2,
              extend0 = FALSE,
              linkedGauss = "super-consistent",
              substitute_vars = list(region = c("geo", "eu"))) 
#> 
#> ====== Linked GaussSuppression by "super-consistent" algorithm:
#> 
#> GaussSuppression_anySum: ....................................
              
print(output[c(1, 6:7, 12, 19, 23, 25:28), ])
#>      age  year   region freq primary suppressed table_1 table_2
#> 1  Total Total    Total   44   FALSE      FALSE    TRUE    TRUE
#> 6  Total  2014    Total   24   FALSE      FALSE    TRUE    TRUE
#> 7  Total  2015    Total    9   FALSE       TRUE    TRUE    TRUE
#> 12   old Total       EU   17   FALSE      FALSE    TRUE   FALSE
#> 19 young  2016    Total   11   FALSE       TRUE    TRUE   FALSE
#> 23 Total  2014    nonEU    8   FALSE      FALSE    TRUE   FALSE
#> 25 Total  2016    nonEU    2    TRUE       TRUE    TRUE   FALSE
#> 26 Total  2014  Iceland    8   FALSE      FALSE   FALSE    TRUE
#> 27 Total  2014 Portugal    1    TRUE       TRUE   FALSE    TRUE
#> 28 Total  2014    Spain   15   FALSE       TRUE   FALSE    TRUE
```

## The parameters `recordAware` and `collapseAware`

An important issue is which cells are considered common cells. In the
functions, the parameter `recordAware` is set to `TRUE` by default. In
this case, common cells are determined based on whether they aggregate
the same underlying records. This is similar to the use of cell keys, a
well-known concept from the cell-key method of statistical disclosure
control.

When `recordAware = FALSE`, common cells are instead identified by
matching variable combinations. This does not always work well. For
example, here `recordAware = TRUE` is necessary to capture that
*Iceland-2014* and *nonEU-2014* are the same.

A related parameter is `collapseAware`, but it is not available when
using
[`SuppressLinkedTables()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/SuppressLinkedTables.md).
When it is used, even more cells are treated as common cells. In
particular, the suppression algorithm then automatically accounts for
cells in one table that are sums of cells in another table. In our
example, this means that the combination `"consistent"` and
`collapseAware = TRUE` gives the same result as `"super-consistent"`.

For more details on parameters and options, see the documentation for
[`SuppressLinkedTables()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/SuppressLinkedTables.md).

## Interval protection

Intervals for the primary suppressed cells are computed whenever the
`lpPackage` parameter is specified. When
`linkedGauss = "super-consistent"`, intervals can be calculated using
this method as well.

There are several possibilities. See the documentation for the parameter
`linkedIntervals` in the help page for
[`SuppressLinkedTables()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/SuppressLinkedTables.md).

If `rangePercent` and/or `rangeMin` are provided, further suppression is
performed to ensure that the interval width requirements are met. See
the help page for
[`GaussSuppressionFromData()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/GaussSuppressionFromData.md),
under the description of the `lpPackage` parameter, for more details.

In the example below, the required interval width is 4. To achieve this,
two additional cells are suppressed: *Portugal-2015* and *Spain-2015*.
Without this additional suppression, some intervals are as narrow as 3
(see variables `lo_1` and `up_1` below).

  

**Table 4**: Linked suppressed tables with intervals by  
**`linkedGauss = "super-consistent", rangeMin = 4`**

|   age |  year |      nonEU |  EU |      Total |
|------:|------:|-----------:|----:|-----------:|
| young |  2014 |          8 |   6 |         14 |
| young |  2015 | 2 \[0, 4\] |     | 2 \[0, 4\] |
| young |  2016 | 2 \[0, 4\] |   9 |         11 |
| young | Total |         12 |  15 |         27 |
|   old |  2014 |            |  10 |         10 |
|   old |  2015 |            |   7 |          7 |
|   old |  2016 |            |     |            |
|   old | Total |            |  17 |         17 |
| Total |  2014 |          8 |  16 |         24 |
| Total |  2015 | 2 \[0, 4\] |   7 |          9 |
| Total |  2016 | 2 \[0, 4\] |   9 |         11 |
| Total | Total |         12 |  32 |         44 |

|  year |    Iceland |   Portugal | Spain | Total |
|------:|-----------:|-----------:|------:|------:|
|  2014 |          8 | 1 \[0, 6\] |    15 |    24 |
|  2015 | 2 \[0, 4\] |          3 |     4 |     9 |
|  2016 | 2 \[0, 4\] | 2 \[0, 6\] |     7 |    11 |
| Total |         12 |          6 |    26 |    44 |

  

This functionality can be used with all the function calls above. Below
is shown
[`SuppressLinkedTables()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/SuppressLinkedTables.md)
with `dimVar`.

``` r
output <- SuppressLinkedTables(data = dataset,
                               fun = SuppressSmallCounts, 
                               withinArg = list(table_1 = list(dimVar = c("age", "eu", "year")), 
                                                table_2 = list(dimVar = c("geo", "year"))),
                               freqVar = "freq", 
                               maxN = 2,
                               extend0 = FALSE, 
                               removeEmpty = TRUE,
                               linkedGauss = "super-consistent",
                               lpPackage = "highs", 
                               rangeMin = 4)
#> [preAggregate 10*12->7*11]
#> [preAggregate 10*12->9*10]
#> 
#> ====== Linked GaussSuppression by "super-consistent" algorithm:
#> 
#> GaussSuppression_anySum: .....................................
#> (16*18-0exact->9*5-DDcol2->9*3-GaussI->9*3)
#> 
#> Using highs for intervals...
#> ----
#> (16*18)
#> ..................
#> 10+1-6+2-5+3+
#>   2: 1 new, (4.000) 1-
#>   1: 2 new, (3.000) 1+
#> GaussSuppression_none: .............................
#> (16*16-0exact->11*5-DDcol2->11*3-GaussI->11*3)
#> 
#> Using highs for intervals...
#> ----

print(output[["table_1"]])
#>      age    eu  year freq rlim_freq lo_1 up_1 lo up suppressed_integer primary
#> 1  Total Total Total   44        NA   NA   NA NA NA                  0   FALSE
#> 2  Total Total  2014   24        NA   NA   NA NA NA                  0   FALSE
#> 3  Total Total  2015    9        NA   NA   NA NA NA                  2   FALSE
#> 4  Total Total  2016   11        NA   NA   NA NA NA                  2   FALSE
#> 5  Total    EU Total   32        NA   NA   NA NA NA                  0   FALSE
#> 6  Total    EU  2014   16        NA   NA   NA NA NA                  0   FALSE
#> 7  Total    EU  2015    7        NA   NA   NA NA NA                  0   FALSE
#> 8  Total    EU  2016    9        NA   NA   NA NA NA                  0   FALSE
#> 9  Total nonEU Total   12        NA   NA   NA NA NA                  0   FALSE
#> 10 Total nonEU  2014    8        NA   NA   NA NA NA                  0   FALSE
#> 11 Total nonEU  2015    2         4    0    4  0  4                  1    TRUE
#> 12 Total nonEU  2016    2         4    0    4  0  4                  1    TRUE
#> 13   old Total Total   17        NA   NA   NA NA NA                  0   FALSE
#> 14   old Total  2014   10        NA   NA   NA NA NA                  0   FALSE
#> 15   old Total  2015    7        NA   NA   NA NA NA                  0   FALSE
#> 16   old    EU Total   17        NA   NA   NA NA NA                  0   FALSE
#> 17   old    EU  2014   10        NA   NA   NA NA NA                  0   FALSE
#> 18   old    EU  2015    7        NA   NA   NA NA NA                  0   FALSE
#> 19 young Total Total   27        NA   NA   NA NA NA                  0   FALSE
#> 20 young Total  2014   14        NA   NA   NA NA NA                  0   FALSE
#> 21 young Total  2015    2         4    0    4  0  4                  1    TRUE
#> 22 young Total  2016   11        NA   NA   NA NA NA                  2   FALSE
#> 23 young    EU Total   15        NA   NA   NA NA NA                  0   FALSE
#> 24 young    EU  2014    6        NA   NA   NA NA NA                  0   FALSE
#> 25 young    EU  2016    9        NA   NA   NA NA NA                  0   FALSE
#> 26 young nonEU Total   12        NA   NA   NA NA NA                  0   FALSE
#> 27 young nonEU  2014    8        NA   NA   NA NA NA                  0   FALSE
#> 28 young nonEU  2015    2         4    0    4  0  4                  1    TRUE
#> 29 young nonEU  2016    2         4    0    4  0  4                  1    TRUE
#>    suppressed
#> 1       FALSE
#> 2       FALSE
#> 3        TRUE
#> 4        TRUE
#> 5       FALSE
#> 6       FALSE
#> 7       FALSE
#> 8       FALSE
#> 9       FALSE
#> 10      FALSE
#> 11       TRUE
#> 12       TRUE
#> 13      FALSE
#> 14      FALSE
#> 15      FALSE
#> 16      FALSE
#> 17      FALSE
#> 18      FALSE
#> 19      FALSE
#> 20      FALSE
#> 21       TRUE
#> 22       TRUE
#> 23      FALSE
#> 24      FALSE
#> 25      FALSE
#> 26      FALSE
#> 27      FALSE
#> 28       TRUE
#> 29       TRUE
print(output[["table_2"]])
#>         geo  year freq rlim_freq lo_1 up_1 lo up suppressed_integer primary
#> 1     Total Total   44        NA   NA   NA NA NA                  0   FALSE
#> 2     Total  2014   24        NA   NA   NA NA NA                  0   FALSE
#> 3     Total  2015    9        NA   NA   NA NA NA                  2   FALSE
#> 4     Total  2016   11        NA   NA   NA NA NA                  2   FALSE
#> 5   Iceland Total   12        NA   NA   NA NA NA                  0   FALSE
#> 6   Iceland  2014    8        NA   NA   NA NA NA                  0   FALSE
#> 7   Iceland  2015    2         4    0    4  0  4                  1    TRUE
#> 8   Iceland  2016    2         4    0    4  0  4                  1    TRUE
#> 9  Portugal Total    6        NA   NA   NA NA NA                  0   FALSE
#> 10 Portugal  2014    1         4    0    3  0  6                  1    TRUE
#> 11 Portugal  2015    3        NA   NA   NA NA NA                  3   FALSE
#> 12 Portugal  2016    2         4    0    3  0  6                  1    TRUE
#> 13    Spain Total   26        NA   NA   NA NA NA                  0   FALSE
#> 14    Spain  2014   15        NA   NA   NA NA NA                  2   FALSE
#> 15    Spain  2015    4        NA   NA   NA NA NA                  3   FALSE
#> 16    Spain  2016    7        NA   NA   NA NA NA                  2   FALSE
#>    suppressed
#> 1       FALSE
#> 2       FALSE
#> 3        TRUE
#> 4        TRUE
#> 5       FALSE
#> 6       FALSE
#> 7        TRUE
#> 8        TRUE
#> 9       FALSE
#> 10       TRUE
#> 11       TRUE
#> 12       TRUE
#> 13      FALSE
#> 14       TRUE
#> 15       TRUE
#> 16       TRUE
```
