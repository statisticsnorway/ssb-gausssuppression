# Small count frequency table suppression

## Introduction

The `GaussSuppression` package contains several easy-to-use wrapper
functions and in this vignette we will look at the `SuppressSmallCounts`
function. In this function, small frequencies are primary suppressed.
Then, as always in this package, secondary suppression is performed
using the Gauss method.

We begin by creating datasets to be used below. The first examples are
based on `dataset_a`, which has six rows.

``` r
library(GaussSuppression)
dataset <- SSBtoolsData("example1")
dataset_a <- dataset[dataset$year == "2014", -4]
dataset_b <- dataset[dataset$year == "2015", -4]
dataset_a
#>     age      geo    eu freq
#> 1 young    Spain    EU    5
#> 2 young  Iceland nonEU    2
#> 3 young Portugal    EU    0
#> 4   old    Spain    EU    6
#> 5   old  Iceland nonEU    3
#> 6   old Portugal    EU    4
```

## An initial basic example

In the function description
([`?SuppressSmallCounts`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/SuppressSmallCounts.md)),
the only visible parameter is `maxN` in addition to the parameters
considered in the define-tables vignette. In the first example, we use
`maxN = 1` which means that zeros and ones are primary suppressed.

``` r
SuppressSmallCounts(data = dataset_a, 
                    dimVar = c("age", "geo"), 
                    freqVar = "freq", 
                    maxN = 1)
#> [extend0 6*3->6*3]
#> GaussSuppression_anySum: ...........
#>      age      geo freq primary suppressed
#> 1  Total    Total   20   FALSE      FALSE
#> 2  Total  Iceland    5   FALSE      FALSE
#> 3  Total Portugal    4   FALSE      FALSE
#> 4  Total    Spain   11   FALSE      FALSE
#> 5    old    Total   13   FALSE      FALSE
#> 6    old  Iceland    3   FALSE       TRUE
#> 7    old Portugal    4   FALSE       TRUE
#> 8    old    Spain    6   FALSE      FALSE
#> 9  young    Total    7   FALSE      FALSE
#> 10 young  Iceland    2   FALSE       TRUE
#> 11 young Portugal    0    TRUE       TRUE
#> 12 young    Spain    5   FALSE      FALSE
```

A formatted version of this output is given in Table 1 below. Primary
suppressed cells are underlined and labeled in red, while the secondary
suppressed cells are labeled in purple.

  

**Table 1**: `dimVar = c("age", "geo"), maxN = 1`

|   age | Iceland | Portugal | Spain | Total |
|------:|--------:|---------:|------:|------:|
| young |       2 |        0 |     5 |     7 |
|   old |       3 |        4 |     6 |    13 |
| Total |       5 |        4 |    11 |    20 |

  

The same output is obtained if microdata is sent as input as illustrated
by de code below.

``` r
microdata_a <- SSBtools::MakeMicro(dataset_a, "freq")[-4]
output <- SuppressSmallCounts(data = microdata_a, 
                              dimVar = c("age", "geo"), 
                              maxN = 1)
#> [preAggregate 20*3->5*3]
#> [extend0 5*3->6*3]
#> GaussSuppression_anySum: ...........
```

A related point is that the third row of the table can be omitted
(`data = dataset_a[-3, ]`) since the frequency is zero. When the
frequency is zero, there is no underlying microdata. Later in this
vignette, we address scenarios where the inclusion of zeros may be
important.

## An hierarchical table

A more advanced example is obtained by including the variable “eu”.

``` r
SuppressSmallCounts(data = dataset_a, 
                    dimVar = c("age", "geo", "eu"), 
                    freqVar = "freq", 
                    maxN = 2)
#> [extend0 6*4->6*4]
#> GaussSuppression_anySum: .............
#>      age      geo freq primary suppressed
#> 1  Total    Total   20   FALSE      FALSE
#> 2  Total       EU   15   FALSE      FALSE
#> 3  Total    nonEU    5   FALSE      FALSE
#> 4  Total  Iceland    5   FALSE      FALSE
#> 5  Total Portugal    4   FALSE      FALSE
#> 6  Total    Spain   11   FALSE      FALSE
#> 7    old    Total   13   FALSE      FALSE
#> 8    old       EU   10   FALSE       TRUE
#> 9    old    nonEU    3   FALSE       TRUE
#> 10   old  Iceland    3   FALSE       TRUE
#> 11   old Portugal    4   FALSE       TRUE
#> 12   old    Spain    6   FALSE      FALSE
#> 13 young    Total    7   FALSE      FALSE
#> 14 young       EU    5   FALSE       TRUE
#> 15 young    nonEU    2    TRUE       TRUE
#> 16 young  Iceland    2    TRUE       TRUE
#> 17 young Portugal    0    TRUE       TRUE
#> 18 young    Spain    5   FALSE      FALSE
```

A formatted version of this output:

  

**Table 2**: `dimVar = c("age", "geo", "eu"), maxN = 2`

|   age | Iceland | Portugal | Spain | nonEU |  EU | Total |
|------:|--------:|---------:|------:|------:|----:|------:|
| young |       2 |        0 |     5 |     2 |   5 |     7 |
|   old |       3 |        4 |     6 |     3 |  10 |    13 |
| Total |       5 |        4 |    11 |     5 |  15 |    20 |

  

As described in the define-tables vignette hierarchies are here detected
automatically. The same output is obtained if we first generate
hierarchies by:

``` r
dimlists <- SSBtools::FindDimLists(dataset_a[c("age", "geo", "eu")])
dimlists
#> $age
#>   levels codes
#> 1      @ Total
#> 2     @@   old
#> 3     @@ young
#> 
#> $geo
#>   levels    codes
#> 1      @    Total
#> 2     @@       EU
#> 3    @@@ Portugal
#> 4    @@@    Spain
#> 5     @@    nonEU
#> 6    @@@  Iceland
```

And thereafter run SuppressSmallCounts with these hierarchies as input:

``` r
SuppressSmallCounts(data = dataset_a[c("age", "geo", "freq")], 
                    hierarchies = dimlists, 
                    freqVar = "freq", 
                    maxN = 2)
```

## Using the formula interface

Using the formula interface is one way to achieve fewer cells in the
output:

``` r
SuppressSmallCounts(data = dataset_a, 
                    formula = ~age:eu + geo, 
                    freqVar = "freq", 
                    maxN = 2)
#> [extend0 6*4->6*4]
#> GaussSuppression_anySum: .......
#>     age      geo freq primary suppressed
#> 1 Total    Total   20   FALSE      FALSE
#> 2 Total  Iceland    5   FALSE      FALSE
#> 3 Total Portugal    4   FALSE      FALSE
#> 4 Total    Spain   11   FALSE      FALSE
#> 5   old       EU   10   FALSE      FALSE
#> 6   old    nonEU    3   FALSE       TRUE
#> 7 young       EU    5   FALSE      FALSE
#> 8 young    nonEU    2    TRUE       TRUE
```

In the formatted version of this output, blank cells indicate that they
are not included in the output.

  

**Table 3**: `formula = ~age:eu + geo, maxN = 2`

|   age | Iceland | Portugal | Spain | nonEU |  EU | Total |
|------:|--------:|---------:|------:|------:|----:|------:|
| young |         |          |       |     2 |   5 |       |
|   old |         |          |       |     3 |  10 |       |
| Total |       5 |        4 |    11 |       |     |    20 |

  

## About suppression of zeros

By default, zeros are suppressed in order to protect against attribute
disclosure in frequency tables. However, there are exceptions. Below are
several options for handling exceptions.

### Zeros not suppressed

One option is to use `protectZeros = FALSE`.

``` r
SuppressSmallCounts(data = dataset_a, 
                    dimVar = c("age", "geo", "eu"),  
                    freqVar = "freq", 
                    maxN = 4,
                    protectZeros = FALSE)
#> [extend0 6*4->6*4]
#> GaussSuppression_anySum: ...........
#>      age      geo freq primary suppressed
#> 1  Total    Total   20   FALSE      FALSE
#> 2  Total       EU   15   FALSE      FALSE
#> 3  Total    nonEU    5   FALSE      FALSE
#> 4  Total  Iceland    5   FALSE      FALSE
#> 5  Total Portugal    4    TRUE       TRUE
#> 6  Total    Spain   11   FALSE       TRUE
#> 7    old    Total   13   FALSE      FALSE
#> 8    old       EU   10   FALSE       TRUE
#> 9    old    nonEU    3    TRUE       TRUE
#> 10   old  Iceland    3    TRUE       TRUE
#> 11   old Portugal    4    TRUE       TRUE
#> 12   old    Spain    6   FALSE      FALSE
#> 13 young    Total    7   FALSE      FALSE
#> 14 young       EU    5   FALSE       TRUE
#> 15 young    nonEU    2    TRUE       TRUE
#> 16 young  Iceland    2    TRUE       TRUE
#> 17 young Portugal    0   FALSE      FALSE
#> 18 young    Spain    5   FALSE       TRUE
```

  

**Table 4**:
`dimVar = c("age", "geo", "eu"), maxN = 4, protectZeros = FALSE`

|   age | Iceland | Portugal | Spain | nonEU |  EU | Total |
|------:|--------:|---------:|------:|------:|----:|------:|
| young |       2 |        0 |     5 |     2 |   5 |     7 |
|   old |       3 |        4 |     6 |     3 |  10 |    13 |
| Total |       5 |        4 |    11 |     5 |  15 |    20 |

  

Another possibility that gives the same output is:

``` r
output <- SuppressSmallCounts(data = dataset_a[-3, ], 
                              dimVar = c("age", "geo", "eu"),  
                              freqVar = "freq", 
                              maxN = 4,
                              extend0 = FALSE, 
                              structuralEmpty = TRUE)
#> GaussSuppression_anySum: ..........
```

Here the zero-frequency row is omitted in the input. By default, the
table is automatically extended so that the Gauss algorithm handles
zeros correctly. When this is turned off (`extend0 = FALSE`), a warning
with the following text will appear: “*Suppressed cells with empty input
will not be protected. Extend input data with zeros?*”. However, with
`structuralEmpty = TRUE`, the “empty zeros” are assumed to represent
structural zeros that must not be suppressed. As exemplified a little
further below, one can thus handle data with both structural and
non-structural zeros.

### Secondary suppressed zeros

We can combine `protectZeros = FALSE` with `secondaryZeros = TRUE`.

``` r
SuppressSmallCounts(data = dataset_a, 
                    dimVar = c("age", "geo", "eu"),  
                    freqVar = "freq", 
                    maxN = 3,
                    protectZeros = FALSE, 
                    secondaryZeros = TRUE)
#> [extend0 6*4->6*4]
#> GaussSuppression_anySumNOTprimary: .............
#>      age      geo freq primary suppressed
#> 1  Total    Total   20   FALSE      FALSE
#> 2  Total       EU   15   FALSE      FALSE
#> 3  Total    nonEU    5   FALSE      FALSE
#> 4  Total  Iceland    5   FALSE      FALSE
#> 5  Total Portugal    4   FALSE      FALSE
#> 6  Total    Spain   11   FALSE      FALSE
#> 7    old    Total   13   FALSE      FALSE
#> 8    old       EU   10   FALSE       TRUE
#> 9    old    nonEU    3    TRUE       TRUE
#> 10   old  Iceland    3    TRUE       TRUE
#> 11   old Portugal    4   FALSE       TRUE
#> 12   old    Spain    6   FALSE      FALSE
#> 13 young    Total    7   FALSE      FALSE
#> 14 young       EU    5   FALSE       TRUE
#> 15 young    nonEU    2    TRUE       TRUE
#> 16 young  Iceland    2    TRUE       TRUE
#> 17 young Portugal    0   FALSE       TRUE
#> 18 young    Spain    5   FALSE      FALSE
```

  

**Table 5**: `dimVar = c("age", "geo", "eu"), maxN = 3,`  
`protectZeros = FALSE, secondaryZeros = TRUE`

|   age | Iceland | Portugal | Spain | nonEU |  EU | Total |
|------:|--------:|---------:|------:|------:|----:|------:|
| young |       2 |        0 |     5 |     2 |   5 |     7 |
|   old |       3 |        4 |     6 |     3 |  10 |    13 |
| Total |       5 |        4 |    11 |     5 |  15 |    20 |

  

### Both structural and non-structural zeros

The example below uses `dataset_b`, which has two zeros.

``` r
  dataset_b
#>      age      geo    eu freq
#> 7  young    Spain    EU    5
#> 8  young  Iceland nonEU    0
#> 9  young Portugal    EU    0
#> 10   old    Spain    EU    6
#> 11   old  Iceland nonEU    3
#> 12   old Portugal    EU    4
```

Let’s assume that the first zero is considered as a structural zero. In
order to account for this characteristic, we will exclude this
particular zero and retain the other. As a general rule, we will exclude
all structural zeros.

``` r
SuppressSmallCounts(data = dataset_b[-2,  ], 
                    dimVar = c("age", "geo", "eu"),  
                    freqVar = "freq", 
                    maxN = 2,
                    extend0 = FALSE, 
                    structuralEmpty = TRUE)
#> GaussSuppression_anySum: .............
#>      age      geo freq primary suppressed
#> 1  Total    Total   18   FALSE      FALSE
#> 2  Total       EU   15   FALSE      FALSE
#> 3  Total    nonEU    3   FALSE      FALSE
#> 4  Total  Iceland    3   FALSE      FALSE
#> 5  Total Portugal    4   FALSE      FALSE
#> 6  Total    Spain   11   FALSE      FALSE
#> 7    old    Total   13   FALSE      FALSE
#> 8    old       EU   10   FALSE      FALSE
#> 9    old    nonEU    3   FALSE      FALSE
#> 10   old  Iceland    3   FALSE      FALSE
#> 11   old Portugal    4   FALSE       TRUE
#> 12   old    Spain    6   FALSE       TRUE
#> 13 young    Total    5   FALSE      FALSE
#> 14 young       EU    5   FALSE      FALSE
#> 15 young    nonEU    0   FALSE      FALSE
#> 16 young  Iceland    0   FALSE      FALSE
#> 17 young Portugal    0    TRUE       TRUE
#> 18 young    Spain    5   FALSE       TRUE
```

  

**Table 6**: `dimVar = c("age", "geo", "eu"), maxN = 2,`  
`extend0 = FALSE, structuralEmpty = TRUE`

|   age | Iceland | Portugal | Spain | nonEU |  EU | Total |
|------:|--------:|---------:|------:|------:|----:|------:|
| young |       0 |        0 |     5 |     0 |   5 |     5 |
|   old |       3 |        4 |     6 |     3 |  10 |    13 |
| Total |       3 |        4 |    11 |     3 |  15 |    18 |

  

Now, the data has been processed correctly, the structural zeros will be
published while the other zeros are suppressed.

To get the same output with the formula interface, we can use the
following code:

``` r
SuppressSmallCounts(data = dataset_b[-2,  ], 
                    formula = ~age * (geo + eu),  
                    freqVar = "freq", 
                    maxN = 2,
                    extend0 = FALSE, 
                    structuralEmpty = TRUE,
                    removeEmpty = FALSE)
```

Please note that in order to include empty cells in the output, you need
to set the `removeEmpty` parameter to `FALSE`. By default, this
parameter is set to `TRUE` when using the formula interface.

## The problem of singletons and zeros

### The problem of zeros

When using the standard suppression technique on table `dataset_b`, many
cells are suppressed.

``` r
SuppressSmallCounts(data = dataset_b, 
                    dimVar = c("age", "geo", "eu"),  
                    freqVar = "freq", 
                    maxN = 2)
#> [extend0 6*4->6*4]
#> GaussSuppression_anySum: .............
#>      age      geo freq primary suppressed
#> 1  Total    Total   18   FALSE      FALSE
#> 2  Total       EU   15   FALSE      FALSE
#> 3  Total    nonEU    3   FALSE      FALSE
#> 4  Total  Iceland    3   FALSE      FALSE
#> 5  Total Portugal    4   FALSE      FALSE
#> 6  Total    Spain   11   FALSE      FALSE
#> 7    old    Total   13   FALSE      FALSE
#> 8    old       EU   10   FALSE       TRUE
#> 9    old    nonEU    3   FALSE       TRUE
#> 10   old  Iceland    3   FALSE       TRUE
#> 11   old Portugal    4   FALSE       TRUE
#> 12   old    Spain    6   FALSE       TRUE
#> 13 young    Total    5   FALSE      FALSE
#> 14 young       EU    5   FALSE       TRUE
#> 15 young    nonEU    0    TRUE       TRUE
#> 16 young  Iceland    0    TRUE       TRUE
#> 17 young Portugal    0    TRUE       TRUE
#> 18 young    Spain    5   FALSE       TRUE
```

  

**Table 7**: `dimVar = c("age", "geo", "eu"), maxN = 2`

|   age | Iceland | Portugal | Spain | nonEU |  EU | Total |
|------:|--------:|---------:|------:|------:|----:|------:|
| young |       0 |        0 |     5 |     0 |   5 |     5 |
|   old |       3 |        4 |     6 |     3 |  10 |    13 |
| Total |       3 |        4 |    11 |     3 |  15 |    18 |

  

The reason for the Spain suppressions is to prevent the disclosure of
zeros, which would be easily revealed if *young:Spain* is not
suppressed. In that case the sum of *young:Iceland* and *young:Portugal*
can easily be calculated to be zero. Since negative frequencies are not
possible, the only possibility is two zeros.

The handling of this problem is standard, but it can be turned off by
`singletonMethod = "none"`.

### The problem of singletons

This problem occurs when `protectZeros = FALSE` and
`secondaryZeros = FALSE` (default). We now also look at a larger example
that uses `dataset` which has 18 rows.

``` r
output <- SuppressSmallCounts(data = dataset, 
                              formula = ~age*geo*year + eu*year,  
                              freqVar = "freq", 
                              maxN = 1, 
                              protectZeros = FALSE)
#> [extend0 18*5->18*5]
#> GaussSuppression_anySum: .................................................
head(output)
#>     age      geo  year freq primary suppressed
#> 1 Total    Total Total   59   FALSE      FALSE
#> 2   old    Total Total   38   FALSE      FALSE
#> 3 young    Total Total   21   FALSE      FALSE
#> 4 Total  Iceland Total   13   FALSE      FALSE
#> 5 Total Portugal Total   12   FALSE      FALSE
#> 6 Total    Spain Total   34   FALSE      FALSE
```

  

**Table 8**:
`formula = ~age*geo*year + eu*year, maxN = 1, protectZeros = FALSE`

|   age |  year | Iceland | Portugal | Spain | nonEU |  EU | Total |
|------:|------:|--------:|---------:|------:|------:|----:|------:|
| young |  2014 |       2 |        0 |     5 |       |     |     7 |
| young |  2015 |       0 |        0 |     5 |       |     |     5 |
| young |  2016 |       1 |        1 |     7 |       |     |     9 |
| young | Total |       3 |        1 |    17 |       |     |    21 |
|   old |  2014 |       3 |        4 |     6 |       |     |    13 |
|   old |  2015 |       3 |        4 |     6 |       |     |    13 |
|   old |  2016 |       4 |        3 |     5 |       |     |    12 |
|   old | Total |      10 |       11 |    17 |       |     |    38 |
| Total |  2014 |       5 |        4 |    11 |     5 |  15 |    20 |
| Total |  2015 |       3 |        4 |    11 |     3 |  15 |    18 |
| Total |  2016 |       5 |        4 |    12 |     5 |  16 |    21 |
| Total | Total |      13 |       12 |    34 |    13 |  46 |    59 |

  

In this output, *young:2016:Spain* is suppressed due to the standard
handling of the singleton problem.

However, by using `singletonMethod = "none"` in this case,
*young:2016:Spain* will not be suppressed. Then the sum of
*young:2016:Iceland* and *young:2016:Portugal* can easily be calculated
to be two. Since zeros are never suppressed, the only possible values
for these two cells are two ones.

## Intervals

Intervals for the primarily suppressed cells are computed whenever the
`lpPackage` parameter is specified. Additionally, if `rangePercent`
and/or `rangeMin` are provided, further suppression is performed to
ensure that the interval width requirements are met. See the
documentation for the `lpPackage` parameter in
[`?GaussSuppressionFromData`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/GaussSuppressionFromData.md)
for more details.

In the example below, the interval widths are 4 after applying the
standard Gaussian suppression algorithm. Since a minimum width of 5 is
required, the two cells for Spain are additionally suppressed.

``` r
output <- SuppressSmallCounts(data = dataset, 
                              formula = ~age*geo,  
                              freqVar = "freq", 
                              maxN = 3, 
                              rangeMin = 5,
                              lpPackage = "Rglpk")
#> [preAggregate 18*5->6*3]
#> [extend0 6*3->6*3]
#> GaussSuppression_anySum: ..........
#> (6*8-0exact->4*5-GaussI->4*3)
#> 
#> Using Rglpk for intervals...
#> --
#> (6*8)
#> ........
#> 5+1-4-
#>   4: 1 new, (17.00) 2-3-
#>   3: 2 new, (17.00) 2+1+
#> GaussSuppression_none: ......
#> (6*6-0exact->6*6-GaussI->6*4)
#> 
#> Using Rglpk for intervals...
#> --
tail(output) 
#>      age      geo freq rlim_freq lo_1 up_1 lo up suppressed_integer primary
#> 7    old  Iceland   10        NA   NA   NA NA NA                  2   FALSE
#> 8    old Portugal   11        NA   NA   NA NA NA                  2   FALSE
#> 9    old    Spain   17        NA   NA   NA NA NA                  3   FALSE
#> 10 young  Iceland    3         5    0    4  0 13                  1    TRUE
#> 11 young Portugal    1         5    0    4  0 12                  1    TRUE
#> 12 young    Spain   17        NA   NA   NA NA NA                  3   FALSE
#>    suppressed
#> 7        TRUE
#> 8        TRUE
#> 9        TRUE
#> 10       TRUE
#> 11       TRUE
#> 12       TRUE
```

  

In the formatted output shown in Table 9 below, the final intervals are
given in parentheses.

  

**Table 9**: Output from `SuppressSmallCounts` with
`formula = ~age*geo`, `maxN = 3`, `rangeMin = 5`, `lpPackage = "Rglpk"`

|   age |     Iceland |    Portugal | Spain | Total |
|------:|------------:|------------:|------:|------:|
| young | 3 \[0, 13\] | 1 \[0, 12\] |    17 |    21 |
|   old |          10 |          11 |    17 |    38 |
| Total |          13 |          12 |    34 |    59 |

  
