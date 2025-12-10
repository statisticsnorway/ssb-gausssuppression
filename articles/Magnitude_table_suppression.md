# Magnitude table suppression

## Introduction

The `GaussSuppression` package contains several easy-to-use wrapper
functions and in this vignette we will look at the
`SuppressFewContributors` and `SuppressDominantCells` functions. In
these functions, primary suppression is based on the number of
contributors or by dominance rules. Then, as always in this package,
secondary suppression is performed using the Gauss method.

We begin by loading a dataset to be used below.

``` r
library(GaussSuppression)
dataset <- SSBtoolsData("magnitude1")
dataset
#>          sector4 sector2      geo    eu company value
#> 1    Agriculture private Portugal    EU       A  75.9
#> 2    Agriculture private Portugal    EU       B  24.5
#> 3    Agriculture private    Spain    EU       A  96.6
#> 4    Agriculture private    Spain    EU       B  43.2
#> 5  Entertainment private  Iceland nonEU       B  16.8
#> 6  Entertainment private Portugal    EU       B   7.1
#> 7  Entertainment private Portugal    EU       D   2.3
#> 8  Entertainment private    Spain    EU       A  77.4
#> 9  Entertainment private    Spain    EU       B  11.5
#> 10 Entertainment private    Spain    EU       C  16.4
#> 11  Governmental  public Portugal    EU       B  21.6
#> 12  Governmental  public Portugal    EU       D   2.0
#> 13  Governmental  public    Spain    EU       C   6.5
#> 14  Governmental  public    Spain    EU       D   2.7
#> 15      Industry private  Iceland nonEU       B   9.6
#> 16      Industry private  Iceland nonEU       C   8.8
#> 17      Industry private  Iceland nonEU       D   1.9
#> 18      Industry private Portugal    EU       B  25.7
#> 19      Industry private Portugal    EU       D   3.4
#> 20      Industry private    Spain    EU       C   8.4
```

We can imagine the figures in the variable `"value"` represent sales
value to different sectors from different companies. In the first
examples, we will not use the `"company"` variable, but instead assume
that each row represents the contribution of a unique company. Our input
data can then be reformatted and illustrated like this:

  

|       sector4 |       Iceland |   Portugal |            Spain |
|--------------:|--------------:|-----------:|-----------------:|
|   Agriculture |               | 75.9, 24.5 |       96.6, 43.2 |
| Entertainment |          16.8 |   7.1, 2.3 | 77.4, 11.5, 16.4 |
|  Governmental |               |  21.6, 2.0 |         6.5, 2.7 |
|      Industry | 9.6, 8.8, 1.9 |  25.7, 3.4 |              8.4 |

**Table 1**: Input data with the 20 contributions.

  

## Initial basic examples

### Using few contributors primary suppression

In the first example, we use `SuppressFewContributors` with `maxN = 1`.
This means that cells based on a single contributor are primary
suppressed.

``` r
SuppressFewContributors(data=dataset, 
                        numVar = "value", 
                        dimVar= c("sector4", "geo"), 
                        maxN=1)
#> [extraAggregate 20*3->10*5] Checking ....
#> GaussSuppression_numttHTT: ..............::::
#>          sector4      geo value nRule nAll primary suppressed
#> 1          Total    Total 462.3    20   20   FALSE      FALSE
#> 2          Total  Iceland  37.1     4    4   FALSE      FALSE
#> 3          Total Portugal 162.5     8    8   FALSE      FALSE
#> 4          Total    Spain 262.7     8    8   FALSE      FALSE
#> 5    Agriculture    Total 240.2     4    4   FALSE      FALSE
#> 6    Agriculture  Iceland   0.0     0    0   FALSE      FALSE
#> 7    Agriculture Portugal 100.4     2    2   FALSE      FALSE
#> 8    Agriculture    Spain 139.8     2    2   FALSE      FALSE
#> 9  Entertainment    Total 131.5     6    6   FALSE      FALSE
#> 10 Entertainment  Iceland  16.8     1    1    TRUE       TRUE
#> 11 Entertainment Portugal   9.4     2    2   FALSE       TRUE
#> 12 Entertainment    Spain 105.3     3    3   FALSE      FALSE
#> 13  Governmental    Total  32.8     4    4   FALSE      FALSE
#> 14  Governmental  Iceland   0.0     0    0   FALSE      FALSE
#> 15  Governmental Portugal  23.6     2    2   FALSE       TRUE
#> 16  Governmental    Spain   9.2     2    2   FALSE       TRUE
#> 17      Industry    Total  57.8     6    6   FALSE      FALSE
#> 18      Industry  Iceland  20.3     3    3   FALSE       TRUE
#> 19      Industry Portugal  29.1     2    2   FALSE       TRUE
#> 20      Industry    Spain   8.4     1    1    TRUE       TRUE
```

In the output, the number of contributors is in columns `nRule` and
`nAll`. The two columns are equal under normal usage.

A formatted version of this output is given in Table 2 below. Primary
suppressed cells are underlined and labeled in red, while the secondary
suppressed cells are labeled in purple.

  

|       sector4 |   Iceland |   Portugal |      Spain |       Total |
|--------------:|----------:|-----------:|-----------:|------------:|
|   Agriculture |     0 (0) |  100.4 (2) |  139.8 (2) |   240.2 (4) |
| Entertainment |  16.8 (1) |    9.4 (2) |  105.3 (3) |   131.5 (6) |
|  Governmental |     0 (0) |   23.6 (2) |    9.2 (2) |    32.8 (4) |
|      Industry |  20.3 (3) |   29.1 (2) |    8.4 (1) |    57.8 (6) |
|         Total |  37.1 (4) |  162.5 (8) |  262.7 (8) |  462.3 (20) |

**Table 2**: Output from `SuppressFewContributors` with `maxN = 1`
(number of contributors in parenthesis)

  

### Using dominant cell primary suppression

In the second example, we use `SuppressDominantCells` with `n = 1` and
`k = 80`. This means that aggregates are primary suppressed whenever the
largest contribution exceeds 80% of the cell total.

``` r
SuppressDominantCells(data=dataset, 
                      numVar = "value", 
                      dimVar= c("sector4", "geo"), 
                      n = 1, k = 80, allDominance = TRUE)
#> [extraAggregate 20*3->10*5] Checking ....
#> GaussSuppression_numttHTT: ..............::
#>          sector4      geo value dominant1 max1contributor n_contr n_non0_contr
#> 1          Total    Total 462.3 0.2089552               3      20           20
#> 2          Total  Iceland  37.1 0.4528302               5       4            4
#> 3          Total Portugal 162.5 0.4670769               1       8            8
#> 4          Total    Spain 262.7 0.3677198               3       8            8
#> 5    Agriculture    Total 240.2 0.4021649               3       4            4
#> 6    Agriculture  Iceland   0.0 0.0000000              NA       0            0
#> 7    Agriculture Portugal 100.4 0.7559761               1       2            2
#> 8    Agriculture    Spain 139.8 0.6909871               3       2            2
#> 9  Entertainment    Total 131.5 0.5885932               8       6            6
#> 10 Entertainment  Iceland  16.8 1.0000000               5       1            1
#> 11 Entertainment Portugal   9.4 0.7553191               6       2            2
#> 12 Entertainment    Spain 105.3 0.7350427               8       3            3
#> 13  Governmental    Total  32.8 0.6585366              11       4            4
#> 14  Governmental  Iceland   0.0 0.0000000              NA       0            0
#> 15  Governmental Portugal  23.6 0.9152542              11       2            2
#> 16  Governmental    Spain   9.2 0.7065217              13       2            2
#> 17      Industry    Total  57.8 0.4446367              18       6            6
#> 18      Industry  Iceland  20.3 0.4729064              15       3            3
#> 19      Industry Portugal  29.1 0.8831615              18       2            2
#> 20      Industry    Spain   8.4 1.0000000              20       1            1
#>    primary suppressed
#> 1    FALSE      FALSE
#> 2    FALSE      FALSE
#> 3    FALSE      FALSE
#> 4    FALSE      FALSE
#> 5    FALSE      FALSE
#> 6    FALSE      FALSE
#> 7    FALSE      FALSE
#> 8    FALSE      FALSE
#> 9    FALSE      FALSE
#> 10    TRUE       TRUE
#> 11   FALSE       TRUE
#> 12   FALSE      FALSE
#> 13   FALSE       TRUE
#> 14   FALSE      FALSE
#> 15    TRUE       TRUE
#> 16   FALSE       TRUE
#> 17   FALSE       TRUE
#> 18   FALSE       TRUE
#> 19    TRUE       TRUE
#> 20    TRUE       TRUE
```

To incorporate the percentage of the two largest contributions in the
output, the parameter `allDominance = TRUE` was utilized. A formatted
version of this output is given in Table 3 below.

  

|       sector4 |      Iceland |     Portugal |        Spain |        Total |
|--------------:|-------------:|-------------:|-------------:|-------------:|
|   Agriculture |       0 (0%) |  100.4 (76%) |  139.8 (69%) |  240.2 (40%) |
| Entertainment |  16.8 (100%) |    9.4 (76%) |  105.3 (74%) |  131.5 (59%) |
|  Governmental |       0 (0%) |   23.6 (92%) |    9.2 (71%) |   32.8 (66%) |
|      Industry |   20.3 (47%) |   29.1 (88%) |   8.4 (100%) |   57.8 (44%) |
|         Total |   37.1 (45%) |  162.5 (47%) |  262.7 (37%) |  462.3 (21%) |

**Table 3**: Output from `SuppressDominantCells`  
with `n = 1` and `k = 80`  
(percentage from largest contribution in parenthesis)

Note that this table, as well as Table 2, is discussed below in the
section on the singleton problem.  
  

## An hierarchical table and two dominance rules

Here we use `SuppressDominantCells` with `n = 1:2` and `k = c(80, 99)`.
This means that aggregates are primary suppressed whenever the largest
contribution exceeds 80% of the cell total or when the two largest
contributions exceed 99% of the cell total.

In addition, the example below is made even more advanced by including
the variables “sector2” and “eu”.

``` r
output <- SuppressDominantCells(data=dataset, 
                                numVar = "value", 
                                dimVar= c("sector4", "sector2", "geo", "eu"), 
                                n = 1:2, k = c(80, 99))
#> [extraAggregate 20*5->10*7] Checking ....
#> GaussSuppression_numttHTT: .............::::::
head(output)
#>   sector4      geo value primary suppressed
#> 1   Total    Total 462.3   FALSE      FALSE
#> 2   Total       EU 425.2   FALSE      FALSE
#> 3   Total    nonEU  37.1   FALSE      FALSE
#> 4   Total  Iceland  37.1   FALSE      FALSE
#> 5   Total Portugal 162.5   FALSE      FALSE
#> 6   Total    Spain 262.7   FALSE      FALSE
```

  

|       sector4 | Iceland | Portugal | Spain | nonEU |    EU | Total |
|--------------:|--------:|---------:|------:|------:|------:|------:|
|   Agriculture |       0 |    100.4 | 139.8 |     0 | 240.2 | 240.2 |
| Entertainment |    16.8 |      9.4 | 105.3 |  16.8 | 114.7 | 131.5 |
|  Governmental |       0 |     23.6 |   9.2 |     0 |  32.8 |  32.8 |
|      Industry |    20.3 |     29.1 |   8.4 |  20.3 |  37.5 |  57.8 |
|       private |    37.1 |    138.9 | 253.5 |  37.1 | 392.4 | 429.5 |
|        public |       0 |     23.6 |   9.2 |     0 |  32.8 |  32.8 |
|         Total |    37.1 |    162.5 | 262.7 |  37.1 | 425.2 | 462.3 |

**Table 4**: Output from `SuppressDominantCells`  
with `n = 1:2` and `k = c(80, 99)`

  

As described in the define-tables vignette hierarchies are here detected
automatically. The same output is obtained if we first generate
hierarchies by:

``` r
dimlists <- SSBtools::FindDimLists(dataset[c("sector4", "sector2", "geo", "eu")])
dimlists
#> $sector4
#>   levels         codes
#> 1      @         Total
#> 2     @@       private
#> 3    @@@   Agriculture
#> 4    @@@ Entertainment
#> 5    @@@      Industry
#> 6     @@        public
#> 7    @@@  Governmental
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

And thereafter run SuppressDominantCells with these hierarchies as
input:

``` r
output <- SuppressDominantCells(data=dataset, 
                                numVar = "value", 
                                hierarchies = dimlists,  
                                n = 1:2, k = c(80, 99))
#> [extraAggregate 20*3->10*5] Checking ....
#> GaussSuppression_numttHTT: .............::::::
```

  
  

## Using the formula interface

Using the formula interface is one way to achieve fewer cells in the
output. Below we use `SuppressFewContributors` with `maxN = 2`. This
means that table cells based on one or two contributors are primary
suppressed.

``` r
output <- SuppressFewContributors(data=dataset, 
                                  numVar = "value", 
                                  formula = ~sector2*geo + sector4*eu, 
                                  maxN=2,
                                  removeEmpty = FALSE)
#> [extraAggregate 20*5->10*7] Checking ....
#> GaussSuppression_numttHTT: ............:::::
head(output)
#>        geo sector4 value nRule nAll primary suppressed
#> 1    Total   Total 462.3    20   20   FALSE      FALSE
#> 2    Total private 429.5    16   16   FALSE      FALSE
#> 3    Total  public  32.8     4    4   FALSE      FALSE
#> 4  Iceland   Total  37.1     4    4   FALSE      FALSE
#> 5 Portugal   Total 162.5     8    8   FALSE      FALSE
#> 6    Spain   Total 262.7     8    8   FALSE      FALSE
tail(output)
#>      geo       sector4 value nRule nAll primary suppressed
#> 21    EU Entertainment 114.7     5    5   FALSE       TRUE
#> 22 nonEU Entertainment  16.8     1    1    TRUE       TRUE
#> 23    EU  Governmental  32.8     4    4   FALSE      FALSE
#> 24 nonEU  Governmental   0.0     0    0   FALSE      FALSE
#> 25    EU      Industry  37.5     3    3   FALSE       TRUE
#> 26 nonEU      Industry  20.3     3    3   FALSE       TRUE
```

In the formatted version of this output, blank cells indicate that they
are not included in the output.

  

|       sector4 |   Iceland |   Portugal |      Spain |     nonEU |          EU |       Total |
|--------------:|----------:|-----------:|-----------:|----------:|------------:|------------:|
|   Agriculture |           |            |            |     0 (0) |   240.2 (4) |   240.2 (4) |
| Entertainment |           |            |            |  16.8 (1) |   114.7 (5) |   131.5 (6) |
|  Governmental |           |            |            |     0 (0) |    32.8 (4) |    32.8 (4) |
|      Industry |           |            |            |  20.3 (3) |    37.5 (3) |    57.8 (6) |
|       private |  37.1 (4) |  138.9 (6) |  253.5 (6) |           |             |  429.5 (16) |
|        public |     0 (0) |   23.6 (2) |    9.2 (2) |           |             |    32.8 (4) |
|         Total |  37.1 (4) |  162.5 (8) |  262.7 (8) |  37.1 (4) |  425.2 (16) |  462.3 (20) |

**Table 5**: Output from `SuppressFewContributors` with `maxN = 2`  
(number of contributors in parenthesis)

  

Please note that in order to include the three empty cells with no
contributors, the `removeEmpty` parameter was set to `FALSE`. By
default, this parameter is set to `TRUE` when using the formula
interface.  
  

## Using the contributorVar parameter (`contributorVar = "company"`)

According to the `"company"` variable in the data set, there are only
four contribution companies (A, B, C and D). We specify this using the
`contributorVar` parameter, which corresponds to a variable within the
dataset, in this case, `"company"`. In general, this variable refers to
the holding information to be used by the suppression method. When this
is taken into account, the primary suppression rules will be applied to
data that, within each cell, is aggregated within each contributor. Our
example data aggregated in this way is shown below.

  

|       sector4 |              Iceland |              Portugal |                          Spain |                nonEU |                               EU |                            Total |
|--------------:|---------------------:|----------------------:|-------------------------------:|---------------------:|---------------------------------:|---------------------------------:|
|   Agriculture |                      |        A=75.9, B=24.5 |                 A=96.6, B=43.2 |                      |                  A=172.5, B=67.7 |                  A=172.5, B=67.7 |
| Entertainment |               B=16.8 |          B=7.1, D=2.3 |         A=77.4, B=11.5, C=16.4 |               B=16.8 |    A=77.4, B=18.6, C=16.4, D=2.3 |    A=77.4, B=35.4, C=16.4, D=2.3 |
|  Governmental |                      |         B=21.6, D=2.0 |                   C=6.5, D=2.7 |                      |             B=21.6, C=6.5, D=4.7 |             B=21.6, C=6.5, D=4.7 |
|      Industry |  B=9.6, C=8.8, D=1.9 |         B=25.7, D=3.4 |                          C=8.4 |  B=9.6, C=8.8, D=1.9 |             B=25.7, C=8.4, D=3.4 |            B=35.3, C=17.2, D=5.3 |
|       private | B=26.4, C=8.8, D=1.9 | A=75.9, B=57.3, D=5.7 |        A=174.0, B=54.7, C=24.8 | B=26.4, C=8.8, D=1.9 |  A=249.9, B=112.0, C=24.8, D=5.7 |  A=249.9, B=138.4, C=33.6, D=7.6 |
|        public |                      |         B=21.6, D=2.0 |                   C=6.5, D=2.7 |                      |             B=21.6, C=6.5, D=4.7 |             B=21.6, C=6.5, D=4.7 |
|         Total | B=26.4, C=8.8, D=1.9 | A=75.9, B=78.9, D=7.7 | A=174.0, B=54.7, C=31.3, D=2.7 | B=26.4, C=8.8, D=1.9 | A=249.9, B=133.6, C=31.3, D=10.4 | A=249.9, B=160.0, C=40.1, D=12.3 |

**Table 6**: The “value” data aggregated according to hierarchy and
contributor

  

Below we take into account contributor IDs when using few contributors
primary suppression.

``` r
output <- SuppressFewContributors(data=dataset, 
                                  numVar = "value", 
                                  dimVar = c("sector4", "sector2", "geo", "eu"),
                                  maxN=2,
                                  contributorVar = "company")
#> [preAggregate 20*6->20*7]
#> [extraAggregate 20*7->10*7] Checking .....
#> GaussSuppression_numttHTT: ............::::::
head(output)
#>   sector4      geo freq value nRule nAll primary suppressed
#> 1   Total    Total   20 462.3     4    4   FALSE      FALSE
#> 2   Total       EU   16 425.2     4    4   FALSE      FALSE
#> 3   Total    nonEU    4  37.1     3    3   FALSE      FALSE
#> 4   Total  Iceland    4  37.1     3    3   FALSE      FALSE
#> 5   Total Portugal    8 162.5     3    3   FALSE      FALSE
#> 6   Total    Spain    8 262.7     4    4   FALSE      FALSE
```

  

|       sector4 |   Iceland |   Portugal |      Spain |     nonEU |         EU |      Total |
|--------------:|----------:|-----------:|-----------:|----------:|-----------:|-----------:|
|   Agriculture |     0 (0) |  100.4 (2) |  139.8 (2) |     0 (0) |  240.2 (2) |  240.2 (2) |
| Entertainment |  16.8 (1) |    9.4 (2) |  105.3 (3) |  16.8 (1) |  114.7 (4) |  131.5 (4) |
|  Governmental |     0 (0) |   23.6 (2) |    9.2 (2) |     0 (0) |   32.8 (3) |   32.8 (3) |
|      Industry |  20.3 (3) |   29.1 (2) |    8.4 (1) |  20.3 (3) |   37.5 (3) |   57.8 (3) |
|       private |  37.1 (3) |  138.9 (3) |  253.5 (3) |  37.1 (3) |  392.4 (4) |  429.5 (4) |
|        public |     0 (0) |   23.6 (2) |    9.2 (2) |     0 (0) |   32.8 (3) |   32.8 (3) |
|         Total |  37.1 (3) |  162.5 (3) |  262.7 (4) |  37.1 (3) |  425.2 (4) |  462.3 (4) |

**Table 7**: Output from `SuppressFewContributors` with `maxN = 2` and
with `contributorVar = "company"` (number contributors in parenthesis)

  

Below we take into account contributor IDs when using dominant cell
primary suppression.

``` r
output <- SuppressDominantCells(data=dataset, 
                                numVar = "value", 
                                formula = ~sector2*geo + sector4*eu,
                                contributorVar = "company",
                                n = 1:2, k = c(80, 99))
#> [preAggregate 20*6->20*7]
#> [extraAggregate 20*7->10*7] Checking .....
#> GaussSuppression_numttHTT: .........:::::
head(output)
#>        geo sector4 freq value primary suppressed
#> 1    Total   Total   20 462.3   FALSE      FALSE
#> 2    Total private   16 429.5   FALSE      FALSE
#> 3    Total  public    4  32.8   FALSE      FALSE
#> 4  Iceland   Total    4  37.1   FALSE      FALSE
#> 5 Portugal   Total    8 162.5   FALSE      FALSE
#> 6    Spain   Total    8 262.7   FALSE      FALSE
```

Here we have also made use of the formula interface.

  

|       sector4 | Iceland | Portugal | Spain | nonEU |    EU | Total |
|--------------:|--------:|---------:|------:|------:|------:|------:|
|   Agriculture |         |          |       |       | 240.2 | 240.2 |
| Entertainment |         |          |       |  16.8 | 114.7 | 131.5 |
|  Governmental |         |          |       |       |  32.8 |  32.8 |
|      Industry |         |          |       |  20.3 |  37.5 |  57.8 |
|       private |    37.1 |    138.9 | 253.5 |       |       | 429.5 |
|        public |         |     23.6 |   9.2 |       |       |  32.8 |
|         Total |    37.1 |    162.5 | 262.7 |  37.1 | 425.2 | 462.3 |

**Table 8**: Output from `SuppressDominantCells` with `n = 1:2` and  
`k = c(80, 99)` and with `contributorVar = "company"`

  
  

## The singleton problem

Below, the data is suppressed in the same way as in Table 7, but with a
different formula.  

``` r
output <- SuppressDominantCells(data=dataset,
                                numVar = "value", 
                                formula = ~sector4*geo + sector2*eu,
                                contributorVar = "company",
                                n = 1:2, k = c(80, 99))
#> [preAggregate 20*6->20*7]
#> [extraAggregate 20*7->10*7] Checking .....
#> GaussSuppression_numttHTT: ............
head(output)
#>         sector4     geo freq value primary suppressed
#> 1         Total   Total   20 462.3   FALSE      FALSE
#> 2   Agriculture   Total    4 240.2    TRUE       TRUE
#> 3 Entertainment   Total    6 131.5   FALSE      FALSE
#> 4  Governmental   Total    4  32.8   FALSE      FALSE
#> 5      Industry   Total    6  57.8   FALSE       TRUE
#> 6         Total Iceland    4  37.1   FALSE      FALSE
```

  

|       sector4 | Iceland | Portugal | Spain | nonEU |    EU | Total |
|--------------:|--------:|---------:|------:|------:|------:|------:|
|   Agriculture |         |    100.4 | 139.8 |       |       | 240.2 |
| Entertainment |    16.8 |      9.4 | 105.3 |       |       | 131.5 |
|  Governmental |         |     23.6 |   9.2 |       |       |  32.8 |
|      Industry |    20.3 |     29.1 |   8.4 |       |       |  57.8 |
|       private |         |          |       |  37.1 | 392.4 | 429.5 |
|        public |         |          |       |       |  32.8 |  32.8 |
|         Total |    37.1 |    162.5 | 262.7 |  37.1 | 425.2 | 462.3 |

**Table 9**: Output from `SuppressDominantCells` with `n = 1:2` and
`k = c(80, 99)` and with `contributorVar = "company"`

  

By using `singletonMethod = "none"` in this case, *Entertainment:Spain*
will not be suppressed. This cell is suppressed due to the default
handling of the singleton problem. The reason is that
*Entertainment:Iceland* has a single contributor. This contributor can
reveal *Entertainment:Portugal* if *Entertainment:Spain* is not
suppressed.

Here it might appear that the table contains another issue,
*Entertainment:Iceland* can reveal *Industry:Iceland*. However, this can
be considered ok in this case. *Industry:Iceland* is secondary
suppressed and the only reason for this is to protect
*Entertainment:Iceland*.

Nevertheless, in most cases, secondary suppressed cells introduce
further complexity to the handling of singletons. A part of the
singleton handling of magnitude tables is to add virtual primary
suppressed cells prior to the secondary suppression algorithm. Secondary
suppressed cells cannot, therefore, be treated in this way. However,
another part of the singleton handling solves many of the remaining
problems. This is done within the suppression algorithm.

Here we can observe the effect of this in Tables 2 and 3. By using
`singletonMethod = "none"` in Table 2, *Industry:Portugal* will not be
suppressed. In that case, *Industry:Spain* can reveal *Industry:Iceland*
and consequently *Entertainment:Iceland*. In Table 3,
*Governmental:Total* and *Industry:Total* are suppressed due to advanced
singleton handling. In fact, by using `singletonMethod = "none"`, all
tables above will be suppressed differently.

## Intervals

Intervals for the primarily suppressed cells are computed whenever the
`lpPackage` parameter is specified. Additionally, if `rangePercent`
and/or `rangeMin` are provided, further suppression is performed to
ensure that the interval width requirements are met. See the
documentation for the `lpPackage` parameter in
[`?GaussSuppressionFromData`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/GaussSuppressionFromData.md)
for more details.

In the example below, the interval widths for output lines 12, 14, 17,
and 20 are 60.7, which is too narrow since a minimum width of 80 is
required. Therefore, the cell on line 16 is additionally suppressed

``` r
output <- SuppressDominantCells(data = dataset, 
                                numVar = "value", 
                                dimVar = c("geo", "sector2", "sector4"), 
                                pPercent = 30, 
                                rangePercent = 70, rangeMin = 80, 
                                lpPackage = "Rglpk")
#> [extraAggregate 20*4->10*6] Checking ....
#> GaussSuppression_numttHTT: ............
#> (10*8-DDcol->10*6-0exact->10*5-GaussI->10*5)
#> 
#> Using Rglpk for intervals...
#> -----------
#> (10*8-DDcol->10*6-DDrow->9*6->)
#> ......
#> 4+1-3+2-
#>   2: 1 new, (138.9) 1+
#> GaussSuppression_none: ......
#> (10*7-DDcol->10*5-0exact->10*4-GaussI->10*4)
#> 
#> Using Rglpk for intervals...
#> -----------
output[c(12, 14, 16, 17, 20, 25), ] # some output lines 
#>         geo       sector4 value rlim_value  lo_1  up_1   lo    up
#> 12  Iceland Entertainment  16.8      80.00   0.0  60.7  0.0 131.5
#> 14  Iceland      Industry  20.3      80.00   0.0  60.7  0.0  90.6
#> 16 Portugal       private 138.9         NA    NA    NA   NA    NA
#> 17 Portugal        public  23.6      80.00   0.0  60.7  0.0  90.6
#> 20 Portugal  Governmental  23.6      80.00   0.0  60.7  0.0  90.6
#> 25    Spain   Agriculture 139.8      97.86 101.3 240.2 40.6 240.2
#>    suppressed_integer primary suppressed
#> 12                  1    TRUE       TRUE
#> 14                  1    TRUE       TRUE
#> 16                  3   FALSE       TRUE
#> 17                  1    TRUE       TRUE
#> 20                  1    TRUE       TRUE
#> 25                  1    TRUE       TRUE
```

  

In the formatted output shown in Table 10 below, the final intervals are
given in parentheses.

  

|       sector4 |           Iceland |           Portugal |                 Spain |            Total |
|--------------:|------------------:|-------------------:|----------------------:|-----------------:|
|   Agriculture |                 0 | 100.4 \[0, 199.6\] | 139.8 \[40.6, 240.2\] |            240.2 |
| Entertainment | 16.8 \[0, 131.5\] |   9.4 \[0, 131.5\] |    105.3 \[0, 131.5\] |            131.5 |
|  Governmental |                 0 |   23.6 \[0, 90.6\] |       9.2 \[0, 90.6\] | 32.8 \[0, 90.6\] |
|      Industry |  20.3 \[0, 90.6\] |   29.1 \[0, 90.6\] |       8.4 \[0, 90.6\] |             57.8 |
|       private |              37.1 |              138.9 |                 253.5 |            429.5 |
|        public |                 0 |   23.6 \[0, 90.6\] |       9.2 \[0, 90.6\] | 32.8 \[0, 90.6\] |
|         Total |              37.1 |              162.5 |                 262.7 |            462.3 |

**Table 10**: Output from `SuppressDominantCells` with `pPercent = 30`,
`rangePercent = 70`, `rangeMin = 80`, `lpPackage = "Rglpk"`

  
