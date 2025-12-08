# Defining Tables for GaussSuppression

## Introduction and setup

The `GaussSuppression` package uses a common interface shared by other
SDC packages developed at Statistics Norway (see also
[`SmallCountRounding`](https://cran.r-project.org/package=SmallCountRounding)
and [`SSBcellKey`](https://github.com/statisticsnorway/ssb-ssbcellkey)).
In the background, these packages use a *model matrix* representation,
which connects the input data to the intended output. This functionality
is provided by the R package `SSBtools`. In this vignette, we look at
multiple ways of specifying output tables given different forms of
input. Note that this vignette only scratches the surface of what is
possible with the provided interface, and rather is intended to help
users get going with the package.

We begin by importing the necessary dependencies as well as loading a
test data set provided in the SSBtools package.

``` r
library(SSBtools)
library(GaussSuppression)

dataset <- SSBtools::SSBtoolsData("d2s")

microdata <- SSBtools::MakeMicro(dataset, "freq")

head(dataset)
#>   region   county  size main_income freq
#> 1      A county-1   BIG       other    2
#> 2      B county-2   BIG       other    3
#> 3      C county-2   BIG       other    5
#> 4      D county-1 small       other    3
#> 5      E county-3 small       other    9
#> 6      F county-3 small       other    4
nrow(dataset)
#> [1] 24
head(microdata)
#>   region   county size main_income freq
#> 1      A county-1  BIG       other    1
#> 2      A county-1  BIG       other    1
#> 3      B county-2  BIG       other    1
#> 4      B county-2  BIG       other    1
#> 5      B county-2  BIG       other    1
#> 6      C county-2  BIG       other    1
nrow(microdata)
#> [1] 338
```

The imported data set is a fictitious data set containing the variables:
region, county, size, main_income, freq, where region, county, and size
are different (non-nested) regional hierarchies. `GaussSuppression` can
take microdata as input as well, which we will demonstrate in the
following sections.

The table below illustrates this dataset reshaped to wide format with
several `freq` columns created from the `main_income` variable. However,
please note that data that is input and output in the `GaussSuppression`
package is always in long format.

  

[TABLE]

**Table 1**: `dataset` reshaped to wide format.

  

## Defining Table Dimensions

Output tables are mainly specified using the following three parameters:
`dimVar`, `hierarchies`, and `formula`.

### Creating tables using `dimVar`

The most basic way of defining output tables is by using the `dimVar`
parameter. This generates by default all combinations of the variables
provided, including marginals. For example, the following function call
creates a one dimensional frequency table over the variable region.

``` r
GaussSuppressionFromData(data = dataset,
                         dimVar = "region",
                         freqVar = "freq",
                         primary = FALSE,
                         protectZeros = FALSE)
#>   region freq primary suppressed
#> 1  Total  338   FALSE      FALSE
#> 2      A  104   FALSE      FALSE
#> 3      B   51   FALSE      FALSE
#> 4      C   73   FALSE      FALSE
#> 5      D   14   FALSE      FALSE
#> 6      E   61   FALSE      FALSE
#> 7      F   35   FALSE      FALSE
```

The same output is shown below as a formatted table.  
  

**Table 2**: `dimVar = "region"`

| region |     |
|-------:|----:|
|      A | 104 |
|      B |  51 |
|      C |  73 |
|      D |  14 |
|      E |  61 |
|      F |  35 |
|  Total | 338 |

  

Note the use of the function GaussSuppressionFromData and the inclusion
of two parameters `primary` and `protectZeros`. The functions in
`GaussSuppression` are designed to incorporate both table building and
protection into a single function call. Thus, to illustrate the table
building features, we have set that nothing must be protected. To learn
more about the different ways of protecting tables, see the other
vignettes of this package.

In a similar fashion, we can include multiple variables in the `dimVar`
parameter:

``` r
GaussSuppressionFromData(data = dataset,
                         dimVar = c("region", "main_income"),
                         freqVar = "freq",
                         primary = FALSE,
                         protectZeros = FALSE)
#>    region main_income freq primary suppressed
#> 1   Total       Total  338   FALSE      FALSE
#> 2   Total  assistance  178   FALSE      FALSE
#> 3   Total       other   26   FALSE      FALSE
#> 4   Total    pensions  112   FALSE      FALSE
#> 5   Total       wages   22   FALSE      FALSE
#> 6       A       Total  104   FALSE      FALSE
#> 7       A  assistance   55   FALSE      FALSE
#> 8       A       other    2   FALSE      FALSE
#> 9       A    pensions   36   FALSE      FALSE
#> 10      A       wages   11   FALSE      FALSE
#> 11      B       Total   51   FALSE      FALSE
#> 12      B  assistance   29   FALSE      FALSE
#> 13      B       other    3   FALSE      FALSE
#> 14      B    pensions   18   FALSE      FALSE
#> 15      B       wages    1   FALSE      FALSE
#> 16      C       Total   73   FALSE      FALSE
#> 17      C  assistance   35   FALSE      FALSE
#> 18      C       other    5   FALSE      FALSE
#> 19      C    pensions   25   FALSE      FALSE
#> 20      C       wages    8   FALSE      FALSE
#> 21      D       Total   14   FALSE      FALSE
#> 22      D  assistance    9   FALSE      FALSE
#> 23      D       other    3   FALSE      FALSE
#> 24      D    pensions    2   FALSE      FALSE
#> 25      D       wages    0   FALSE      FALSE
#> 26      E       Total   61   FALSE      FALSE
#> 27      E  assistance   32   FALSE      FALSE
#> 28      E       other    9   FALSE      FALSE
#> 29      E    pensions   20   FALSE      FALSE
#> 30      E       wages    0   FALSE      FALSE
#> 31      F       Total   35   FALSE      FALSE
#> 32      F  assistance   18   FALSE      FALSE
#> 33      F       other    4   FALSE      FALSE
#> 34      F    pensions   11   FALSE      FALSE
#> 35      F       wages    2   FALSE      FALSE
```

The same output is shown below as a formatted and reshaped table. Cells
that also occur as input/inner cells have white background.  
  

| region | other | wages | assistance | pensions | Total |
|-------:|------:|------:|-----------:|---------:|------:|
|      A |     2 |    11 |         55 |       36 |   104 |
|      B |     3 |     1 |         29 |       18 |    51 |
|      C |     5 |     8 |         35 |       25 |    73 |
|      D |     3 |     0 |          9 |        2 |    14 |
|      E |     9 |     0 |         32 |       20 |    61 |
|      F |     4 |     2 |         18 |       11 |    35 |
|  Total |    26 |    22 |        178 |      112 |   338 |

**Table 3**: `dimVar = c("region", "main_income")`

  

Note in particular what happens when we provide two regional variables:

``` r
GaussSuppressionFromData(data = dataset,
                         dimVar = c("region", "county"),
                         freqVar = "freq",
                         primary = FALSE,
                         protectZeros = FALSE)
#>      region freq primary suppressed
#> 1     Total  338   FALSE      FALSE
#> 2  county-1  118   FALSE      FALSE
#> 3  county-2  124   FALSE      FALSE
#> 4  county-3   96   FALSE      FALSE
#> 5         A  104   FALSE      FALSE
#> 6         B   51   FALSE      FALSE
#> 7         C   73   FALSE      FALSE
#> 8         D   14   FALSE      FALSE
#> 9         E   61   FALSE      FALSE
#> 10        F   35   FALSE      FALSE
```

  

**Table 4**: `dimVar = c("region", "county")`

|   region |     |
|---------:|----:|
|        A | 104 |
|        B |  51 |
|        C |  73 |
|        D |  14 |
|        E |  61 |
|        F |  35 |
| county-1 | 118 |
| county-2 | 124 |
| county-3 |  96 |
|    Total | 338 |

  

The function detects hierarchies encoded in `dimVar` columns, and
collapses them into a single column (with the name of the most detailed
variable). In this way, it is not necessary to specify hierarchies by
hand and include them explicitly in the function call. This also works
for non-nested hierarchies:

``` r
GaussSuppressionFromData(data = dataset,
                         dimVar = c("region", "county", "size"),
                         freqVar = "freq",
                         primary = FALSE,
                         protectZeros = FALSE)
#>      region freq primary suppressed
#> 1       BIG  228   FALSE      FALSE
#> 2  county-1  118   FALSE      FALSE
#> 3  county-2  124   FALSE      FALSE
#> 4  county-3   96   FALSE      FALSE
#> 5     small  110   FALSE      FALSE
#> 6     Total  338   FALSE      FALSE
#> 7         A  104   FALSE      FALSE
#> 8         B   51   FALSE      FALSE
#> 9         C   73   FALSE      FALSE
#> 10        D   14   FALSE      FALSE
#> 11        E   61   FALSE      FALSE
#> 12        F   35   FALSE      FALSE
```

  

**Table 5**: `dimVar = c("region", "county", "size")`

|   region |     |
|---------:|----:|
|        A | 104 |
|        B |  51 |
|        C |  73 |
|        D |  14 |
|        E |  61 |
|        F |  35 |
| county-1 | 118 |
| county-2 | 124 |
| county-3 |  96 |
|    small | 110 |
|      BIG | 228 |
|    Total | 338 |

We can combine all the dimensional variables in our example data:

``` r
output <- GaussSuppressionFromData(data = dataset,
                                   dimVar = c("region", "county", "size", "main_income"),
                                   freqVar = "freq",
                                   primary = FALSE,
                                   protectZeros = FALSE)
head(output)
#>     region main_income freq primary suppressed
#> 1      BIG       Total  228   FALSE      FALSE
#> 2      BIG  assistance  119   FALSE      FALSE
#> 3      BIG       other   10   FALSE      FALSE
#> 4      BIG    pensions   79   FALSE      FALSE
#> 5      BIG       wages   20   FALSE      FALSE
#> 6 county-1       Total  118   FALSE      FALSE
```

  

**Table 6**: `dimVar = c("region", "county", "size", "main_income")`

|   region | other | wages | assistance | pensions | Total |
|---------:|------:|------:|-----------:|---------:|------:|
|        A |     2 |    11 |         55 |       36 |   104 |
|        B |     3 |     1 |         29 |       18 |    51 |
|        C |     5 |     8 |         35 |       25 |    73 |
|        D |     3 |     0 |          9 |        2 |    14 |
|        E |     9 |     0 |         32 |       20 |    61 |
|        F |     4 |     2 |         18 |       11 |    35 |
| county-1 |     5 |    11 |         64 |       38 |   118 |
| county-2 |     8 |     9 |         64 |       43 |   124 |
| county-3 |    13 |     2 |         50 |       31 |    96 |
|    small |    16 |     2 |         59 |       33 |   110 |
|      BIG |    10 |    20 |        119 |       79 |   228 |
|    Total |    26 |    22 |        178 |      112 |   338 |

  
In the background, functions from SSBtools are used to find the
hierarchies. There are multiple ways of inspecting which hierarchies can
be found; users familiar with DimLists used in other SDC packages can
for example use the following:

``` r
FindDimLists(dataset[c("region", "county")])
#> $region
#>    levels    codes
#> 1       @    Total
#> 2      @@ county-1
#> 3     @@@        A
#> 4     @@@        D
#> 5      @@ county-2
#> 6     @@@        B
#> 7     @@@        C
#> 8      @@ county-3
#> 9     @@@        E
#> 10    @@@        F
FindDimLists(dataset[c("region", "county", "size")])
#> $region
#>    levels    codes
#> 1       @    Total
#> 2      @@ county-1
#> 3     @@@        A
#> 4     @@@        D
#> 5      @@ county-2
#> 6     @@@        B
#> 7     @@@        C
#> 8      @@ county-3
#> 9     @@@        E
#> 10    @@@        F
#> 
#> $region
#>   levels codes
#> 1      @ Total
#> 2     @@   BIG
#> 3    @@@     A
#> 4    @@@     B
#> 5    @@@     C
#> 6     @@ small
#> 7    @@@     D
#> 8    @@@     E
#> 9    @@@     F
```

Note the last example which contained non-nested hierarchies. Here, a
unique DimList is created for each tree-shaped hierarchy in the data
set. This avoids the need for specifying non-nested hierarchies as
linked tables.

Finally, for illustration purposes, we see that the same function calls
work with microdata as input:

``` r
GaussSuppressionFromData(data = microdata,
                         dimVar = c("region", "county", "size"),
                         freqVar = "freq",
                         primary = FALSE,
                         protectZeros = FALSE)
#>      region freq primary suppressed
#> 1       BIG  228   FALSE      FALSE
#> 2  county-1  118   FALSE      FALSE
#> 3  county-2  124   FALSE      FALSE
#> 4  county-3   96   FALSE      FALSE
#> 5     small  110   FALSE      FALSE
#> 6     Total  338   FALSE      FALSE
#> 7         A  104   FALSE      FALSE
#> 8         B   51   FALSE      FALSE
#> 9         C   73   FALSE      FALSE
#> 10        D   14   FALSE      FALSE
#> 11        E   61   FALSE      FALSE
#> 12        F   35   FALSE      FALSE
```

This output is the same as illustrated in Table 5 above.

### Creating tables using `hierarchies`

The `hierarchies` parameter allows the explicit specification of which
hierarchies should be used when creating the output table. This allows
for a more fine-grained approach as opposed to simply using `dimVar`, as
it allows for applying hierarchies not already present in the data set.
Hierarchies can be provided in many ways. In this vignette, we will
exemplify the following three forms: as a dimlist (as defined in
`sdcTable`), using the hrc format from TauArgus, and finally with a more
general hierarchy specification (internally, not surprisingly, simply
called hierarchy). Any of these can be provided to the `hierarchies`
parameter, as they are all translated to the internal hierarchy
representation. For the purposes of this vignette, we will use dimlists,
however in the following example we shall see how these can be
translated to one another using functions from `SSBtools`. Let us begin
by defining two hierarchies by using dimlists:

``` r
region_dim <- data.frame(levels = c("@", "@@", rep("@@@", 2), rep("@@", 4)),
                         codes = c("Total", "AB", LETTERS[1:6]))
region_dim
#>   levels codes
#> 1      @ Total
#> 2     @@    AB
#> 3    @@@     A
#> 4    @@@     B
#> 5     @@     C
#> 6     @@     D
#> 7     @@     E
#> 8     @@     F

income_dim <- data.frame(levels = c("@", "@@", "@@", "@@@", "@@@", "@@@"),
                         codes = c("Total", "wages", "not_wages", "other", "assistance", "pensions"))
income_dim
#>   levels      codes
#> 1      @      Total
#> 2     @@      wages
#> 3     @@  not_wages
#> 4    @@@      other
#> 5    @@@ assistance
#> 6    @@@   pensions
SSBtools::DimList2Hrc(income_dim)
#> [1] "wages"       "not_wages"   "@other"      "@assistance" "@pensions"
SSBtools::DimList2Hierarchy(income_dim)
#>     mapsFrom    mapsTo sign level
#> 1      wages     Total    1     2
#> 2  not_wages     Total    1     2
#> 3      other not_wages    1     1
#> 4 assistance not_wages    1     1
#> 5   pensions not_wages    1     1
```

We can use these hierarchies to specify our output table. We do this by
supplying a named list to the `hierarchies` parameter, where the list
names correspond to variables in the data, and the list elements
correspond to hierarchies we wish to include.

``` r
GaussSuppressionFromData(data = dataset,
                         hierarchies = list(region = region_dim, main_income = income_dim),
                         freqVar = "freq",
                         primary = FALSE,
                         protectZeros = FALSE)
#>    region main_income freq primary suppressed
#> 1   Total       Total  338   FALSE      FALSE
#> 2   Total   not_wages  316   FALSE      FALSE
#> 3   Total  assistance  178   FALSE      FALSE
#> 4   Total       other   26   FALSE      FALSE
#> 5   Total    pensions  112   FALSE      FALSE
#> 6   Total       wages   22   FALSE      FALSE
#> 7      AB       Total  155   FALSE      FALSE
#> 8      AB   not_wages  143   FALSE      FALSE
#> 9      AB  assistance   84   FALSE      FALSE
#> 10     AB       other    5   FALSE      FALSE
#> 11     AB    pensions   54   FALSE      FALSE
#> 12     AB       wages   12   FALSE      FALSE
#> 13      A       Total  104   FALSE      FALSE
#> 14      A   not_wages   93   FALSE      FALSE
#> 15      A  assistance   55   FALSE      FALSE
#> 16      A       other    2   FALSE      FALSE
#> 17      A    pensions   36   FALSE      FALSE
#> 18      A       wages   11   FALSE      FALSE
#> 19      B       Total   51   FALSE      FALSE
#> 20      B   not_wages   50   FALSE      FALSE
#> 21      B  assistance   29   FALSE      FALSE
#> 22      B       other    3   FALSE      FALSE
#> 23      B    pensions   18   FALSE      FALSE
#> 24      B       wages    1   FALSE      FALSE
#> 25      C       Total   73   FALSE      FALSE
#> 26      C   not_wages   65   FALSE      FALSE
#> 27      C  assistance   35   FALSE      FALSE
#> 28      C       other    5   FALSE      FALSE
#> 29      C    pensions   25   FALSE      FALSE
#> 30      C       wages    8   FALSE      FALSE
#> 31      D       Total   14   FALSE      FALSE
#> 32      D   not_wages   14   FALSE      FALSE
#> 33      D  assistance    9   FALSE      FALSE
#> 34      D       other    3   FALSE      FALSE
#> 35      D    pensions    2   FALSE      FALSE
#> 36      D       wages    0   FALSE      FALSE
#> 37      E       Total   61   FALSE      FALSE
#> 38      E   not_wages   61   FALSE      FALSE
#> 39      E  assistance   32   FALSE      FALSE
#> 40      E       other    9   FALSE      FALSE
#> 41      E    pensions   20   FALSE      FALSE
#> 42      E       wages    0   FALSE      FALSE
#> 43      F       Total   35   FALSE      FALSE
#> 44      F   not_wages   33   FALSE      FALSE
#> 45      F  assistance   18   FALSE      FALSE
#> 46      F       other    4   FALSE      FALSE
#> 47      F    pensions   11   FALSE      FALSE
#> 48      F       wages    2   FALSE      FALSE
```

  

**Table 7**:
`hierarchies = list(region = region_dim, main_income = income_dim)`

| region | other | wages | assistance | pensions | not_wages | Total |
|-------:|------:|------:|-----------:|---------:|----------:|------:|
|      A |     2 |    11 |         55 |       36 |        93 |   104 |
|      B |     3 |     1 |         29 |       18 |        50 |    51 |
|      C |     5 |     8 |         35 |       25 |        65 |    73 |
|      D |     3 |     0 |          9 |        2 |        14 |    14 |
|      E |     9 |     0 |         32 |       20 |        61 |    61 |
|      F |     4 |     2 |         18 |       11 |        33 |    35 |
|     AB |     5 |    12 |         84 |       54 |       143 |   155 |
|  Total |    26 |    22 |        178 |      112 |       316 |   338 |

  
As mentioned previously, the `GaussSuppression` package supports
non-nested hierarchies natively. We achieve this by having multiple
elements with the same name in the `hierarchies` list:

``` r
region2_dim <- data.frame(levels = c("@", rep(c("@@", rep("@@@", 2)), 2), rep("@@", 2)),
                          codes = c("Total", "AD", "A", "D",  
                                    "BF", "B", "F", 
                                    "C", "E"))
region2_dim
#>   levels codes
#> 1      @ Total
#> 2     @@    AD
#> 3    @@@     A
#> 4    @@@     D
#> 5     @@    BF
#> 6    @@@     B
#> 7    @@@     F
#> 8     @@     C
#> 9     @@     E

GaussSuppressionFromData(data = dataset,
                         hierarchies = list(region = region_dim, region = region2_dim),
                         freqVar = "freq",
                         primary = FALSE,
                         protectZeros = FALSE)
#>    region freq primary suppressed
#> 1      AB  155   FALSE      FALSE
#> 2      AD  118   FALSE      FALSE
#> 3      BF   86   FALSE      FALSE
#> 4   Total  338   FALSE      FALSE
#> 5       A  104   FALSE      FALSE
#> 6       B   51   FALSE      FALSE
#> 7       C   73   FALSE      FALSE
#> 8       D   14   FALSE      FALSE
#> 9       E   61   FALSE      FALSE
#> 10      F   35   FALSE      FALSE
```

  

**Table 8**:
`hierarchies = list(region = region_dim, region = region2_dim)`

| region |     |
|-------:|----:|
|      A | 104 |
|      B |  51 |
|      C |  73 |
|      D |  14 |
|      E |  61 |
|      F |  35 |
|     AB | 155 |
|     AD | 118 |
|     BF |  86 |
|  Total | 338 |

Finally, as before, all of this functionality works with microdata as
input as well.

``` r
GaussSuppressionFromData(data = microdata,
                         hierarchies = list(region = region_dim, region = region2_dim),
                         freqVar = "freq",
                         primary = FALSE,
                         protectZeros = FALSE)
#>    region freq primary suppressed
#> 1      AB  155   FALSE      FALSE
#> 2      AD  118   FALSE      FALSE
#> 3      BF   86   FALSE      FALSE
#> 4   Total  338   FALSE      FALSE
#> 5       A  104   FALSE      FALSE
#> 6       B   51   FALSE      FALSE
#> 7       C   73   FALSE      FALSE
#> 8       D   14   FALSE      FALSE
#> 9       E   61   FALSE      FALSE
#> 10      F   35   FALSE      FALSE
```

### Creating tables using `formula`

The most flexible method for specifying the output of GaussSuppression
is by using the `formula` interface. This makes use of model formulas in
R, and provides a powerful way of specifying multiple different tables.
Indeed, all of the above examples—and much more—can be replicated using
the formula interface. The formula’s predictor variables must be
variable names occuring in the data set (the dependent variable is
ignored, and thus we leave it empty). In the following, we create a
table based on the region and county variables. As before, the
hierarchical relationship between these variables is detected
automatically:

``` r
GaussSuppressionFromData(data = microdata,
                         formula = ~ region + county,
                         freqVar = "freq",
                         primary = FALSE,
                         protectZeros = FALSE)
#>      region freq primary suppressed
#> 1     Total  338   FALSE      FALSE
#> 2         A  104   FALSE      FALSE
#> 3         B   51   FALSE      FALSE
#> 4         C   73   FALSE      FALSE
#> 5         D   14   FALSE      FALSE
#> 6         E   61   FALSE      FALSE
#> 7         F   35   FALSE      FALSE
#> 8  county-1  118   FALSE      FALSE
#> 9  county-2  124   FALSE      FALSE
#> 10 county-3   96   FALSE      FALSE
```

  

**Table 9**: `formula = ~ region + county`

|   region |     |
|---------:|----:|
|        A | 104 |
|        B |  51 |
|        C |  73 |
|        D |  14 |
|        E |  61 |
|        F |  35 |
| county-1 | 118 |
| county-2 | 124 |
| county-3 |  96 |
|    Total | 338 |

  

If there is no hierarchical relationship between variables,
multiplication in the `formula` and specification in `dimVar` yield the
same results.

``` r

GaussSuppressionFromData(data = microdata,
                         formula = ~ county * main_income,
                         freqVar = "freq",
                         primary = FALSE,
                         protectZeros = FALSE)
#>      county main_income freq primary suppressed
#> 1     Total       Total  338   FALSE      FALSE
#> 2  county-1       Total  118   FALSE      FALSE
#> 3  county-2       Total  124   FALSE      FALSE
#> 4  county-3       Total   96   FALSE      FALSE
#> 5     Total  assistance  178   FALSE      FALSE
#> 6     Total       other   26   FALSE      FALSE
#> 7     Total    pensions  112   FALSE      FALSE
#> 8     Total       wages   22   FALSE      FALSE
#> 9  county-1  assistance   64   FALSE      FALSE
#> 10 county-1       other    5   FALSE      FALSE
#> 11 county-1    pensions   38   FALSE      FALSE
#> 12 county-1       wages   11   FALSE      FALSE
#> 13 county-2  assistance   64   FALSE      FALSE
#> 14 county-2       other    8   FALSE      FALSE
#> 15 county-2    pensions   43   FALSE      FALSE
#> 16 county-2       wages    9   FALSE      FALSE
#> 17 county-3  assistance   50   FALSE      FALSE
#> 18 county-3       other   13   FALSE      FALSE
#> 19 county-3    pensions   31   FALSE      FALSE
#> 20 county-3       wages    2   FALSE      FALSE
  GaussSuppressionFromData(data = microdata,
                         dimVar = c("county" , "main_income"),
                         freqVar = "freq",
                         primary = FALSE,
                         protectZeros = FALSE)
#>      county main_income freq primary suppressed
#> 1     Total       Total  338   FALSE      FALSE
#> 2     Total  assistance  178   FALSE      FALSE
#> 3     Total       other   26   FALSE      FALSE
#> 4     Total    pensions  112   FALSE      FALSE
#> 5     Total       wages   22   FALSE      FALSE
#> 6  county-1       Total  118   FALSE      FALSE
#> 7  county-1  assistance   64   FALSE      FALSE
#> 8  county-1       other    5   FALSE      FALSE
#> 9  county-1    pensions   38   FALSE      FALSE
#> 10 county-1       wages   11   FALSE      FALSE
#> 11 county-2       Total  124   FALSE      FALSE
#> 12 county-2  assistance   64   FALSE      FALSE
#> 13 county-2       other    8   FALSE      FALSE
#> 14 county-2    pensions   43   FALSE      FALSE
#> 15 county-2       wages    9   FALSE      FALSE
#> 16 county-3       Total   96   FALSE      FALSE
#> 17 county-3  assistance   50   FALSE      FALSE
#> 18 county-3       other   13   FALSE      FALSE
#> 19 county-3    pensions   31   FALSE      FALSE
#> 20 county-3       wages    2   FALSE      FALSE
```

  

**Table 10**: `formula = ~ county * main_income` or
`dimVar = c("county" , "main_income")`

|   county | other | wages | assistance | pensions | Total |
|---------:|------:|------:|-----------:|---------:|------:|
| county-1 |     5 |    11 |         64 |       38 |   118 |
| county-2 |     8 |     9 |         64 |       43 |   124 |
| county-3 |    13 |     2 |         50 |       31 |    96 |
|    Total |    26 |    22 |        178 |      112 |   338 |

  
However, `formula` lets us specify different shapes for our tables. For
example, if we are only interested in marginal values, we can supply
this with the use of the addition operator:

``` r

GaussSuppressionFromData(data = microdata,
                         formula = ~ county + main_income,
                         freqVar = "freq",
                         primary = FALSE,
                         protectZeros = FALSE)
#>     county main_income freq primary suppressed
#> 1    Total       Total  338   FALSE      FALSE
#> 2 county-1       Total  118   FALSE      FALSE
#> 3 county-2       Total  124   FALSE      FALSE
#> 4 county-3       Total   96   FALSE      FALSE
#> 5    Total  assistance  178   FALSE      FALSE
#> 6    Total       other   26   FALSE      FALSE
#> 7    Total    pensions  112   FALSE      FALSE
#> 8    Total       wages   22   FALSE      FALSE
```

The same output is shown below as a formatted and reshaped table where
empty cells means cells not included in the output.  

**Table 11**: `formula = ~ county + main_income`

|   county | other | wages | assistance | pensions | Total |
|---------:|------:|------:|-----------:|---------:|------:|
| county-1 |       |       |            |          |   118 |
| county-2 |       |       |            |          |   124 |
| county-3 |       |       |            |          |    96 |
|    Total |    26 |    22 |        178 |      112 |   338 |

  
This example demonstrates, in fact, the ability of specifying multiple
linked tables: a one-dimensional table for county linked with a
one-dimensional table for main_income. Similarly, we can use the colon
(“:”) operator to omit row and column marginals:

``` r
GaussSuppressionFromData(data = microdata,
                         formula = ~ county:main_income,
                         freqVar = "freq",
                         primary = FALSE,
                         protectZeros = FALSE)
#>      county main_income freq primary suppressed
#> 1     Total       Total  338   FALSE      FALSE
#> 2  county-1  assistance   64   FALSE      FALSE
#> 3  county-1       other    5   FALSE      FALSE
#> 4  county-1    pensions   38   FALSE      FALSE
#> 5  county-1       wages   11   FALSE      FALSE
#> 6  county-2  assistance   64   FALSE      FALSE
#> 7  county-2       other    8   FALSE      FALSE
#> 8  county-2    pensions   43   FALSE      FALSE
#> 9  county-2       wages    9   FALSE      FALSE
#> 10 county-3  assistance   50   FALSE      FALSE
#> 11 county-3       other   13   FALSE      FALSE
#> 12 county-3    pensions   31   FALSE      FALSE
#> 13 county-3       wages    2   FALSE      FALSE
```

  

**Table 12**: `formula = ~ county:main_income`

|   county | other | wages | assistance | pensions | Total |
|---------:|------:|------:|-----------:|---------:|------:|
| county-1 |     5 |    11 |         64 |       38 |       |
| county-2 |     8 |     9 |         64 |       43 |       |
| county-3 |    13 |     2 |         50 |       31 |       |
|    Total |       |       |            |          |   338 |

  
Using subtraction, we can omit marginals and other cells from the
output. For example, the intercept (sum over all records) can be omitted
by including `- 1` in the formula, like this:
`formula = county : main_income - 1`.

Using these features, we can define more complicated linked tables. To
illustrate this, let us assume we wish to publish the following:

- all levels of detail of main_income for all counties and sizes,
- at the region level, we only wish to publish if the main source of
  income was “wages” or “not_wages”.

To do this, we begin by adding a column encoding whether the main source
of income was “wages” or “not_wages”.

``` r
dataset$income2 <- ifelse(dataset$main_income == "wages", "wages", "not_wages")
microdata$income2 <- ifelse(microdata$main_income == "wages", "wages", "not_wages")
head(dataset)
#>   region   county  size main_income freq   income2
#> 1      A county-1   BIG       other    2 not_wages
#> 2      B county-2   BIG       other    3 not_wages
#> 3      C county-2   BIG       other    5 not_wages
#> 4      D county-1 small       other    3 not_wages
#> 5      E county-3 small       other    9 not_wages
#> 6      F county-3 small       other    4 not_wages
```

Then we can specify the desired output with the following formula:

``` r
GaussSuppressionFromData(data = dataset,
                         formula = ~ region * income2 + (county + size) * main_income,
                         freqVar = "freq",
                         primary = FALSE,
                         protectZeros = FALSE)
#>      region main_income freq primary suppressed
#> 1     Total       Total  338   FALSE      FALSE
#> 2         A       Total  104   FALSE      FALSE
#> 3         B       Total   51   FALSE      FALSE
#> 4         C       Total   73   FALSE      FALSE
#> 5         D       Total   14   FALSE      FALSE
#> 6         E       Total   61   FALSE      FALSE
#> 7         F       Total   35   FALSE      FALSE
#> 8     Total   not_wages  316   FALSE      FALSE
#> 9     Total       wages   22   FALSE      FALSE
#> 10 county-1       Total  118   FALSE      FALSE
#> 11 county-2       Total  124   FALSE      FALSE
#> 12 county-3       Total   96   FALSE      FALSE
#> 13      BIG       Total  228   FALSE      FALSE
#> 14    small       Total  110   FALSE      FALSE
#> 15    Total  assistance  178   FALSE      FALSE
#> 16    Total       other   26   FALSE      FALSE
#> 17    Total    pensions  112   FALSE      FALSE
#> 18    Total       wages   22   FALSE      FALSE
#> 19        A   not_wages   93   FALSE      FALSE
#> 20        A       wages   11   FALSE      FALSE
#> 21        B   not_wages   50   FALSE      FALSE
#> 22        B       wages    1   FALSE      FALSE
#> 23        C   not_wages   65   FALSE      FALSE
#> 24        C       wages    8   FALSE      FALSE
#> 25        D   not_wages   14   FALSE      FALSE
#> 26        D       wages    0   FALSE      FALSE
#> 27        E   not_wages   61   FALSE      FALSE
#> 28        E       wages    0   FALSE      FALSE
#> 29        F   not_wages   33   FALSE      FALSE
#> 30        F       wages    2   FALSE      FALSE
#> 31 county-1  assistance   64   FALSE      FALSE
#> 32 county-1       other    5   FALSE      FALSE
#> 33 county-1    pensions   38   FALSE      FALSE
#> 34 county-1       wages   11   FALSE      FALSE
#> 35 county-2  assistance   64   FALSE      FALSE
#> 36 county-2       other    8   FALSE      FALSE
#> 37 county-2    pensions   43   FALSE      FALSE
#> 38 county-2       wages    9   FALSE      FALSE
#> 39 county-3  assistance   50   FALSE      FALSE
#> 40 county-3       other   13   FALSE      FALSE
#> 41 county-3    pensions   31   FALSE      FALSE
#> 42 county-3       wages    2   FALSE      FALSE
#> 43      BIG  assistance  119   FALSE      FALSE
#> 44      BIG       other   10   FALSE      FALSE
#> 45      BIG    pensions   79   FALSE      FALSE
#> 46      BIG       wages   20   FALSE      FALSE
#> 47    small  assistance   59   FALSE      FALSE
#> 48    small       other   16   FALSE      FALSE
#> 49    small    pensions   33   FALSE      FALSE
#> 50    small       wages    2   FALSE      FALSE
```

  

**Table 13**:
`formula = ~ region * income2 + (county + size) * main_income`

|   region | other | wages | assistance | pensions | not_wages | Total |
|---------:|------:|------:|-----------:|---------:|----------:|------:|
|        A |       |    11 |            |          |        93 |   104 |
|        B |       |     1 |            |          |        50 |    51 |
|        C |       |     8 |            |          |        65 |    73 |
|        D |       |     0 |            |          |        14 |    14 |
|        E |       |     0 |            |          |        61 |    61 |
|        F |       |     2 |            |          |        33 |    35 |
| county-1 |     5 |    11 |         64 |       38 |           |   118 |
| county-2 |     8 |     9 |         64 |       43 |           |   124 |
| county-3 |    13 |     2 |         50 |       31 |           |    96 |
|    small |    16 |     2 |         59 |       33 |           |   110 |
|      BIG |    10 |    20 |        119 |       79 |           |   228 |
|    Total |    26 |    22 |        178 |      112 |       316 |   338 |

In this manner, we can specify multiple linked tables, each of which can
use different non-nested hierarchies. This allows the suppression
algorithm to protect all of these tables simultaneously (indeed, they
are treated as a single table internally), avoiding the need for a
stratified protection paradigm. Furthermore, the fine-grained
specification of which cells are to be published allows the secondary
suppression algorithm to protect with respect to precisely those cells
that will be published. If row and column marginals are not published,
for example, the suppression algorithm does not need to secondary
suppress with respect to these marginals. See the other vignettes in
this package for more details on setting up the protection methods.

Looking at the output data above Table 13, you will see that row 9 is
duplicated on row 18. The reason is that the code `wages` is used both
in the `main_income` variable and in the `income2` variable. Currently,
the formula interface does not do any special checking for this
phenomenon. The recommended practice is to avoid such duplicate codes.
When running `FindDimLists`, you will see that this function performs
checking.

## Tabulating continuous variables

In addition to defining the dimensions of the output tables, we need to
decide whether they should be frequency tables (where we count
contributing records) or magnititude tables (where we add contributing
records’ numerical values for a given variable). All of the above
examples have been frequency tables. However, the process is exactly the
same if one wishes to construct magnititude tables; the only difference
is that one must specify the numerical variable with the help of the
parameter `numVar`.

Since most magnitude table suppression methods are based on comparing
units’ contributions, the input data will most likely be supplied as
microdata. Therefore, let us add a fake numerical variable to our
microdata:

``` r
set.seed(12345)
microdata$num <- sample(0:1000, nrow(microdata), replace = TRUE) 
```

Then in order to construct a magnitude table where records’
contributions to `num` are aggregated, we supply this as a parameter to
`GaussSuppressionFromData`:

``` r
GaussSuppressionFromData(data = microdata,
                         formula = ~ region * income2 + (county + size) * main_income,
                         numVar = "num",
                         primary = FALSE,
                         protectZeros = FALSE)
#> [preAggregate 338*7->22*7]
#>      region main_income freq.1    num primary suppressed
#> 1     Total       Total    338 168843   FALSE      FALSE
#> 2         A       Total    104  50640   FALSE      FALSE
#> 3         B       Total     51  27386   FALSE      FALSE
#> 4         C       Total     73  35826   FALSE      FALSE
#> 5         D       Total     14   5730   FALSE      FALSE
#> 6         E       Total     61  30295   FALSE      FALSE
#> 7         F       Total     35  18966   FALSE      FALSE
#> 8     Total   not_wages    316 157199   FALSE      FALSE
#> 9     Total       wages     22  11644   FALSE      FALSE
#> 10 county-1       Total    118  56370   FALSE      FALSE
#> 11 county-2       Total    124  63212   FALSE      FALSE
#> 12 county-3       Total     96  49261   FALSE      FALSE
#> 13      BIG       Total    228 113852   FALSE      FALSE
#> 14    small       Total    110  54991   FALSE      FALSE
#> 15    Total  assistance    178  91989   FALSE      FALSE
#> 16    Total       other     26  13000   FALSE      FALSE
#> 17    Total    pensions    112  52210   FALSE      FALSE
#> 18    Total       wages     22  11644   FALSE      FALSE
#> 19        A   not_wages     93  45268   FALSE      FALSE
#> 20        A       wages     11   5372   FALSE      FALSE
#> 21        B   not_wages     50  26500   FALSE      FALSE
#> 22        B       wages      1    886   FALSE      FALSE
#> 23        C   not_wages     65  32111   FALSE      FALSE
#> 24        C       wages      8   3715   FALSE      FALSE
#> 25        D   not_wages     14   5730   FALSE      FALSE
#> 26        E   not_wages     61  30295   FALSE      FALSE
#> 27        F   not_wages     33  17295   FALSE      FALSE
#> 28        F       wages      2   1671   FALSE      FALSE
#> 29 county-1  assistance     64  33024   FALSE      FALSE
#> 30 county-1       other      5   1260   FALSE      FALSE
#> 31 county-1    pensions     38  16714   FALSE      FALSE
#> 32 county-1       wages     11   5372   FALSE      FALSE
#> 33 county-2  assistance     64  33424   FALSE      FALSE
#> 34 county-2       other      8   4696   FALSE      FALSE
#> 35 county-2    pensions     43  20491   FALSE      FALSE
#> 36 county-2       wages      9   4601   FALSE      FALSE
#> 37 county-3  assistance     50  25541   FALSE      FALSE
#> 38 county-3       other     13   7044   FALSE      FALSE
#> 39 county-3    pensions     31  15005   FALSE      FALSE
#> 40 county-3       wages      2   1671   FALSE      FALSE
#> 41      BIG  assistance    119  62407   FALSE      FALSE
#> 42      BIG       other     10   4887   FALSE      FALSE
#> 43      BIG    pensions     79  36585   FALSE      FALSE
#> 44      BIG       wages     20   9973   FALSE      FALSE
#> 45    small  assistance     59  29582   FALSE      FALSE
#> 46    small       other     16   8113   FALSE      FALSE
#> 47    small    pensions     33  15625   FALSE      FALSE
#> 48    small       wages      2   1671   FALSE      FALSE
```

  

|   region |        other |        wages |   assistance |     pensions |     not_wages |         Total |
|---------:|-------------:|-------------:|-------------:|-------------:|--------------:|--------------:|
|        A |              |   5372 ( 11) |              |              |   45268 ( 93) |   50640 (104) |
|        B |              |     886 ( 1) |              |              |   26500 ( 50) |   27386 ( 51) |
|        C |              |    3715 ( 8) |              |              |   32111 ( 65) |   35826 ( 73) |
|        D |              |              |              |              |    5730 ( 14) |    5730 ( 14) |
|        E |              |              |              |              |   30295 ( 61) |   30295 ( 61) |
|        F |              |    1671 ( 2) |              |              |   17295 ( 33) |   18966 ( 35) |
| county-1 |    1260 ( 5) |   5372 ( 11) |  33024 ( 64) |  16714 ( 38) |               |   56370 (118) |
| county-2 |    4696 ( 8) |    4601 ( 9) |  33424 ( 64) |  20491 ( 43) |               |   63212 (124) |
| county-3 |   7044 ( 13) |    1671 ( 2) |  25541 ( 50) |  15005 ( 31) |               |   49261 ( 96) |
|    small |   8113 ( 16) |    1671 ( 2) |  29582 ( 59) |  15625 ( 33) |               |   54991 (110) |
|      BIG |   4887 ( 10) |   9973 ( 20) |  62407 (119) |  36585 ( 79) |               |  113852 (228) |
|    Total |  13000 ( 26) |  11644 ( 22) |  91989 (178) |  52210 (112) |  157199 (316) |  168843 (338) |

**Table 14**:
`formula = ~ region * income2 + (county + size) * main_income`  
In each cell: `num` with frequencies in parenthesis.

  

Note that there are two empty cells in the wages column. This means that
these cells are not included in the output data. One reason is that the
`removeEmpty` parameter to
[`SSBtools::ModelMatrix`](https://statisticsnorway.github.io/ssb-ssbtools/reference/ModelMatrix.html)
has `TRUE` as default in the case of a formula interface. By including
`removeEmpty = FALSE`, zeros will be included in the output. Another way
to achieve this is to use `extend0 = TRUE`. By this parameter, zeros are
added to the input data after the automatic aggregation from microdata.
As you will see in other vignettes in this package, the `extend0`
parameter can be important for suppression methods.

Note also that a new frequency variable is generated with the above
call. If a frequency variable is already present in the input data, we
can provide it in addition to `numVar` and the method will use that
information instead:

``` r
GaussSuppressionFromData(data = microdata,
                         formula = ~ region * income2 + (county + size) * main_income,
                         freqVar = "freq",
                         numVar = "num",
                         primary = FALSE,
                         protectZeros = FALSE)
#>      region main_income freq    num primary suppressed
#> 1     Total       Total  338 168843   FALSE      FALSE
#> 2         A       Total  104  50640   FALSE      FALSE
#> 3         B       Total   51  27386   FALSE      FALSE
#> 4         C       Total   73  35826   FALSE      FALSE
#> 5         D       Total   14   5730   FALSE      FALSE
#> 6         E       Total   61  30295   FALSE      FALSE
#> 7         F       Total   35  18966   FALSE      FALSE
#> 8     Total   not_wages  316 157199   FALSE      FALSE
#> 9     Total       wages   22  11644   FALSE      FALSE
#> 10 county-1       Total  118  56370   FALSE      FALSE
#> 11 county-2       Total  124  63212   FALSE      FALSE
#> 12 county-3       Total   96  49261   FALSE      FALSE
#> 13      BIG       Total  228 113852   FALSE      FALSE
#> 14    small       Total  110  54991   FALSE      FALSE
#> 15    Total  assistance  178  91989   FALSE      FALSE
#> 16    Total       other   26  13000   FALSE      FALSE
#> 17    Total    pensions  112  52210   FALSE      FALSE
#> 18    Total       wages   22  11644   FALSE      FALSE
#> 19        A   not_wages   93  45268   FALSE      FALSE
#> 20        A       wages   11   5372   FALSE      FALSE
#> 21        B   not_wages   50  26500   FALSE      FALSE
#> 22        B       wages    1    886   FALSE      FALSE
#> 23        C   not_wages   65  32111   FALSE      FALSE
#> 24        C       wages    8   3715   FALSE      FALSE
#> 25        D   not_wages   14   5730   FALSE      FALSE
#> 26        E   not_wages   61  30295   FALSE      FALSE
#> 27        F   not_wages   33  17295   FALSE      FALSE
#> 28        F       wages    2   1671   FALSE      FALSE
#> 29 county-1  assistance   64  33024   FALSE      FALSE
#> 30 county-1       other    5   1260   FALSE      FALSE
#> 31 county-1    pensions   38  16714   FALSE      FALSE
#> 32 county-1       wages   11   5372   FALSE      FALSE
#> 33 county-2  assistance   64  33424   FALSE      FALSE
#> 34 county-2       other    8   4696   FALSE      FALSE
#> 35 county-2    pensions   43  20491   FALSE      FALSE
#> 36 county-2       wages    9   4601   FALSE      FALSE
#> 37 county-3  assistance   50  25541   FALSE      FALSE
#> 38 county-3       other   13   7044   FALSE      FALSE
#> 39 county-3    pensions   31  15005   FALSE      FALSE
#> 40 county-3       wages    2   1671   FALSE      FALSE
#> 41      BIG  assistance  119  62407   FALSE      FALSE
#> 42      BIG       other   10   4887   FALSE      FALSE
#> 43      BIG    pensions   79  36585   FALSE      FALSE
#> 44      BIG       wages   20   9973   FALSE      FALSE
#> 45    small  assistance   59  29582   FALSE      FALSE
#> 46    small       other   16   8113   FALSE      FALSE
#> 47    small    pensions   33  15625   FALSE      FALSE
#> 48    small       wages    2   1671   FALSE      FALSE
```
