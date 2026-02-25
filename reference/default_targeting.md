# Default `targeting` function for SuppressKDisclosure()

Generates a `targeting` specification for use with
[`SuppressKDisclosure()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/SuppressKDisclosure.md).
The function is actually used internally by
[`KDisclosurePrimary()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/KDisclosurePrimary.md).

## Usage

``` r
default_targeting(
  crossTable,
  x,
  identifying = NULL,
  sensitive = NULL,
  targeting_include = NULL,
  targeting_exclude = NULL,
  ...
)
```

## Arguments

- crossTable:

  A `crossTable`, possibly extended after applying `mc_hierarchies`.

- x:

  The model matrix, `x`, possibly extended after applying
  `mc_hierarchies`.

- identifying:

  Specification of information that an intruder may already know. The
  specification is subject to the same requirements as `sensitive`
  below. If not all variables are included, total codes for the missing
  variables are derived automatically. This requires that the overall
  total is included as an output row.

- sensitive:

  Specification of information considered unacceptable to disclose. It
  may be given as a character vector of variable names, a named list
  with variable names as names and specified codes as values, or a data
  frame specifying variable combinations. The wildcard characters `*`
  and `?`, as well as the exclusion operator `!`, may be used, since
  [`SSBtools::WildcardGlobbing()`](https://statisticsnorway.github.io/ssb-ssbtools/reference/WildcardGlobbing.html)
  is applied.

- targeting_include:

  A list of two-element lists with components `identifying` and
  `sensitive`. Each element defines identifying–sensitive relations
  using the same specification rules as the parameters `identifying` and
  `sensitive`. All specifications together, including the main
  `identifying` and `sensitive` parameters, define the relations that
  are examined for suppression.

- targeting_exclude:

  A list specified in the same way as `targeting_include`. The relations
  defined here are ignored when examining suppression.

- ...:

  Unused parameters.

## Value

A named `targeting` list. See
[`SuppressKDisclosure()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/SuppressKDisclosure.md).

## Details

The parameters `identifying` and `sensitive` are used to select table
cells (including hidden cells constructed via `mc_hierarchies`). All
such cells are represented by rows in `crossTable`, which may be
extended due to `mc_hierarchies`. Thus, rows in `crossTable` are
selected as identifying or sensitive.

In addition, `sensitive` specifies which codes within the selected rows
are regarded as sensitive.

The logic differs slightly for unspecified variables: For `identifying`,
unspecified variables are set to total codes. For `sensitive`, all rows
in `crossTable` matching the specified variables are selected.

The parameters `identifying` and `sensitive` are used to construct the
`targeting` specification for
[`KDisclosurePrimary()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/KDisclosurePrimary.md),
resulting in the elements `identifying`, `sensitive`, and
`is_sensitive`.

When `targeting_include` and/or `targeting_exclude` are specified,
additional elements `include_relations` and `exclude_relations` are
created.

## Examples

``` r
mm <- SSBtools::ModelMatrix(SSBtoolsData("example1"), 
     formula = ~age * eu + geo, crossTable = TRUE)
crossTable <- mm$crossTable
x <- mm$modelMatrix      

default_targeting(crossTable, x)  # just NULL 
#> NULL

# geo identifying and age sensitive (age sensitive variable)
a2 <- default_targeting(crossTable, x, 
                        identifying = "geo", 
                        sensitive = "age")
a1 <- default_targeting(crossTable, x, 
                        identifying = list(age = "Total", geo = "*"), 
                        sensitive = list(age = "*")) 
identical(a1, a2)
#> [1] TRUE
a1                         
#> $identifying
#>     age      geo
#> 1 Total    Total
#> 2 Total       EU
#> 3 Total    nonEU
#> 4 Total  Iceland
#> 5 Total Portugal
#> 6 Total    Spain
#> 
#> $sensitive
#>     age   geo
#> 1   old Total
#> 2 young Total
#> 3   old    EU
#> 4   old nonEU
#> 5 young    EU
#> 6 young nonEU
#> 
#> $is_sensitive
#>    age   geo
#> 1 TRUE FALSE
#> 2 TRUE FALSE
#> 3 TRUE FALSE
#> 4 TRUE FALSE
#> 5 TRUE FALSE
#> 6 TRUE FALSE
#> 
                  
                  
# Not ok to disclose 'EU' and 'Portugal'
# But ok to disclose 'Spain' with 'EU' known
# and also ok to disclose 'Spain' in other table cells without 'EU' as marginal  
default_targeting(crossTable, x, 
                  sensitive = list(geo = c("Portugal", "EU")))
#> $sensitive
#>     age      geo
#> 1 Total       EU
#> 2 Total Portugal
#> 3   old       EU
#> 4 young       EU
#> 
#> $is_sensitive
#>     age  geo
#> 1 FALSE TRUE
#> 2 FALSE TRUE
#> 3 FALSE TRUE
#> 4 FALSE TRUE
#> 
                  
# As above but now also ok to disclose 'Portugal' from 'EU' known,
# since protection only considers 'age' identifying.                   
default_targeting(crossTable, x, 
                  identifying = "age",
                  sensitive = list(geo = c("Portugal", "EU")))                 
#> $identifying
#>     age   geo
#> 1 Total Total
#> 2   old Total
#> 3 young Total
#> 
#> $sensitive
#>     age      geo
#> 1 Total       EU
#> 2 Total Portugal
#> 3   old       EU
#> 4 young       EU
#> 
#> $is_sensitive
#>     age  geo
#> 1 FALSE TRUE
#> 2 FALSE TRUE
#> 3 FALSE TRUE
#> 4 FALSE TRUE
#> 
```
