# Get default value of a function

The value may be found in a spec. See
[`PackageSpecs`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/PackageSpecs.md).

## Usage

``` r
GetDefault(fun, parameter, ifnotfound = NULL)
```

## Arguments

- fun:

  A function

- parameter:

  parameter name

- ifnotfound:

## Value

The default parameter, possibly evaluated.

## Details

The result is evaluated if [`is.name`](https://rdrr.io/r/base/name.html)
returns `TRUE`.

## Examples

``` r
fun1 <- GetDefault(GaussSuppressionFromData, "candidates")
fun2 <- GetDefault(SuppressFewContributors, "primary")
fun3 <- GetDefault(SuppressDominantCells, "primary") 
```
