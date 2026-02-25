# K-disclosure suppression

Frequency table suppression for targeted attribute disclosure
protection.

## Usage

``` r
SuppressKDisclosure(
  data,
  coalition = 0,
  mc_hierarchies = NULL,
  upper_bound = Inf,
  dimVar = NULL,
  formula = NULL,
  hierarchies = NULL,
  freqVar = NULL,
  targeting = default_targeting,
  identifying = NULL,
  sensitive = NULL,
  print_frames = FALSE,
  ...,
  spec = PackageSpecs("kDisclosureSpec")
)
```

## Arguments

- data:

  a data.frame representing the data set

- coalition:

  numeric vector of length one, representing possible size of an
  attacking coalition. This parameter corresponds to the parameter k in
  the definition of k-disclosure.

- mc_hierarchies:

  a hierarchy representing meaningful combinations to be protected.
  Default value is `NULL`.

- upper_bound:

  Numeric value specifying the maximum cell frequency for which
  disclosure of belonging to the cell may be regarded as unacceptable.
  When freq \> upper_bound, disclosure of belonging to the cell is
  regarded as acceptable regardless of the specification of the
  `sensitive` parameter. Default is Inf. Note that this parameter may
  also be useful for reducing computational burden.

- dimVar:

  The main dimensional variables and additional aggregating variables.
  This parameter can be useful when hierarchies and formula are
  unspecified.

- formula:

  A model formula

- hierarchies:

  List of hierarchies, which can be converted by
  [`AutoHierarchies`](https://statisticsnorway.github.io/ssb-ssbtools/reference/AutoHierarchies.html).
  Thus, the variables can also be coded by `"rowFactor"` or `""`, which
  correspond to using the categories in the data.

- freqVar:

  name of the frequency variable in `data`

- targeting:

  The mechanism underlying the interpretation of `identifying` and
  `sensitive`. See Details in
  [`KDisclosurePrimary()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/KDisclosurePrimary.md).

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

- print_frames:

  Logical or character. If TRUE, additional data frames are printed to
  the console. When `mc_hierarchies` is used, this includes a data frame
  with hidden results. In addition, a data frame containing the primary
  suppressed difference cells is printed. If set to `"primary_cells"`,
  only the primary suppressed difference cells are printed. The default
  is FALSE.

- ...:

  parameters passed to children functions

- spec:

  `NULL` or a named list of arguments that will act as default values.

## Value

A data.frame containing the publishable data set, with a boolean
variable `$suppressed` representing cell suppressions.

## Details

The argument `targeting` may also be a function that returns such a
list. This works similarly to supplied functions in
[`GaussSuppressionFromData()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/GaussSuppressionFromData.md).
Note, however, that the function operates on possibly extended versions
of `freq`, `x`, and `crossTable` that reflect the use of
`mc_hierarchies`, when applicable.

The parameters `identifying` and `sensitive` are included here as
explicit arguments, but they are in fact parameters of
[`default_targeting()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/default_targeting.md).
In addition, the
[`default_targeting()`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/default_targeting.md)
parameters `targeting_include` and `targeting_exclude` may also be used
(see examples).

## Author

Daniel P. Lupp and Øyvind Langsrud

## Examples

``` r
# data
mun_a <- SSBtools::SSBtoolsData("mun_accidents")

# Function to print output in wide format, marking suppressed values with `*`
show_out <- function(out) {  
  out$freq = sprintf("%s%s", out$freq, c(" ","*")[1+out$suppressed])
  a <- reshape(out[1:3], idvar = "mun", timevar = "inj", direction = "wide", )
  names(a) <- sub("^freq\\.", "", names(a))
  print(a)}

# hierarchies as DimLists
mun <- data.frame(levels = c("@", rep("@@", 6)),
codes = c("Total", paste("k", 1:6, sep = "")))
inj <- data.frame(levels = c("@", "@@" ,"@@", "@@", "@@"),
codes = c("Total", "serious", "light", "none", "unknown"))
dimlists <- list(mun = mun, inj = inj)

inj2 <- data.frame(levels = c("@", "@@", "@@@" ,"@@@", "@@", "@@"),
codes = c("Total", "injured", "serious", "light", "none", "unknown"))
inj3 <- data.frame(levels = c("@", "@@", "@@" ,"@@", "@@"),
codes = c( "shadowtotal", "serious", "light", "none", "unknown"))
mc_dimlist <- list(inj = inj2)
mc_nomargs <- list(inj = inj3)

#' # Example with formula, no meaningful combination
out <- SuppressKDisclosure(mun_a, coalition = 1, freqVar = "freq", 
                           formula = ~mun*inj, print_frames = TRUE)
#> [extend0 24*3->24*3]
#> 
#> ---- primary suppressed difference cells ---
#>        mun           inj diff
#> 1       k1 Total-serious    0
#> 2       k2 Total-serious    1
#> 3       k5 Total-serious    0
#> 4       k6 Total-serious    0
#> 5 Total-k3         light    1
#> 
#> GaussSuppression_anySum0: ...................................
show_out(out)
#>     mun Total light none serious unknown
#> 1 Total   32     3    6      23       0 
#> 2    k1    4     0    0*      4*      0 
#> 3    k2    6     0    1*      5*      0 
#> 4    k3    6     2*   1*      3       0 
#> 5    k4    9     1    4       4       0 
#> 6    k5    1     0    0*      1*      0 
#> 7    k6    6     0*   0       6*      0 

# Example with hierarchy and meaningful combination
out2 <- SuppressKDisclosure(mun_a, coalition = 1, freqVar = "freq",
                       hierarchies = dimlists, mc_hierarchies = mc_dimlist,
                       print_frames = TRUE)
#> [extend0 24*3->24*3]
#> 
#> ----- hidden cells from mc_hierarchies -----
#>     mun     inj freq
#> 1 Total injured   26
#> 2    k1 injured    4
#> 3    k2 injured    5
#> 4    k3 injured    5
#> 5    k4 injured    5
#> 6    k5 injured    1
#> 7    k6 injured    6
#> 
#> ---- primary suppressed difference cells ---
#>         mun             inj diff
#> 1  Total-k3           light    1
#> 2        k1   Total-serious    0
#> 3        k1   Total-injured    0
#> 4        k2   Total-serious    1
#> 5        k2   Total-injured    1
#> 6        k3   Total-injured    1
#> 7        k5   Total-serious    0
#> 8        k5   Total-injured    0
#> 9        k6   Total-serious    0
#> 10       k6   Total-injured    0
#> 11       k1 injured-serious    0
#> 12       k2 injured-serious    0
#> 13       k4 injured-serious    1
#> 14       k5 injured-serious    0
#> 15       k6 injured-serious    0
#> 
#> GaussSuppression_anySum0: ..............................
show_out(out2)
#>      mun Total light none serious unknown
#> 1  Total   32     3*   6      23       0*
#> 6     k1    4     0*   0       4*      0*
#> 11    k2    6     0*   1*      5*      0 
#> 16    k3    6     2*   1*      3       0 
#> 21    k4    9     1*   4       4       0*
#> 26    k5    1     0*   0       1*      0*
#> 31    k6    6     0*   0       6*      0*

#' # Example of table without mariginals, and mc_hierarchies to protect
out3 <- SuppressKDisclosure(mun_a, coalition = 1, freqVar = "freq",
                       formula = ~mun:inj, mc_hierarchies = mc_nomargs,
                       print_frames = TRUE)
#> [extend0 24*3->24*3]
#> 
#> ----- hidden cells from mc_hierarchies -----
#>   mun         inj freq
#> 1  k1 shadowtotal    4
#> 2  k2 shadowtotal    6
#> 3  k3 shadowtotal    6
#> 4  k4 shadowtotal    9
#> 5  k5 shadowtotal    1
#> 6  k6 shadowtotal    6
#> 
#> ---- primary suppressed difference cells ---
#>   mun                 inj diff
#> 1  k1 shadowtotal-serious    0
#> 2  k2 shadowtotal-serious    1
#> 3  k5 shadowtotal-serious    0
#> 4  k6 shadowtotal-serious    0
#> 
#> GaussSuppression_anySum0: .........................
show_out(out3)
#>      mun Total light none serious unknown
#> 1  Total   32   <NA> <NA>    <NA>    <NA>
#> 2     k1  <NA>    0    0       4       0*
#> 6     k2  <NA>    0    1*      5       0 
#> 10    k3  <NA>    2    1       3       0 
#> 14    k4  <NA>    1    4       4       0 
#> 18    k5  <NA>    0    0       1       0*
#> 22    k6  <NA>    0    0       6       0*


### Examples with identifying and sensitive ###
                
out_d <- SuppressKDisclosure(mun_a, coalition = 1, freqVar = "freq",
                             formula = ~mun*inj, sensitive= "inj",
                             print_frames = TRUE)
#> [extend0 24*3->24*3]
#> 
#> ---- primary suppressed difference cells ---
#>   mun           inj diff
#> 1  k1 Total-serious    0
#> 2  k2 Total-serious    1
#> 3  k5 Total-serious    0
#> 4  k6 Total-serious    0
#> 
#> GaussSuppression_anySum0: ...................................
show_out(out_d)                                                    
#>     mun Total light none serious unknown
#> 1 Total   32     3    6      23       0 
#> 2    k1    4     0    0*      4*      0 
#> 3    k2    6     0    1*      5*      0 
#> 4    k3    6     2    1       3       0 
#> 5    k4    9     1    4       4       0 
#> 6    k5    1     0    0*      1*      0 
#> 7    k6    6     0    0*      6*      0 
                

out_d1 <- SuppressKDisclosure(mun_a, coalition = 1, freqVar = "freq",
                              formula = ~mun*inj, mc_hierarchies = mc_dimlist,
                              sensitive = list(mun =  "k3", inj = "injured"),
                              print_frames = TRUE)
#> [extend0 24*3->24*3]
#> 
#> ----- hidden cells from mc_hierarchies -----
#>     mun     inj freq
#> 1 Total injured   26
#> 2    k1 injured    4
#> 3    k2 injured    5
#> 4    k3 injured    5
#> 5    k4 injured    5
#> 6    k5 injured    1
#> 7    k6 injured    6
#> 
#> ---- primary suppressed difference cells ---
#>        mun           inj diff
#> 1       k1 Total-injured    0
#> 2       k2 Total-injured    1
#> 3       k3 Total-injured    1
#> 4       k5 Total-injured    0
#> 5       k6 Total-injured    0
#> 6 Total-k3         light    1
#> 
#> GaussSuppression_anySum0: ...................................
show_out(out_d1)                             
#>     mun Total light none serious unknown
#> 1 Total   32     3    6      23       0 
#> 2    k1    4     0    0*      4*      0 
#> 3    k2    6     0*   1*      5       0 
#> 4    k3    6     2*   1*      3*      0 
#> 5    k4    9     1    4       4       0 
#> 6    k5    1     0    0*      1*      0 
#> 7    k6    6     0    0*      6*      0 

out_d2 <- SuppressKDisclosure(mun_a, coalition = 1, freqVar = "freq",
                              formula = ~mun*inj, 
                              sensitive = list(inj = "serious", mun = "k3"),
                              print_frames = TRUE)
#> [extend0 24*3->24*3]
#> 
#> ---- primary suppressed difference cells ---
#>        mun           inj diff
#> 1       k1 Total-serious    0
#> 2       k2 Total-serious    1
#> 3       k5 Total-serious    0
#> 4       k6 Total-serious    0
#> 5 Total-k3         light    1
#> 
#> GaussSuppression_anySum0: ...................................
show_out(out_d2)                         
#>     mun Total light none serious unknown
#> 1 Total   32     3    6      23       0 
#> 2    k1    4     0    0*      4*      0 
#> 3    k2    6     0    1*      5*      0 
#> 4    k3    6     2*   1*      3       0 
#> 5    k4    9     1    4       4       0 
#> 6    k5    1     0    0*      1*      0 
#> 7    k6    6     0*   0       6*      0 

out_i1 <- SuppressKDisclosure(mun_a, coalition = 1, freqVar = "freq",
                              formula = ~mun*inj, identifying = "mun",
                              print_frames = TRUE)
#> [extend0 24*3->24*3]
#> 
#> ---- primary suppressed difference cells ---
#>   mun           inj diff
#> 1  k1 Total-serious    0
#> 2  k2 Total-serious    1
#> 3  k5 Total-serious    0
#> 4  k6 Total-serious    0
#> 
#> GaussSuppression_anySum0: ...................................
show_out(out_i1)                            
#>     mun Total light none serious unknown
#> 1 Total   32     3    6      23       0 
#> 2    k1    4     0    0*      4*      0 
#> 3    k2    6     0    1*      5*      0 
#> 4    k3    6     2    1       3       0 
#> 5    k4    9     1    4       4       0 
#> 6    k5    1     0    0*      1*      0 
#> 7    k6    6     0    0*      6*      0 
 
out_i2 <- SuppressKDisclosure(mun_a, coalition = 1, freqVar = "freq",
                              formula = ~mun*inj, identifying = "inj",
                              print_frames = TRUE)
#> [extend0 24*3->24*3]
#> 
#> ---- primary suppressed difference cells ---
#>        mun   inj diff
#> 1 Total-k3 light    1
#> 
#> GaussSuppression_anySum0: ...................................
show_out(out_i2)
#>     mun Total light none serious unknown
#> 1 Total   32     3    6      23       0 
#> 2    k1    4     0    0       4       0 
#> 3    k2    6     0*   1*      5       0 
#> 4    k3    6     2*   1*      3       0 
#> 5    k4    9     1    4       4       0 
#> 6    k5    1     0    0       1       0 
#> 7    k6    6     0    0       6       0 


mun_b <- SSBtools::SSBtoolsData("mun_accidents")
mun_b$freq <- c(0,5,3,4,1,0,
                0,0,2,0,0,6,
                4,1,0,4,0,0,
                0,0,0,0,0,0)

# With cells forced to be published, yielding unsafe table
out_unsafe <- SuppressKDisclosure(mun_b, coalition = 1, freqVar = "freq",
                                 formula = ~mun*inj, sensitive = "inj", 
                                 forced = c(12,14,15), output = "all",
                                 print_frames = TRUE)
#> [extend0 24*3->24*3]
#> 
#> ---- primary suppressed difference cells ---
#>   mun           inj diff
#> 1  k1    Total-none    0
#> 2  k2 Total-serious    1
#> 3  k5 Total-serious    0
#> 4  k6   Total-light    0
#> 
#> GaussSuppression_anySum0
#> Warning: Singleton marking of forced cells ignored (freq)
#> : ....
#> Warning: 1 (0 ordinary, 1 extra, 0 singleton) unsafe primary cells due to forced cells
#> ...............................
show_out(out_unsafe$publish)
#>     mun Total light none serious unknown
#> 1 Total   30     8    9      13       0 
#> 2    k1    4     0    4       0       0 
#> 3    k2    6     0    1*      5*      0 
#> 4    k3    5     2    0       3       0 
#> 5    k4    8     0    4       4       0 
#> 6    k5    1     0*   0       1*      0 
#> 7    k6    6     6*   0*      0*      0 

# colnames in $unsafe give an indication as to which cells/differences are unsafe
colnames(out_unsafe$unsafe)
#> [1] "k1:Total-none"
                               
                               
                               
 ### Advanced examples using `targeting_exclude` and `targeting_include`                             
                               
# Create a wrapper function to avoid repeating common arguments                                
fun <- function(..., coalition = 7) {
   SuppressKDisclosure(SSBtoolsData("d3"), 
       formula = ~(region + county)*main_income + region*months + county*main_income*months, 
       freqVar = "freq", coalition = coalition , print_frames = "primary_cells", 
       mc_hierarchies = list(main_income = c("special = assistance + other", 
                                             "ordinary = pensions + wages")),
       ...)}
       
# Without any sensitive or identifying specifications       
a1 <- fun()
#> [extend0 432*5->432*5]
#> 
#> ---- primary suppressed difference cells ---
#>    region        main_income       months diff
#> 1       S   Total-assistance        Total    1
#> 2       S     Total-pensions        Total    2
#> 3       S              Total Total-m06m09    2
#> 4       S              Total Total-m10m12    1
#> 5       S     Total-ordinary        Total    2
#> 6       S      Total-special        Total    1
#> 7       U        Total-other        Total    2
#> 8       U     Total-pensions        Total    1
#> 9       U              Total Total-m01m05    0
#> 10      U     Total-ordinary        Total    1
#> 11      U      Total-special        Total    2
#> 12      b              Total Total-m01m05    7
#> 13      g   Total-assistance        Total    5
#> 14      g              Total Total-m01m05    3
#> 15      g     Total-ordinary        Total    7
#> 16      g      Total-special        Total    3
#> 17      h              Total Total-m01m05    6
#> 18      h      Total-special        Total    7
#> 19      i   Total-assistance        Total    4
#> 20      i        Total-other        Total    6
#> 21      i     Total-pensions        Total    5
#> 22      i        Total-wages        Total    6
#> 23      i              Total Total-m01m05    2
#> 24      i              Total Total-m06m09    6
#> 25      i              Total Total-m10m12    6
#> 26      i     Total-ordinary        Total    4
#> 27      i      Total-special        Total    3
#> 28      j   Total-assistance        Total    7
#> 29      j              Total Total-m01m05    7
#> 30      j      Total-special        Total    5
#> 31      p              Total Total-m01m05    6
#> 32      t              Total Total-m01m05    7
#> 33      u   Total-assistance        Total    0
#> 34      u              Total Total-m01m05    1
#> 35      u              Total Total-m06m09    3
#> 36      u      Total-special        Total    0
#> 37      L  ordinary-pensions        Total    5
#> 38      N  ordinary-pensions        Total    7
#> 39      Q  ordinary-pensions        Total    5
#> 40      S  ordinary-pensions        Total    0
#> 41      T  ordinary-pensions        Total    3
#> 42      T     ordinary-wages        Total    5
#> 43      U  ordinary-pensions        Total    0
#> 44      V  ordinary-pensions        Total    4
#> 45      X  ordinary-pensions        Total    7
#> 46      Y  ordinary-pensions        Total    7
#> 47      Z  ordinary-pensions        Total    2
#> 48      a  ordinary-pensions        Total    2
#> 49      b  ordinary-pensions        Total    2
#> 50      g  ordinary-pensions        Total    1
#> 51      g     ordinary-wages        Total    2
#> 52      h  ordinary-pensions        Total    1
#> 53      h     ordinary-wages        Total    6
#> 54      i  ordinary-pensions        Total    1
#> 55      i     ordinary-wages        Total    2
#> 56      j  ordinary-pensions        Total    2
#> 57      j     ordinary-wages        Total    3
#> 58      l  ordinary-pensions        Total    6
#> 59      m  ordinary-pensions        Total    6
#> 60      q  ordinary-pensions        Total    3
#> 61      t  ordinary-pensions        Total    7
#> 62     14 special-assistance       m10m12    7
#> 63      N special-assistance        Total    7
#> 64      O special-assistance        Total    3
#> 65      Q special-assistance        Total    6
#> 66      S special-assistance        Total    0
#> 67      T special-assistance        Total    3
#> 68      T      special-other        Total    7
#> 69      U      special-other        Total    0
#> 70      X special-assistance        Total    6
#> 71      Z special-assistance        Total    5
#> 72      a special-assistance        Total    1
#> 73      b special-assistance        Total    6
#> 74      f special-assistance        Total    4
#> 75      g special-assistance        Total    2
#> 76      g      special-other        Total    5
#> 77      h special-assistance        Total    2
#> 78      i special-assistance        Total    1
#> 79      i      special-other        Total    3
#> 80      j special-assistance        Total    2
#> 81      l special-assistance        Total    4
#> 82      m special-assistance        Total    4
#> 83      n special-assistance        Total    4
#> 84      p special-assistance        Total    3
#> 85      q special-assistance        Total    3
#> 86      t special-assistance        Total    2
#> 87      u special-assistance        Total    0
#> 
#> GaussSuppression_anySum0: ..........................

# Treat the `main_income` variable as sensitive
a2 <- fun(sensitive = "main_income")
#> [extend0 432*5->432*5]
#> 
#> ---- primary suppressed difference cells ---
#>    region        main_income months diff
#> 1       S   Total-assistance  Total    1
#> 2       S     Total-pensions  Total    2
#> 3       S     Total-ordinary  Total    2
#> 4       S      Total-special  Total    1
#> 5       U        Total-other  Total    2
#> 6       U     Total-pensions  Total    1
#> 7       U     Total-ordinary  Total    1
#> 8       U      Total-special  Total    2
#> 9       g   Total-assistance  Total    5
#> 10      g     Total-ordinary  Total    7
#> 11      g      Total-special  Total    3
#> 12      h      Total-special  Total    7
#> 13      i   Total-assistance  Total    4
#> 14      i        Total-other  Total    6
#> 15      i     Total-pensions  Total    5
#> 16      i        Total-wages  Total    6
#> 17      i     Total-ordinary  Total    4
#> 18      i      Total-special  Total    3
#> 19      j   Total-assistance  Total    7
#> 20      j      Total-special  Total    5
#> 21      u   Total-assistance  Total    0
#> 22      u      Total-special  Total    0
#> 23      L  ordinary-pensions  Total    5
#> 24      N  ordinary-pensions  Total    7
#> 25      Q  ordinary-pensions  Total    5
#> 26      S  ordinary-pensions  Total    0
#> 27      T  ordinary-pensions  Total    3
#> 28      T     ordinary-wages  Total    5
#> 29      U  ordinary-pensions  Total    0
#> 30      V  ordinary-pensions  Total    4
#> 31      X  ordinary-pensions  Total    7
#> 32      Y  ordinary-pensions  Total    7
#> 33      Z  ordinary-pensions  Total    2
#> 34      a  ordinary-pensions  Total    2
#> 35      b  ordinary-pensions  Total    2
#> 36      g  ordinary-pensions  Total    1
#> 37      g     ordinary-wages  Total    2
#> 38      h  ordinary-pensions  Total    1
#> 39      h     ordinary-wages  Total    6
#> 40      i  ordinary-pensions  Total    1
#> 41      i     ordinary-wages  Total    2
#> 42      j  ordinary-pensions  Total    2
#> 43      j     ordinary-wages  Total    3
#> 44      l  ordinary-pensions  Total    6
#> 45      m  ordinary-pensions  Total    6
#> 46      q  ordinary-pensions  Total    3
#> 47      t  ordinary-pensions  Total    7
#> 48     14 special-assistance m10m12    7
#> 49      N special-assistance  Total    7
#> 50      O special-assistance  Total    3
#> 51      Q special-assistance  Total    6
#> 52      S special-assistance  Total    0
#> 53      T special-assistance  Total    3
#> 54      T      special-other  Total    7
#> 55      U      special-other  Total    0
#> 56      X special-assistance  Total    6
#> 57      Z special-assistance  Total    5
#> 58      a special-assistance  Total    1
#> 59      b special-assistance  Total    6
#> 60      f special-assistance  Total    4
#> 61      g special-assistance  Total    2
#> 62      g      special-other  Total    5
#> 63      h special-assistance  Total    2
#> 64      i special-assistance  Total    1
#> 65      i      special-other  Total    3
#> 66      j special-assistance  Total    2
#> 67      l special-assistance  Total    4
#> 68      m special-assistance  Total    4
#> 69      n special-assistance  Total    4
#> 70      p special-assistance  Total    3
#> 71      q special-assistance  Total    3
#> 72      t special-assistance  Total    2
#> 73      u special-assistance  Total    0
#> 
#> GaussSuppression_anySum0: ..........................

# In addition, treat `region` as identifying
a3 <- fun(sensitive = "main_income", identifying = "region")
#> [extend0 432*5->432*5]
#> 
#> ---- primary suppressed difference cells ---
#>    region      main_income months diff
#> 1       S Total-assistance  Total    1
#> 2       S   Total-pensions  Total    2
#> 3       S   Total-ordinary  Total    2
#> 4       S    Total-special  Total    1
#> 5       U      Total-other  Total    2
#> 6       U   Total-pensions  Total    1
#> 7       U   Total-ordinary  Total    1
#> 8       U    Total-special  Total    2
#> 9       g Total-assistance  Total    5
#> 10      g   Total-ordinary  Total    7
#> 11      g    Total-special  Total    3
#> 12      h    Total-special  Total    7
#> 13      i Total-assistance  Total    4
#> 14      i      Total-other  Total    6
#> 15      i   Total-pensions  Total    5
#> 16      i      Total-wages  Total    6
#> 17      i   Total-ordinary  Total    4
#> 18      i    Total-special  Total    3
#> 19      j Total-assistance  Total    7
#> 20      j    Total-special  Total    5
#> 21      u Total-assistance  Total    0
#> 22      u    Total-special  Total    0
#> 
#> GaussSuppression_anySum0: ..........................

# Only the categories "assistance" and "wages" are considered sensitive
# Also use "special" and "ordinary" as identifying categories (instead of "Total")
a4 <- fun(sensitive = list(main_income = c("assistance", "wages")), 
          identifying = list(region = "*", main_income = c("special", "ordinary")))  
#> [extend0 432*5->432*5]
#> 
#> ---- primary suppressed difference cells ---
#>    region        main_income months diff
#> 1       T     ordinary-wages  Total    5
#> 2       g     ordinary-wages  Total    2
#> 3       h     ordinary-wages  Total    6
#> 4       i     ordinary-wages  Total    2
#> 5       j     ordinary-wages  Total    3
#> 6       N special-assistance  Total    7
#> 7       O special-assistance  Total    3
#> 8       Q special-assistance  Total    6
#> 9       S special-assistance  Total    0
#> 10      T special-assistance  Total    3
#> 11      X special-assistance  Total    6
#> 12      Z special-assistance  Total    5
#> 13      a special-assistance  Total    1
#> 14      b special-assistance  Total    6
#> 15      f special-assistance  Total    4
#> 16      g special-assistance  Total    2
#> 17      h special-assistance  Total    2
#> 18      i special-assistance  Total    1
#> 19      j special-assistance  Total    2
#> 20      l special-assistance  Total    4
#> 21      m special-assistance  Total    4
#> 22      n special-assistance  Total    4
#> 23      p special-assistance  Total    3
#> 24      q special-assistance  Total    3
#> 25      t special-assistance  Total    2
#> 26      u special-assistance  Total    0
#> 
#> GaussSuppression_anySum0: ..........................
          
# As above, but additionally exclude regions i and j via the sensitive specification          
a5 <- fun(sensitive = list(main_income = c("assistance", "wages")), 
          identifying = list(region = "*", main_income = c("special", "ordinary")), 
          targeting_exclude = list(list(sensitive = list(region = c("i", "j")))))
#> [extend0 432*5->432*5]
#> 
#> ---- primary suppressed difference cells ---
#>    region        main_income months diff
#> 1       T     ordinary-wages  Total    5
#> 2       g     ordinary-wages  Total    2
#> 3       h     ordinary-wages  Total    6
#> 4       N special-assistance  Total    7
#> 5       O special-assistance  Total    3
#> 6       Q special-assistance  Total    6
#> 7       S special-assistance  Total    0
#> 8       T special-assistance  Total    3
#> 9       X special-assistance  Total    6
#> 10      Z special-assistance  Total    5
#> 11      a special-assistance  Total    1
#> 12      b special-assistance  Total    6
#> 13      f special-assistance  Total    4
#> 14      g special-assistance  Total    2
#> 15      h special-assistance  Total    2
#> 16      l special-assistance  Total    4
#> 17      m special-assistance  Total    4
#> 18      n special-assistance  Total    4
#> 19      p special-assistance  Total    3
#> 20      q special-assistance  Total    3
#> 21      t special-assistance  Total    2
#> 22      u special-assistance  Total    0
#> 
#> GaussSuppression_anySum0: ..........................

# Same exclusion as above, but specified via identifying instead of sensitive
# Here `main_income` must also be specified, since the default for identifying is "Total" 
a6 <- fun(sensitive = list(main_income = c("assistance", "wages")), 
          identifying = list(region = "*", main_income = c("special", "ordinary")), 
          targeting_exclude = list(list(identifying = list(region = c("i", "j"), 
                                        main_income = "*"))))
#> [extend0 432*5->432*5]
#> 
#> ---- primary suppressed difference cells ---
#>    region        main_income months diff
#> 1       T     ordinary-wages  Total    5
#> 2       g     ordinary-wages  Total    2
#> 3       h     ordinary-wages  Total    6
#> 4       N special-assistance  Total    7
#> 5       O special-assistance  Total    3
#> 6       Q special-assistance  Total    6
#> 7       S special-assistance  Total    0
#> 8       T special-assistance  Total    3
#> 9       X special-assistance  Total    6
#> 10      Z special-assistance  Total    5
#> 11      a special-assistance  Total    1
#> 12      b special-assistance  Total    6
#> 13      f special-assistance  Total    4
#> 14      g special-assistance  Total    2
#> 15      h special-assistance  Total    2
#> 16      l special-assistance  Total    4
#> 17      m special-assistance  Total    4
#> 18      n special-assistance  Total    4
#> 19      p special-assistance  Total    3
#> 20      q special-assistance  Total    3
#> 21      t special-assistance  Total    2
#> 22      u special-assistance  Total    0
#> 
#> GaussSuppression_anySum0: ..........................
                                        
# The results are identical                                          
identical(a5,a6)
#> [1] TRUE


# Add relations so that additional difference cells may be suppressed 
a7 <- fun(sensitive = list(main_income = c("assistance", "wages")), 
          identifying = list(region = "*", main_income = c("special", "ordinary")), 
          targeting_exclude = list(list(identifying = list(region = c("i", "j"), 
                                        main_income = "*"))), 
          targeting_include = list(
            list(identifying = list(region = c("14", "U", "V", "X"), 
                                    main_income = c("special", "ordinary"), 
                                    months = c("m10m12", "Total")), 
                 sensitive = list(region = c("m01m05"), 
                                  main_income = c("pensions", "assistance")))))
#> [extend0 432*5->432*5]
#> 
#> ---- primary suppressed difference cells ---
#>    region        main_income months diff
#> 1       T     ordinary-wages  Total    5
#> 2       U  ordinary-pensions  Total    0
#> 3       V  ordinary-pensions  Total    4
#> 4       X  ordinary-pensions  Total    7
#> 5       g     ordinary-wages  Total    2
#> 6       h     ordinary-wages  Total    6
#> 7       N special-assistance  Total    7
#> 8       O special-assistance  Total    3
#> 9       Q special-assistance  Total    6
#> 10      S special-assistance  Total    0
#> 11      T special-assistance  Total    3
#> 12      X special-assistance  Total    6
#> 13      Z special-assistance  Total    5
#> 14      a special-assistance  Total    1
#> 15      b special-assistance  Total    6
#> 16      f special-assistance  Total    4
#> 17      g special-assistance  Total    2
#> 18      h special-assistance  Total    2
#> 19      l special-assistance  Total    4
#> 20      m special-assistance  Total    4
#> 21      n special-assistance  Total    4
#> 22      p special-assistance  Total    3
#> 23      q special-assistance  Total    3
#> 24      t special-assistance  Total    2
#> 25      u special-assistance  Total    0
#> 26     14 special-assistance m10m12    7
#> 
#> GaussSuppression_anySum0: ..........................
            
# As above, but use a data.frame for precise specification of relations
# Therefore, "V ordinary–pensions" is no longer included                                     
a8 <- fun(sensitive = list(main_income = c("assistance", "wages")), 
          identifying = list(region = "*", main_income = c("special", "ordinary")), 
          targeting_exclude = list(list(identifying = list(region = c("i", "j"), 
                                        main_income = "*"))), 
          targeting_include = list(
            list(identifying = data.frame(region = c("14", "U", "V", "X"), 
                                          main_income = c("special", "ordinary"), 
                                          months = c("m10m12", "Total")), 
                 sensitive = list(region = c("m01m05"), 
                                  main_income = c("pensions", "assistance")))))    
#> [extend0 432*5->432*5]
#> 
#> ---- primary suppressed difference cells ---
#>    region        main_income months diff
#> 1       T     ordinary-wages  Total    5
#> 2       U  ordinary-pensions  Total    0
#> 3       X  ordinary-pensions  Total    7
#> 4       g     ordinary-wages  Total    2
#> 5       h     ordinary-wages  Total    6
#> 6       N special-assistance  Total    7
#> 7       O special-assistance  Total    3
#> 8       Q special-assistance  Total    6
#> 9       S special-assistance  Total    0
#> 10      T special-assistance  Total    3
#> 11      X special-assistance  Total    6
#> 12      Z special-assistance  Total    5
#> 13      a special-assistance  Total    1
#> 14      b special-assistance  Total    6
#> 15      f special-assistance  Total    4
#> 16      g special-assistance  Total    2
#> 17      h special-assistance  Total    2
#> 18      l special-assistance  Total    4
#> 19      m special-assistance  Total    4
#> 20      n special-assistance  Total    4
#> 21      p special-assistance  Total    3
#> 22      q special-assistance  Total    3
#> 23      t special-assistance  Total    2
#> 24      u special-assistance  Total    0
#> 25     14 special-assistance m10m12    7
#> 
#> GaussSuppression_anySum0: ..........................
   
# Specify the same relations as above, but in a different way
# Using multiple list elements                                    
a9 <- fun(sensitive = list(main_income = c("assistance", "wages")), 
          identifying = list(region = "*", main_income = c("special", "ordinary")), 
          targeting_exclude = list(list(identifying = list(region = c("i", "j"), 
                                        main_income = "*"))), 
          targeting_include = list(
            list(identifying = list(region = "14", 
                                    main_income = "special", 
                                    months = "m10m12"), 
                 sensitive = list(region = "14", 
                                  main_income = "assistance", 
                                  months = "m10m12")), 
            list(identifying = list(region = c("U", "X"), 
                                    main_income = "ordinary", 
                                    months = "Total"), 
                 sensitive = list(region = c("U", "X"), 
                                  main_income = "pensions", 
                                  months = "Total"))))                                                                   
#> [extend0 432*5->432*5]
#> 
#> ---- primary suppressed difference cells ---
#>    region        main_income months diff
#> 1       T     ordinary-wages  Total    5
#> 2       U  ordinary-pensions  Total    0
#> 3       X  ordinary-pensions  Total    7
#> 4       g     ordinary-wages  Total    2
#> 5       h     ordinary-wages  Total    6
#> 6       N special-assistance  Total    7
#> 7       O special-assistance  Total    3
#> 8       Q special-assistance  Total    6
#> 9       S special-assistance  Total    0
#> 10      T special-assistance  Total    3
#> 11      X special-assistance  Total    6
#> 12      Z special-assistance  Total    5
#> 13      a special-assistance  Total    1
#> 14      b special-assistance  Total    6
#> 15      f special-assistance  Total    4
#> 16      g special-assistance  Total    2
#> 17      h special-assistance  Total    2
#> 18      l special-assistance  Total    4
#> 19      m special-assistance  Total    4
#> 20      n special-assistance  Total    4
#> 21      p special-assistance  Total    3
#> 22      q special-assistance  Total    3
#> 23      t special-assistance  Total    2
#> 24      u special-assistance  Total    0
#> 25     14 special-assistance m10m12    7
#> 
#> GaussSuppression_anySum0: ..........................

# The results are identical 
identical(a8,a9)
#> [1] TRUE
```
