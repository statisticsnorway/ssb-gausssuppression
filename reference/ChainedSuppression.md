# Repeated GaussSuppression with forwarding of previous results

[`AdditionalSuppression`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/AdditionalSuppression.md)
is called several times. Each time with all previous results as
`suppressedData`.

## Usage

``` r
ChainedSuppression(..., withinArg = NULL)

ChainedSuppressionHi(..., hierarchies)

ChainedSuppressionHi1(..., hierarchies)
```

## Arguments

- ...:

  Arguments to `AdditionalSuppression`/`GaussSuppressionFromData` that
  are kept constant.

- withinArg:

  A list of named lists. Arguments to
  `AdditionalSuppression`/`GaussSuppressionFromData` that are not kept
  constant. List elements with suppressed data are also allowed.

- hierarchies:

  In the wrapper `ChainedSuppressionHi`, this argument will be used to
  generate the `withinArg` to `ChainedSuppression` with the same length
  (see examples). Then, element number `i` of `withinArg` is
  `list(hierarchies = hierarchies[1:i])`. In the similar wrapper,
  `ChainedSuppressionHi1`, `withinArg` has always two elements:
  `list(hierarchies = hierarchies[1])` and
  `list(hierarchies = hierarchies)`.

## Value

List of data frames. The wrappers, `ChainedSuppressionHi` and
`ChainedSuppressionHi1`, return a single data frame, which is the last
list item.

## Examples

``` r
z1 <- SSBtoolsData("z1")
z2 <- SSBtoolsData("z2")
z2b <- z2[3:5]
names(z2b)[1] <- "region"

# As GaussSuppressionFromData when a single element within withinArg
a1 <- ChainedSuppression(z1, 1:2, 3, maxN = 5)
#> GaussSuppression_anySum: ........................................
a2 <- ChainedSuppression(z1, withinArg = list(list(dimVar = 1:2, freqVar = 3, maxN = 5)))
#> GaussSuppression_anySum: ........................................
identical(a1, a2[[1]])
#> [1] TRUE

# b[[3]] include results from b[[1]] and b[[2]]
b <- ChainedSuppression(z1, freqVar = 3, withinArg = list(
       list(dimVar = 1,   maxN = 55), 
       list(dimVar = 2,   maxN = 55), 
       list(dimVar = 1:2, maxN = 5)))
#> GaussSuppression_anySum: ......
#> GaussSuppression_anySum: ....
#> GaussSuppression_anySum: ...................................

# d[[2]] is same as b1 in AdditionalSuppression examples
d <- ChainedSuppression(withinArg = list(
       list(data = z1,  dimVar = 1:2, freqVar = 3, maxN = 5), 
       list(data = z2,  dimVar = 1:4, freqVar = 5, maxN = 1)))
#> GaussSuppression_anySum: ........................................
#> GaussSuppression_anySum: ...........................

# Common variable names important. 
# Therefore kostragr renamed to region in z2b. 
f <- ChainedSuppression(withinArg = list(
       list(data = z1,  dimVar = 1:2, freqVar = 3, maxN = 5), 
       list(data = z2b, dimVar = 1:2, freqVar = 3, maxN = 5), 
       list(data = z2,  dimVar = 1:4, freqVar = 5, maxN = 1)))
#> GaussSuppression_anySum: ........................................
#> GaussSuppression_anySum: ..............
#> GaussSuppression_anySum: ..........................

# Parameters so that only suppressions are forwarded. 
# This is first iteration in linked tables by iterations. 
e <- ChainedSuppression(withinArg = list(
       list(data = z1,  dimVar = 1:2, freqVar = 3, maxN = 5), 
       list(data = z2b, dimVar = 1:2, freqVar = 3, maxN = 5), 
       list(data = z2,  dimVar = 1:4, freqVar = 5, maxN = 1)), 
       makeForced = FALSE, forceNotPrimary = FALSE)
#> GaussSuppression_anySum: ........................................
#> GaussSuppression_anySum: ..............
#> GaussSuppression_anySum: ..........................
       
# "A" "annet"/"arbeid" could be suppressed here, but not in f since f[[1]]      
e[[3]][which(e[[3]]$suppressed != f[[3]]$suppressed), ]  
#>    region hovedint ant primary suppressed
#> 2       1    annet  14   FALSE      FALSE
#> 3       1   arbeid  11   FALSE      FALSE
#> 7      10    annet  13   FALSE       TRUE
#> 8      10   arbeid   2   FALSE       TRUE
#> 47      A    annet  11   FALSE       TRUE
#> 48      A   arbeid  11   FALSE       TRUE
#> 97      K    annet   4   FALSE      FALSE
#> 98      K   arbeid   2   FALSE      FALSE


#### Demonstrate SuppressionByChainedHierarchies

dimLists <- SSBtools::FindDimLists(z2[, 4:1])


# Two ways of doing the same calculations
g1 <- ChainedSuppressionHi(z2, c(1, 3), 5, maxN = 1, hierarchies = dimLists)
#> GaussSuppression_anySum: .................................
#> GaussSuppression_anySum: ............................
g1b <-  ChainedSuppression(z2, c(1, 3), 5, maxN = 1, withinArg = list(
         list(hierarchies = dimLists[1]),
         list(hierarchies = dimLists[1:2]),
         list(hierarchies = dimLists[1:3])))[[3]]      
#> GaussSuppression_anySum: .................................
#> GaussSuppression_anySum: ............................
       
# Results different after combining hierarchies      
g2 <- ChainedSuppressionHi(z2, c(1, 3), 5, maxN = 1, 
         hierarchies = SSBtools::AutoHierarchies(dimLists))        
#> GaussSuppression_anySum: ..............................
       
# In this case, the same results can be obtained by:         
g3 <- ChainedSuppressionHi1(z2, c(1, 3), 5, maxN = 1, hierarchies = dimLists)        
#> GaussSuppression_anySum: ..............................
       
```
