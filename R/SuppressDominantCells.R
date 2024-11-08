

#' Suppress magnitude tables using dominance `(n,k)` or p% rule for primary suppression.
#'
#' @inheritParams GaussSuppressionFromData
#' @inheritParams MagnitudeRule
#' @param n Parameter `n` in dominance rule. Default is `1:length(k)`.
#' @param dominanceVar Numerical variable to be used in dominance rule. 
#'           The first `numVar` variable will be used if it is not specified.
#' @param numVar Numerical variable to be aggregated.
#'           Any `dominanceVar` and `candidatesVar` that are specified and 
#'           not included in `numVar` will be aggregated accordingly. 
#' @param contributorVar Extra variables to be used as grouping elements in the dominance rule.
#'                  Typically, the variable contains the contributor IDs.
#' @param sWeightVar Name of variable which represents sampling weights to be used
#' in dominance rule
#' @param candidatesVar Variable to be used in the candidate function to prioritize cells for 
#'           publication and thus not suppression. If not specified, the same variable that is 
#'           used for the dominance rule will be applied (see `dominanceVar` and `numVar`).
#'
#' @param singletonZeros When negative values cannot occur, one can determine from a 
#'    non-suppressed marginal cell with the value 0 that all underlying cells also have the 
#'    value 0. The use of `singletonZeros = TRUE` is intended to prevent this phenomenon from 
#'    causing suppressed cells to be revealable. It is the zeros in the `dominanceVar` variable 
#'    that are examined. Specifically, the ordinary singleton method is combined with a method 
#'    that is actually designed for frequency tables. This approach also works for volume 
#'    tables when \code{\link{SingletonUniqueContributor0}} is utilized.
#'    
#' @param preAggregate Parameter to \code{\link{GaussSuppressionFromData}}.
#'        Necessary to include here since the specification in `spec` cannot take `sWeightVar` into account.     
#'
#' @return data frame containing aggregated data and suppression information.
#' @export
#' 
#' @examples 
#' num <- c(100,
#'          90, 10,
#'          80, 20,
#'          70, 30,
#'          50, 25, 25,
#'          40, 20, 20, 20,
#'          25, 25, 25, 25)
#' v1 <- c("v1",
#'         rep(c("v2", "v3", "v4"), each = 2),
#'         rep("v5", 3),
#'         rep(c("v6", "v7"), each = 4))
#' sweight <- c(1, 2, 1, 2, 1, 2, 1, 2, 1, 1, 2, 1, 1, 1, 2, 1, 1, 1)
#' d <- data.frame(v1 = v1, num = num, sweight = sweight)
#' 
#' # basic use
#' SuppressDominantCells(d, n = c(1,2), k = c(80,70), numVar = "num", formula = ~v1 -1)
#' SuppressDominantCells(d, k = c(80,70), numVar = "num", formula = ~v1 -1) # same as above
#' SuppressDominantCells(d, pPercent = 7, numVar = "num", formula = ~v1 -1) 
#' 
#' # with weights
#' SuppressDominantCells(d, n = c(1,2), k = c(80,70), numVar = "num",
#' dimVar = "v1", sWeightVar = "sweight")
#' 
#' # overwriting some parameters in default spec
#' SuppressDominantCells(d, n = c(1,2), k = c(80,70), numVar = "num",
#' dimVar = "v1", sWeightVar = "sweight", domWeightMethod = "tauargus")
#' 
#' # using dominance and few contributors rule together, see second example compared to first
#' SuppressDominantCells(d, n = c(1,2), k = c(80,70), numVar = "num", formula = ~v1 -1,
#' primary = c(DominanceRule, NContributorsRule), maxN = 3, allDominance = TRUE)
#' 
#' SuppressDominantCells(d, n = c(1,2), k = c(80,70), numVar = "num", formula = ~v1 -1,
#' primary = c(DominanceRule, NContributorsRule), maxN = 4, allDominance = TRUE)
#' 
#' 
#' d2 <- SSBtoolsData("d2")
#' set.seed(123)
#' d2$v <- rnorm(nrow(d2))^2
#' 
#' # Hierarchical region variables are detected automatically -> same output column
#' SuppressDominantCells(data = d2, n = c(1, 2), k = c(70, 95), numVar = "v", 
#'                       dimVar = c("region", "county", "k_group"), allDominance = TRUE)
#' 
#' # Formula. Hierarchical variables still detected automatically.
#' SuppressDominantCells(data = d2, n = c(1, 2), k = c(70, 95), numVar = "v", 
#'                       formula = ~main_income * k_group + region + county - k_group)
#' 
#' # With hierarchies created manually
#' ml <- data.frame(levels = c("@@", "@@@@", "@@@@@@", "@@@@@@", "@@@@@@", "@@@@"), 
#'         codes = c("Total", "not_assistance", "other", "pensions", "wages", "assistance"))
#' SuppressDominantCells(data = d2, n = c(1, 2), k = c(70, 95), numVar = "v", 
#'                       hierarchies = list(main_income = ml, k_group = "Total_Norway"))
SuppressDominantCells <- function(data,
                                  n = 1:length(k),
                                  k = NULL,
                                  pPercent = NULL, 
                                  allDominance = FALSE,
                                  dominanceVar = NULL,
                                  numVar = NULL,
                                  dimVar = NULL,
                                  hierarchies = NULL,
                                  formula = NULL,
                                  contributorVar = NULL,
                                  sWeightVar = NULL,
                                  ...,
                                  candidatesVar = NULL,
                                  singletonZeros = FALSE,
                                  preAggregate = !is.null(contributorVar) & is.null(sWeightVar), 
                                  spec = PackageSpecs("dominanceSpec")
                                  ) {
  if (is.null(k)) {
    n <- NULL
  }
  if (length(dominanceVar)) {
    numVar <- unique(c(numVar, dominanceVar))
  }
  if (length(candidatesVar)) {
    numVar <- unique(c(numVar, candidatesVar))
  } else {
    if (length(dominanceVar)) {
      candidatesVar <- dominanceVar
    }
  }
  
  
  GetSingletonMethod <- function(..., singletonMethod = eval(spec$singletonMethod)) {
    singletonMethod
  }
  singletonMethodHere <- GetSingletonMethod(...)
  GetSingleton <- function(..., singleton = eval(spec$singleton)) {
    singleton
  }
  singletonHere <- GetSingleton(...)
  
  if (singletonZeros & !inherits(singletonHere, "function")) {
    singletonZeros <- FALSE
    warning("singletonZeros ignored when singleton input is not a function")
  }
  
  if (singletonZeros) {
    if (length(singletonMethodHere) == 1) {
      singletonMethodHere <- c(freq = "anySumNOTprimary", num = singletonMethodHere)
    }
    singletonHere <- SingletonUniqueContributor0
  }
  
  GaussSuppressionFromDataHere <- function(..., singletonMethod, singleton) {
    GaussSuppressionFromData(..., 
                             singletonMethod = singletonMethodHere, 
                             singleton = singletonHere)
  }
  
  
  GaussSuppressionFromDataHere(
    data = data,
    n = n,
    k = k,
    pPercent =  pPercent,
    allDominance = allDominance,
    dominanceVar = dominanceVar, 
    numVar = numVar,
    dimVar = dimVar,
    hierarchies = hierarchies,
    formula = formula,
    charVar = contributorVar,
    sWeightVar = sWeightVar,
    candidatesVar = candidatesVar, 
    preAggregate = preAggregate, 
    spec = spec,
    ...
  )
}




