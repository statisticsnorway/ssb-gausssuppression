

#' Suppress volume tables using dominant cell primary suppression.
#'
#' @inheritParams GaussSuppressionFromData
#' @inheritParams DominanceRule
#' @param numVar numerical variable to be aggregated and used in dominance rule
#' @param contributorVar Extra variables to be used as grouping elements in the dominance rule.
#'                  Typically, the variable contains the contributor IDs.
#' @param sWeightVar Name of variable which represents sampling weights to be used
#' in dominance rule
#' 
#' @note Currently, the implementation of `sWeightVar` cannot handle all issues involving 
#'       `contributorVar` and aggregations. This means that the default for `extraAggregate` 
#'       specified in the spec (`TRUE`) is ignored when `sWeightVar` is non-NULL. Thus,
#'       the "old" default specified in \code{\link{GaussSuppressionFromData}} is used instead. 
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
                                  n,
                                  k,
                                  allDominance = FALSE,
                                  freqVar = NULL,
                                  numVar = NULL,
                                  dimVar = NULL,
                                  hierarchies = NULL,
                                  formula = NULL,
                                  contributorVar = NULL,
                                  sWeightVar = NULL,
                                  ...,
                                  spec = PackageSpecs("dominanceSpec")
                                  ) {
  
  if (!is.null(sWeightVar)) {
    spec["extraAggregate"] <- NULL
  }
  
  GaussSuppressionFromData(
    data = data,
    n = n,
    k = k,
    allDominance = allDominance,
    freqVar = freqVar,
    numVar = numVar,
    dimVar = dimVar,
    hierarchies = hierarchies,
    formula = formula,
    charVar = contributorVar,
    sWeightVar = sWeightVar,
    spec = spec,
    ...
  )
}
