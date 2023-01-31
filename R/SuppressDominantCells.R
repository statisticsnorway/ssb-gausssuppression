

#' Suppress volume tables using dominant cell primary suppression.
#'
#' @inheritParams GaussSuppressionFromData
#' @inheritParams DominanceRule
#' @param numVar numerical variable to be aggregated and used in dominance rule
#' @param preAggVar Extra variables to be used as grouping elements in the dominance rule.
#' @param sWeightVar Name of variable which represents sampling weights to be used
#' in dominance rule
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
#' d <- data.frame(v1 = v1, num = num, sweight = sweight, freq = 1)
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
SuppressDominantCells <- function(data,
                                  n,
                                  k,
                                  freqVar = NULL,
                                  numVar = NULL,
                                  dimVar = NULL,
                                  hierarchies = NULL,
                                  formula = NULL,
                                  preAggVar = NULL,
                                  sWeightVar = NULL,
                                  ...,
                                  spec = PackageSpecs("dominanceSpec")
                                  ) {
  if (is.null(freqVar)) {
    freqVar <- rev(make.unique(c(names(data), "freq")))[1]
    data[[freqVar]] <- 1
  }
  GaussSuppressionFromData(
    data = data,
    n = n,
    k = k,
    freqVar = freqVar,
    numVar = numVar,
    dimVar = dimVar,
    hierarchies = hierarchies,
    formula = formula,
    charVar = preAggVar,
    sWeightVar = sWeightVar,
    spec = spec,
    ...
  )
}
