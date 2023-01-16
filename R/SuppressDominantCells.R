

#' Suppress volume tables using dominant cell primary suppression.
#'
#' @inheritParams GaussSuppressionFromData
#' @inheritParams DominanceRule
#' @param preAggVar Extra variables to be used as grouping elements in the dominance rule.
#'
#' @return data frame containing aggregated data and suppression information.
#' @export
SuppressDominantCells <- function(data,
                                  n,
                                  k,
                                  numVar = NULL,
                                  dimVar = NULL,
                                  hierarchies = NULL,
                                  formula = NULL,
                                  preAggVar = NULL,
                                  sWeightVar = NULL,
                                  ...,
                                  spec = PackageSpecs("dominanceSpec")
                                  ) {
  GaussSuppressionFromData(
    data = data,
    n = n,
    k = k,
    dimVar = dimVar,
    freqVar = freqVar,
    hierarchies = hierarchies,
    formula = formula,
    charVar = preAggVar,
    sWeightVar = SWeightVar,
    spec = spec,
    ...
  )
}
