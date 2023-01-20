

#' Suppress volume tables using dominant cell primary suppression.
#'
#' @inheritParams GaussSuppressionFromData
#' @inheritParams DominanceRule
#' @param preAggVar Extra variables to be used as grouping elements in the dominance rule.
#' @param sWeightVar Name of variable which represents sampling weights to be used
#' in dominance rule
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
