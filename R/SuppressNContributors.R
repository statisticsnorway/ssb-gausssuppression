#' Few contributors suppression
#' 
#' This function provides functionality for suppressing volume tables based on
#' the few contributors rule.
#'
#' @inheritParams GaussSuppressionFromData
#'
#' @return data.frame containing aggregated data and supppression information.
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
#' SuppressNContributors(d, formula = ~v1, maxN = 1, numVar = "num")
#' SuppressNContributors(d, formula = ~v1, maxN = 2, numVar = "num")
#' SuppressNContributors(d, formula = ~v1, maxN = 3, numVar = "num")
SuppressNContributors <- function(data,
                                  maxN,
                                  freqVar = NULL,
                                  dimVar = NULL,
                                  hierarchies = NULL,
                                  formula = NULL,
                                  preAggVar = NULL,
                                  ...,
                                  spec = PackageSpecs("nContributorsSpec")) {
  charVar <- preAggVar
  GaussSuppressionFromData(
    data,
    maxN = maxN,
    freqVar = freqVar,
    dimVar = dimVar,
    hierarchies = hierarchies,
    formula  = formula,
    charVar = charVar,
    spec = spec,
    ...
  )
}