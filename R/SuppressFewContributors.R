#' Few contributors suppression
#' 
#' This function provides functionality for suppressing volume tables based on
#' the few contributors rule (\code{\link{NContributorsRule}}).
#'
#' @inheritParams GaussSuppressionFromData
#' @param numVar Numerical variable to be aggregated.
#'           Any `candidatesVar` that is specified and 
#'           not included in `numVar` will be aggregated accordingly.
#'           Also see patameter `remove0` below.  
#' @param contributorVar Extra variables to be used as grouping elements when counting contributors. 
#'                       Typically, the variable contains the contributor IDs.
#' @param removeCodes Vector of codes to be omitted when counting contributors.
#'                With empty `contributorVar` row indices are assumed
#'                and conversion to integer is performed.
#' @inheritParams NContributorsRule                 
#' @param candidatesVar Variable to be used in the candidate function to prioritize cells for 
#'           publication and thus not suppression. 
#'           The first `numVar` variable will be used if it is not specified.
#'
#' @return data.frame containing aggregated data and supppression information.
#'         Columns `nRule` and `nAll` contain the number of contributors.
#'         In the former, `removeCodes` is taken into account.  
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
#' SuppressFewContributors(d, formula = ~v1, maxN = 1, numVar = "num")
#' SuppressFewContributors(d, formula = ~v1, maxN = 2, numVar = "num")
#' SuppressFewContributors(d, formula = ~v1, maxN = 3, numVar = "num")
#' 
#' 
#' d2 <- SSBtoolsData("d2")[-5]
#' set.seed(123)
#' d2$v <- round(rnorm(nrow(d2))^2, 1)
#' d2$family_id <- round(2*as.integer(factor(d2$region)) + runif(nrow(d2)))
#' 
#' # Hierarchical region variables are detected automatically -> same output column
#' SuppressFewContributors(data = d2, maxN = 2, numVar = "v", contributorVar = "family_id",
#'                       dimVar = c("region", "county", "k_group"))
#' 
#' # Formula. Hierarchical variables still detected automatically.
#' # And codes 1:9 not counted 
#' SuppressFewContributors(data = d2, maxN = 1, numVar = "v", contributorVar = "family_id",
#'                       formula = ~main_income * k_group + region + county - k_group,
#'                       removeCodes = 1:9)
#' 
#' # With hierarchies created manually
#' ml <- data.frame(levels = c("@@", "@@@@", "@@@@@@", "@@@@@@", "@@@@@@", "@@@@"), 
#'         codes = c("Total", "not_assistance", "other", "pensions", "wages", "assistance"))
#' SuppressFewContributors(data = d2, maxN = 2, numVar = "v", contributorVar = "family_id",
#'                       hierarchies = list(main_income = ml, k_group = "Total_Norway"))
#'                       
#'                       
SuppressFewContributors <- function(data,
                                  maxN,
                                  numVar = NULL,
                                  dimVar = NULL,
                                  hierarchies = NULL,
                                  formula = NULL,
                                  contributorVar = NULL,
                                  removeCodes = character(0), 
                                  remove0 = TRUE,
                                  candidatesVar = NULL,
                                  ...,
                                  spec = PackageSpecs("fewContributorsSpec")) {
  if (length(candidatesVar)) {
    numVar <- unique(c(numVar, candidatesVar))
  }
  GaussSuppressionFromData(
    data,
    maxN = maxN,
    numVar = numVar,
    dimVar = dimVar,
    hierarchies = hierarchies,
    formula  = formula,
    charVar = contributorVar,
    removeCodes = removeCodes,
    remove0 = remove0,
    candidatesVar = candidatesVar,
    spec = spec,
    ...
  )
}