

#' Small count frequency table suppression.
#'
#' @description This is a wrapper function of \code{\link{GaussSuppressionFromData}}
#' for small count frequency suppression. For common applications, the `spec`
#' parameter can be adjusted, see \code{\link{PackageSpecs}} for more
#' information. See Details for more information on function call customization.
#'
#' @details The specs provided in the package (see \code{\link{PackageSpecs}})
#' provide common parameter setups for small count suppression. However, it might
#' be necessary to customize the parameters further. In this case, certain
#' parameters from \code{\link{GaussSuppressionFromData}}
#' might need adjusting from the values provided by the package specs. In
#' particular, the parameters `protectZeros` (should zeros be primary
#' suppressed), `extend0` (should empty cells be added before primary
#' suppression), and `secondaryZeros` (should zero frequency cells be
#' candidates for secondary suppression) might be of interest. The examples
#' below illustrate how to override parameters specified by a spec. Note
#'  that this is only possible if `specLock = FALSE`.
#'
#'
#' @inheritParams GaussSuppressionFromData
#'
#' @return data frame containing aggregated data and suppression information.
#' @export
#'
#' @examples
#' mun_accidents <- SSBtoolsData("mun_accidents")
#'
#' SuppressSmallCounts(data = mun_accidents, maxN = 3, dimVar = 1:2, freqVar = 3)
#' # override default spec
#' SuppressSmallCounts(data = mun_accidents, maxN = 3, dimVar = 1:2, freqVar = 3, 
#'                     protectZeros = FALSE)
#'                     
#'                     
#' d2 <- SSBtoolsData("d2")
#' d2$f <- round(d2$freq/10)  # tenth as frequency in examples
#' 
#' # Hierarchical region variables are detected automatically -> same output column
#' SuppressSmallCounts(data = d2, maxN = 2, freqVar = "f", 
#'                     dimVar = c("region", "county", "k_group"))
#' 
#' # Formula. Hierarchical variables still detected automatically.
#' SuppressSmallCounts(data = d2, maxN = 3, freqVar = "f", 
#'                     formula = ~main_income * k_group + region + county - k_group)
#' 
#' # With hierarchies created manually
#' ml <- data.frame(levels = c("@@", "@@@@", "@@@@@@", "@@@@@@", "@@@@@@", "@@@@"), 
#'         codes = c("Total", "not_assistance", "other", "pensions", "wages", "assistance"))
#' SuppressSmallCounts(data = d2, maxN = 2, freqVar = "f", 
#'                     hierarchies = list(main_income = ml, k_group = "Total_Norway"))
#' 
#' 
#' # Data without pensions in k_group 400 
#' # And assume these are structural zeros (will not be suppressed)
#' SuppressSmallCounts(data = d2[1:41, ], maxN = 3, freqVar = "f", 
#'                     hierarchies = list(main_income = ml, k_group = "Total_Norway"), 
#'                     extend0 = FALSE, structuralEmpty = TRUE)
#' # -- Note for the example above -- 
#' # With protectZeros = FALSE 
#' #   - No zeros suppressed
#' # With extend0 = FALSE and structuralEmpty = FALSE 
#' #   - Primary suppression without protection (with warning) 
#' # With extend0 = TRUE and structuralEmpty = TRUE 
#' #   - As default behavior. Suppression/protection of all zeros (since nothing empty)
#' # With formula instead of hierarchies: Extra parameter needed when extend0 = FALSE.
#' #   - removeEmpty = FALSE,  to include empty zeros in output.       
#' 
#' 
#' # Using formula followed by FormulaSelection 
#' output <- SuppressSmallCounts(data = SSBtoolsData("example1"), 
#'                               formula = ~age * geo * year + eu * year, 
#'                               freqVar = "freq", 
#'                               maxN = 1)
#' FormulaSelection(output, ~(age + eu) * year)
#'                
SuppressSmallCounts <- function(data,
                                maxN,
                                freqVar = NULL,
                                dimVar = NULL,
                                hierarchies = NULL,
                                formula = NULL,
                                ...,
                                spec = PackageSpecs("smallCountSpec")) {
  GaussSuppressionFromData(
    data,
    maxN = maxN,
    freqVar = freqVar,
    dimVar = dimVar,
    hierarchies = hierarchies,
    formula  = formula,
    spec = spec,
    ...
  )
}
