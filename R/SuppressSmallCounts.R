

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
#' @param maxN Suppression threshold. Cells with frequency `<= maxN` are marked  
#'        as primary suppressed. This parameter is passed to [PrimaryDefault()]  
#'        via [GaussSuppressionFromData()].
#'
#' @return data frame containing aggregated data and suppression information.
#' @export
#' 
#' @seealso [SSBtools::tables_by_formulas()]
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
#' 
#' # To illustrate hierarchical_extend0 
#' # (parameter to underlying function, SSBtools::Extend0fromModelMatrixInput)
#' SuppressSmallCounts(data = SSBtoolsData("example1"), 
#'                     formula = ~age * geo * eu, freqVar = "freq", 
#'                     maxN = 0,  avoidHierarchical = TRUE)
#' SuppressSmallCounts(data = SSBtoolsData("example1"), 
#'                     formula = ~age * geo * eu, freqVar = "freq", 
#'                     maxN = 0,  avoidHierarchical = TRUE,
#'                     hierarchical_extend0 = TRUE) 
#'                
#'                
#' # This example is similar to the one in the documentation of tables_by_formulas,  
#' # but it uses SuppressSmallCounts, and the input data (SSBtoolsData("magnitude1"))  
#' # is used to generate a frequency table by excluding the "value" variable. 
#' tables_by_formulas(SSBtoolsData("magnitude1"), 
#'                    table_fun = SuppressSmallCounts, 
#'                    table_formulas = list(table_1 = ~region * sector2, 
#'                                          table_2 = ~region1:sector4 - 1, 
#'                                          table_3 = ~region + sector4 - 1), 
#'                    substitute_vars = list(region = c("geo", "eu"), region1 = "eu"), 
#'                    collapse_vars = list(sector = c("sector2", "sector4")), 
#'                    maxN = 2)                 
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
