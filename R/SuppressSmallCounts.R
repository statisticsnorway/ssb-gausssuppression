

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
#' mun <- c("k1", "k2", "k3", "k4", "k5", "k6")
#' inj <- c("serious", "light", "none", "unknown")
#' data <- expand.grid(mun, inj)
#' names(data) <- c("mun", "inj")
#' data$freq <- c(4,5,3,4,1,6,
#' 0,0,2,1,0,0,
#' 0,1,1,4,0,0,
#' 0,0,0,0,0,0)
#'
#' SuppressSmallCounts(data = data, maxN = 3, dimVar = 1:2, freqVar = 3)
#' # override default spec
#' SuppressSmallCounts(data = data, maxN = 3, dimVar = 1:2, freqVar = 3, protectZeros = FALSE)
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
