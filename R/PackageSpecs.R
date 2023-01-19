#' Function for viewing built-in GaussSuppression specs
#'
#' @description Functions to retrieve the built-in specs. These can be retrieved using either
#' numerical indices or by specifying the spec name, see Details.
#'
#' @details The following table summarizes the built-in specs. Columns represent
#' different specs, and rows represent the parameter settings.
#'
#'  |                     |smallCountSpec    |dominanceSpec |fewContributorsSpec |
#'  |:--------------------|:-----------------|:-------------|:-------------------|
#'  |primary              |PrimaryDefault    |DominanceRule |PrimaryDefault      |
#'  |protectZeros         |TRUE              |              |FALSE               |
#'  |candidates           |CandidatesDefault |CandidatesNum |                    |
#'  |singleton            |SingletonDefault  |              |                    |
#'  |extend0              |TRUE              |              |FALSE               |
#'  |representativeSample |                  |FALSE         |                    |
#'
#' @return list of built-in specs
#' @export
#'
#' @examples
#' PackageSpecs()
#' PackageSpecs(1)
#' PackageSpecs("smallCountSpec")
#' PackageSpecs(printTable = TRUE)
PackageSpecs <- function(x = NULL, printTable = FALSE) {
  smallCountSpec <-
    list(
      primary = as.name("PrimaryDefault"),
      protectZeros = TRUE,
      candidates = as.name("CandidatesDefault"),
      singleton = as.name("SingletonDefault"),
      extend0 = TRUE
    )
  
  dominanceSpec <-
    list(
      primary = as.name("DominanceRule"),
      candidates = as.name("CandidatesNum"),
      representativeSample = FALSE
    )
  
  fewContributorsSpec <-
    list(
      primary = as.name("PrimaryDefault"),
      protectZeros = FALSE,
      extend0 = FALSE
    )
  
  specList <- list(
    smallCountSpec = smallCountSpec,
    dominanceSpec = dominanceSpec,
    fewContributorsSpec = fewContributorsSpec
  )
  if (printTable) {
    rows <- unique(unlist(lapply(specList, names)))
    pt <- NULL
    for (name in names(specList)) {
      y <- specList[[name]]
      pt <- cbind(pt, c(name = sapply(rows, function(x) ifelse(x %in% names(y), as.character(y[[x]]), ""))))
    }
    colnames(pt) <- names(specList)
    rownames(pt) <- rows
    return(knitr::kable(pt, format = "markdown", result = "asis"))
  }
  if (is.null(x)) {
    return(specList)
  }
  else if (is.numeric(x)) {
    if (x > 4)
      stop("Invalid spec index.")
  }
  else if (is.character(x)) {
    if (!x %in% names(specList))
      stop("Invalid spec name.")
  }
  specList[[x]]
}
