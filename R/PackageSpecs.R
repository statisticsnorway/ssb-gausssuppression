#' Function for viewing built-in GaussSuppression specs
#'
#' @description Functions to retrieve the built-in specs. These can be retrieved using either
#' numerical indices or by specifying the spec name, see Details.
#'
#' @details The following table summarizes the built-in specs. Columns represent
#' different specs, and rows represent the parameter settings.

#'  |                         |\strong{smallCountSpec} |\strong{dominanceSpec} |\strong{NContributorsSpec} |\strong{KDisclosureSpec}   |
#'  |:------------------------|:-----------------------|:----------------------|:--------------------------|:--------------------------|
#'  |\strong{primary}         |PrimaryDefault          |DominanceRule          |NContributorsRule          |KDisclosurePrimary         |
#'  |\strong{protectZeros}    |TRUE                    |                       |FALSE                      |FALSE                      |
#'  |\strong{candidates}      |CandidatesDefault       |CandidatesNum          |                           |DirectDisclosureCandidates |
#'  |\strong{singleton}       |SingletonDefault        |                       |                           |                           |
#'  |\strong{extend0}         |TRUE                    |                       |FALSE                      |TRUE                       |
#'  |\strong{preAggregate}    |                        |!is.null(charVar)      |!is.null(charVar)          |                           |
#'  |\strong{domWeightMethod} |                        |default                |                           |                           |
#'  |\strong{singletonMethod} |                        |sub2Sum                |                           |                           |
#'  |\strong{secondaryZeros}  |                        |                       |                           |TRUE                       |
#'
#' @param x the character name or index of the spec to be returned. If `NULL` (default),
#' returns list of all specs
#' @param printTable Logical value (default `FALSE`). If `TRUE`, prints a table
#' description of all specs. Primarily used for documentation purposes.
#' @return returns a spec (if `!is.null(x)`), list of all specs (if `is.null(x)`
#'  and `printTable = FALSE`), or markdown table describing all specs (if `printTable = TRUE`).
#' @export
#'
#' @examples
#' PackageSpecs()
#' PackageSpecs(1)
#' PackageSpecs("smallCountSpec")
#' PackageSpecs(printTable = TRUE)
PackageSpecs <- function(x = NULL, printTable = FALSE) {
  specList <- list(
    smallCountSpec =
      list(
        primary = as.name("PrimaryDefault"),
        protectZeros = TRUE,
        candidates = as.name("CandidatesDefault"),
        singleton = as.name("SingletonDefault"),
        extend0 = TRUE
      ),
    
    dominanceSpec =
      list(
        primary = as.name("DominanceRule"),
        candidates = as.name("CandidatesNum"),
        preAggregate = FALSE,
        domWeightMethod = "default",
        singletonMethod = "sub2Sum"
      ),
    
    nContributorsSpec =
      list(
        primary = as.name("NContributorsRule"),
        protectZeros = FALSE,
        extend0 = FALSE,
        preAggregate = FALSE
      ),
    
    kDisclosureSpec = 
      list(
        primary = as.name("KDisclosurePrimary"),
        protectZeros = FALSE,
        secondaryZeros = 1,
        candidates = as.name("DirectDisclosureCandidates"),
        extend0 = TRUE,
        singletonMethod = "anySumNOTprimary"
      )
  )
  
  if (printTable) {
    rows <- unique(unlist(lapply(specList, names)))
    pt <- NULL
    for (name in names(specList)) {
      y <- specList[[name]]
      pt <-
        cbind(pt, c(name = sapply(rows, function(x)
          ifelse(x %in% names(y), as.character(y[[x]]), ""))))
    }
    colnames(pt) <- paste0("\\", "strong{", names(specList), "}")
    rownames(pt) <- paste0("\\strong{", rows, "}")
    return(knitr::kable(pt, format = "markdown", result = "asis"))
  }
  if (is.null(x)) {
    return(specList)
  }
  else if (is.numeric(x)) {
    if (x > length(specList))
      stop("Invalid spec index.")
  }
  else if (is.character(x)) {
    if (!x %in% names(specList))
      stop("Invalid spec name.")
  }
  specList[[x]]
}
