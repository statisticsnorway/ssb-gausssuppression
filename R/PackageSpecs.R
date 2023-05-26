#' Function for viewing built-in GaussSuppression specs
#'
#' @description Functions to retrieve the built-in specs. These can be retrieved using either
#' numerical indices or by specifying the spec name, see Details.
#'
#' @details The following table summarizes the built-in specs. Columns represent
#' different specs, and rows represent the parameter settings.
#'
#'  |                         |\strong{smallCountSpec} |\strong{dominanceSpec}     |\strong{fewContributorsSpec} |\strong{kDisclosureSpec}   |
#'  |:------------------------|:-----------------------|:--------------------------|:----------------------------|:--------------------------|
#'  |\strong{primary}         |PrimaryDefault          |DominanceRule              |NContributorsRule            |KDisclosurePrimary         |
#'  |\strong{protectZeros}    |TRUE                    |FALSE                      |FALSE                        |FALSE                      |
#'  |\strong{candidates}      |CandidatesDefault       |CandidatesNum              |CandidatesNum                |DirectDisclosureCandidates |
#'  |\strong{singleton}       |SingletonDefault        |SingletonUniqueContributor |SingletonUniqueContributor   |SingletonDefault           |
#'  |\strong{extend0}         |TRUE                    |FALSE                      |FALSE                        |TRUE                       |
#'  |\strong{preAggregate}    |is.null(freqVar)        |!is.null(charVar)          |!is.null(charVar)            |is.null(freqVar)           |
#'  |\strong{extraAggregate}  |FALSE                   |TRUE                       |TRUE                         |FALSE                      |
#'  |\strong{secondaryZeros}  |FALSE                   |FALSE                      |FALSE                        |1                          |
#'  |\strong{domWeightMethod} |                        |"default"                  |                             |                           |
#'  |\strong{singletonMethod} |                        |"numttH"                   |"numttH"                     |"anySumNOTprimary"         |
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
      quote(list(
        primary = PrimaryDefault,
        protectZeros = TRUE,
        candidates = CandidatesDefault,
        singleton = SingletonDefault,
        extend0 = TRUE,
        preAggregate = is.null(freqVar),
        extraAggregate = FALSE,
        secondaryZeros = FALSE
      )),
    
    dominanceSpec =       
      quote(list(
      primary = DominanceRule,
      candidates = CandidatesNum,  
      singleton = SingletonUniqueContributor,
      preAggregate = !is.null(charVar),
      extraAggregate=TRUE,
      domWeightMethod = "default",
      singletonMethod = "numttH",
      protectZeros = FALSE,
      extend0 = FALSE,
      secondaryZeros = FALSE 
    )),
    
    fewContributorsSpec =
      quote(list(
        primary = NContributorsRule,
        protectZeros = FALSE,
        extend0 = FALSE,
        preAggregate = !is.null(charVar),
        extraAggregate=TRUE,
        candidates = CandidatesNum,  
        singleton = SingletonUniqueContributor,
        singletonMethod = "numttH",
        secondaryZeros = FALSE 
      )),
    
    kDisclosureSpec = 
      quote(list(
        primary = KDisclosurePrimary,
        protectZeros = FALSE,
        secondaryZeros = 1,
        candidates = DirectDisclosureCandidates,
        extend0 = TRUE,
        singletonMethod = "anySumNOTprimary",
        singleton = SingletonDefault,
        preAggregate = is.null(freqVar),
        extraAggregate = FALSE
      ))
  )
  
  if (printTable) {
    rows <- unique(unlist(lapply(specList, names)))
    rows <- rows[rows != ""]
    pt <- NULL
    for (name in names(specList)) {
      y <- specList[[name]]
      pt <-
        cbind(pt, c(name = sapply(rows, function(x)
          ifelse(x %in% names(y), deparse(y[[x]]), ""))))
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
