smallCountSpec <-
  list(primary = as.name("PrimaryDefault"),
       protectZeros = TRUE,
       candidates = as.name("CandidatesDefault"),
       singleton = as.name("SingletonDefault"),
       extend0 = TRUE)

dominanceSpec <-
  list(
    primary = as.name("DominanceRule"),
    candidates = as.name("CandidatesNum"),
    representativeSample = FALSE
  )

fewContributorsSpec <- 
  list(primary = as.name("PrimaryDefault"),
       protectZeros = FALSE,
       extend0 = FALSE)


#' Function for viewing built-in GaussSuppression specs
#' 
#' @description Functions to retrieve the built-in specs. These can be retrieved using either
#' numerical indices or by specifying the spec name, see Details.
#' 
#' @details The following table summarizes the built-in specs. Columns represent
#' different specs, and rows represent the parameter settings.
#' 
#' |              | `smallCountSpec`    | `dominanceSpec` | `fewContributorsSpec` |
#' | :----------- | :---------------- | :------------ | :------------------ |
#'   | `primary`      | `PrimaryDefault`    | `DominanceRule` | `PrimaryDefault` |
#'   | `protectZeros` | `TRUE`              |               | `FALSE`               |
#'   | `extend0`      | `TRUE`              |               | `FALSE`               |
#'   | `candidates`   | `CandidatesDefault` | `CandidatesNum` |                     |
#'   |  `singleton`   | `SingletonDefault`  |               |                     |
#'   
#' @return list of built-in specs
#' @export
#'
#' @examples
#' PackageSpecs()
#' PackageSpecs(1)
#' PackageSpecs("smallCountSpec")
PackageSpecs <- function(x = NULL) {
  specNames <- c("smallCountSpec",
                 "dominanceSpec",
                 "fewContributorsSpec"
                 )
                 if (is.null(x))
                   x <- seq_along(specNames)
                 else if (is.numeric(x)) {
                   if (x > 4)
                     stop("Invalid spec index.")
                 }
                 else if (is.character(x)) {
                   if (!x %in% specNames)
                     stop("Invalid spec name.")
                 }
                 list(smallCountSpec = smallCountSpec,
                      dominanceSpec = dominanceSpec,
                      fewContributorsSpec = fewContributorsSpec
                      )[[x]]
}

