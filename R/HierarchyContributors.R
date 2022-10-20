


#' Counts of input code contributions
#'
#' @param data input data  
#' @param x model matrix as created by `ModelMatrix` with `data`, `hierarchies` and `inputInOutput` as input
#' @param crossTable `crossTable` as created by `ModelMatrix` with `data`, `hierarchies` and `inputInOutput` as input
#' @param hierarchies Standardized hierarchies.  That is, output from `AutoHierarchies`.
#' @param inputInOutput `ModelMatrix` input.
#'
#' @return List of data frames of counts associated with `crossTable`
#' 
#' * **`min`:** Minimum number of times a contributing input code contributes  
#' * **`max`:** Maximum number of times a contributing input code contributes  
#' * **`n`:**   Number of contributing input codes
#' * **`ac`:**  Theoretical number of contributing input codes according to the hierarchy
#'
#' @keywords internal
#' @export
#' @importFrom SSBtools DummyHierarchies
#'
#' @examples 
#' z <- SSBtoolsData("sprt_emp_withEU")[-(1:3), ]
#' z$age[z$age == "Y15-29"] <- "young"
#' z$age[z$age == "Y30-64"] <- "old"
#' hi <- SSBtools::FindHierarchies(z[, -4])
#' inputInOutput <- c(TRUE, FALSE, FALSE)
#' mm <- SSBtools::ModelMatrix(z, hi, crossTable = TRUE, inputInOutput = inputInOutput)
#' 
#' out <- HierarchyContributors(z, mm$modelMatrix, mm$crossTable, hi, inputInOutput)
#' 
#' # The nonzero values are caused by the removed three data rows
#' cbind(mm$crossTable, out$max - out$min)
#' cbind(mm$crossTable, out$ac - out$n)
#' 
HierarchyContributors <- function(data, x, crossTable, hierarchies, inputInOutput = TRUE){
  a <- NminMax(x, data[names(hierarchies)])
  a$ac <- AllContributors(data, crossTable, hierarchies, inputInOutput)
  a
}



AllContributors <- function(data, crossTable, hierarchies, inputInOutput = TRUE) { # data input unnecessary for intended use ... without data NA may occur ... but this can be handled
  ac <- matrix(0L, nrow = nrow(crossTable), ncol = length(hierarchies))
  colnames(ac) <- names(hierarchies)
  inputInOutput <- rep_len(inputInOutput, length(hierarchies))
  for (i in seq_along(hierarchies)) {
    if (is.character(hierarchies[[i]])) {  # rowFactor
      ac[, i] <- 1L
    } else {
      dh <- rowSums(DummyHierarchies(hierarchies[i], inputInOutput = inputInOutput[i], data = data)[[1]] != 0)
      ac[, i] <- dh[match(crossTable[[names(hierarchies)[i]]], names(dh))]
    }
  }
  ac
}


NminMax <- function(x, g) {
  n <- matrix(0, nrow = ncol(x), ncol = ncol(g))
  colnames(n) <- colnames(g)
  mi <- n
  ma <- n
  y <- rep(1, nrow(x))
  for (i in seq_len(ncol(g))) {
    n[, i] <- Ncontributors(x, g[[i]])
    mi[, i] <- -MaxContribution(x = x, y = -y, n = 1, groups = g[[i]])  # When decreasing fixed:  MaxContribution(x=x, y=y, n=1, decreasing = FALSE, groups = g[[i]])
    ma[, i] <- MaxContribution(x = x, y = y, n = 1, groups = g[[i]])
  }
  list(min = mi, max = ma, n = n)
}
