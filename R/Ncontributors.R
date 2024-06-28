
#' Find the number of unique groups contributing to aggregates 
#' 
#' Assuming aggregates are calculated via a dummy matrix by 
#' \code{z = t(x) \%*\% y}, the the number of unique contributing groups,
#' according to a grouping variable, are found for each aggregate.
#' The missing group category is not counted.  
#'  
#'
#' @param x A (sparse) dummy matrix
#' @param groups Vector of group categories 
#' 
#' @return Vector of numbers of unique groups
#' @export
#' @importFrom SSBtools SortRows As_TsparseMatrix
#' @importFrom Matrix drop0
#' 
#' @seealso \code{\link[SSBtools]{ModelMatrix}}
#' 
#' @author Øyvind Langsrud
#'
#' @examples
#' library(SSBtools)
#' 
#' z <- SSBtoolsData("sprt_emp_withEU")
#' z$age[z$age == "Y15-29"] <- "young"
#' z$age[z$age == "Y30-64"] <- "old"
#' z$groups <- c("A", "A", "B", "A", "B", "C")
#' 
#' a <- ModelMatrix(z, formula = ~age*eu + geo + year, crossTable = TRUE)
#' 
#' cbind(as.data.frame(a$crossTable), nGroups = Ncontributors(a$modelMatrix, z$groups))
#' cbind(as.data.frame(a$crossTable), nYears = Ncontributors(a$modelMatrix, z$year))
#' cbind(as.data.frame(a$crossTable), nUnique_ths_per = Ncontributors(a$modelMatrix, z$ths_per))
#' 
Ncontributors <- function(x, groups) {
  
  if(length(groups) != nrow(x)){
    stop("Incorrect length of groups")
  }
  
  if (anyNA(groups)) {
    rows <- !is.na(groups)
    groups <- groups[rows]
    x <- x[rows, , drop = FALSE]
  }
  
  if (ncol(x) == 0) {
    return(integer(0))
  }
  
  if (nrow(x) == 0) {
    return(rep(0L, ncol(x)))
  }
  
  if (ncol(x) == 1) {
    return(length(unique(groups[x[, 1] != 0])))
  }
  
  groups <- as.integer(factor(groups))
  
  if (max(groups) == 1) {
    # return(rep(1L, ncol(x)))   # But must take empty into account 
    return(as.integer(colSums(abs(x)) > 0))
  }
  
  ordgroups <- order(groups)
  groups <- groups[ordgroups]
  
  xT <- As_TsparseMatrix(x[ordgroups, , drop=FALSE]) # xT <- as(drop0(x[ordgroups, , drop=FALSE]), "dgTMatrix")
  
  xM <- cbind(col = xT@j + 1, row = xT@i + 1)
  
  diffxM1 <- diff(xM[, 1])
  if (any(diffxM1 < 0)) {
  } else {
    if (any(diff(xM[, 2])[diffxM1 == 0] < 0)) {
      xM <- SortRows(xM)
      warning("sorting needed")
    }
  }
  rm(diffxM1)
  
  xM[, 2] <- groups[xM[, 2]]
  
  xM <- xM[!duplicated(xM), ]
  
  tab <- table(c(xM[, 1], seq_len(ncol(x)))) - 1L  # sjekk sortering
  
  if (any(range(diff(as.integer(names(tab)))) != c(1L, 1L))) {
    warning("Sorting problems in output from table")
    return(tab)
  }
  as.vector(tab)
}

#' \code{\link{Ncontributors}} with holding-indicator
#' 
#' The aggregates (columns of \code{x}) are grouped by a holding indicator. 
#' Within each holding group, the number of unique groups (output) is set to be equal. 
#' 
#' A representative within the holding group is used to calculate output by \code{\link{Ncontributors}}. 
#' The one with maximal column sum of \code{x} is chosen as the representative. 
#' Normally this will be an aggregate representing the holding group total. 
#' When holdingInd is NULL (default), the function is equivalent to \code{\link{Ncontributors}}.
#'
#' @param x A (sparse) dummy matrix
#' @param groups      Vector of group categories
#' @param holdingInd  Vector of holding group categories
#' 
#' @return Vector of numbers of unique groups
#' @importFrom Matrix colSums
#' @export
#' 
#' @author Øyvind Langsrud
#' 
NcontributorsHolding <- function(x, groups, holdingInd=NULL) { # holding-indicator
  if (is.null(holdingInd)){
    return(Ncontributors(x, groups))
  }
  if (length(holdingInd) != ncol(x)) {
    stop("Incorrect length of holdingInd")
  }
  holding <- as.integer(factor(holdingInd))
  
  ordcols <- order(colSums(x), decreasing = TRUE)
  
  ma <- match(seq_len(max(holding)), holding[ordcols])
  
  Ncontributors(x[, ordcols[ma], drop = FALSE], groups)[holding]
}
