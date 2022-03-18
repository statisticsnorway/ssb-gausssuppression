#' Find major contributors to aggregates 
#' 
#' Assuming aggregates are calculated via a dummy matrix by 
#' \code{z = t(x) \%*\% y}, 
#' the \code{n} largest contributors are found (value or index) for each aggregate. 
#'
#' @param x A (sparse) dummy matrix
#' @param y Vector of input values (contributors)
#' @param n Number of contributors to be found
#' @param decreasing Ordering parameter. Smallest contributors found when \code{FALSE}. 
#' @param index Indices to \code{y} returned when TRUE
#'
#' @return Matrix with lagest contributors in first column, second largest in second column and so on.  
#' @export
#' @importFrom SSBtools SortRows
#' @importFrom Matrix drop0
#' @importFrom methods as
#' 
#' @seealso \code{\link{ModelMatrix}}
#' 
#' @author Øyvind Langsrud
#'
#' @examples
#' library(SSBtools)
#' 
#' z <- SSBtoolsData("sprt_emp_withEU")
#' z$age[z$age == "Y15-29"] <- "young"
#' z$age[z$age == "Y30-64"] <- "old"
#' 
#' a <- ModelMatrix(z, formula = ~age + geo, crossTable = TRUE)
#' 
#' cbind(as.data.frame(a$crossTable), MaxContribution(a$modelMatrix, z$ths_per, 1))
#' cbind(a$crossTable, MaxContribution(a$modelMatrix, z$ths_per, 10))
#' cbind(a$crossTable, MaxContribution(a$modelMatrix, z$ths_per, 10, index = TRUE))
#' 
#' b <- ModelMatrix(z[, -4], crossTable = TRUE, inputInOutput = c(TRUE, FALSE, TRUE))
#' 
#' cbind(b$crossTable, MaxContribution(b$modelMatrix, z$ths_per, 10))
#' 
MaxContribution <- function(x, y, n = 1, decreasing = TRUE, index = FALSE) {
  
  ordy <- order(y, decreasing = decreasing)
  
  xT <- as(drop0(x[ordy, ]), "dgTMatrix")
  
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
  
  seqCol <- seq_len(ncol(x))
  
  maxC <- matrix(NA_integer_, ncol(x), n)
  
  for (i in seq_len(n)) {
    if (i > 1) {
      xM[ma, 1] <- 0
    }
    ma <- match(seqCol, xM[, 1])
    maxC[, i] <- ordy[xM[ma, 2]]
  }
  if (index) 
    return(maxC)
  
  maxC[] <- y[maxC]
  
  maxC
}