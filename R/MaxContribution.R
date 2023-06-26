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
#' @param groups When non-NULL, major contributions after aggregation within groups.
#'               Cannot be combined with `index = TRUE`. 
#'               The missing group category is excluded.
#'
#' @return Matrix with lagest contributors in first column, second largest in second column and so on.  
#' @export
#' @importFrom SSBtools SortRows As_TsparseMatrix
#' @importFrom Matrix drop0
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
#' k <- cbind(b$crossTable, MaxContribution(b$modelMatrix, z$ths_per, 10))
#' 
#' gr18 <- paste0("g", 1:18)                          # Each row is a group
#' k18 <- cbind(b$crossTable, MaxContribution(b$modelMatrix, z$ths_per, 10, groups = gr18))
#' identical(k, k18) # TRUE
#' 
#' gr9 <- paste0("g", as.integer(10 * z$ths_per)%%10) # 9 groups from decimal
#' k9 <- cbind(b$crossTable, MaxContribution(b$modelMatrix, z$ths_per, 10, groups = gr9))
#' 
#' k18[c(4, 13, 17, 33), ]
#' k9[c(4, 13, 17, 33), ]
#' 
MaxContribution <- function(x, y, n = 1, decreasing = TRUE, index = FALSE, groups = NULL) {
  
  if (!is.null(groups)) {
    if (index) {
      stop("index when groups is not implemented")
    }
    return(MaxContributionGroups(x = x, y = y, n = n, decreasing = decreasing, groups = groups))
  }
  
  ordy <- order(y, decreasing = decreasing)
  
  xT <- As_TsparseMatrix(x[ordy, ]) # xT <- as(drop0(x[ordy, ]), "dgTMatrix")
  
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



MaxContributionGroups <- function(x, y, n = 1, decreasing = TRUE, groups) {
  
  if (length(groups) != nrow(x)) {
    stop("Incorrect length of groups")
  }
  
  if (anyNA(groups)) {
    rows <- !is.na(groups)
    groups <- groups[rows]
    x <- x[rows, , drop = FALSE]
    y <- y[rows]
  }
  
  groups <- as.integer(factor(groups))
  
  xT <- As_TsparseMatrix(x) # xT <- as(drop0(x), "dgTMatrix")
  
  xM <- data.frame(y = -decreasing * y[xT@i + 1], col = xT@j + 1, gr = groups[xT@i + 1])  # row = xT@i + 1 
  
  xM <- aggregate(xM["y"], xM[c("col", "gr")], sum)[c("y", "col", "gr")]
  
  xM <- as.matrix(xM)  # Needed since empty index below 
  
  xM <- SortRows(xM)  # better to rewrite to order with decreasing parameter 
  
  xM[, "y"] <- -decreasing * xM[, "y"]
  
  seqCol <- seq_len(ncol(x))
  
  maxC <- matrix(NA_integer_, ncol(x), n)
  
  for (i in seq_len(n)) {
    if (i > 1) {
      xM[ma, "col"] <- 0
    }
    ma <- match(seqCol, xM[, "col"])
    maxC[, i] <- xM[ma, "y"]
  }
  
  maxC
}


