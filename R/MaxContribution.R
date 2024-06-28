#' Find major contributions to aggregates 
#' 
#' Assuming aggregates are calculated via a dummy matrix by 
#' \code{z = t(x) \%*\% y}, 
#' the \code{n} largest contributions are found (value or index) for each aggregate. 
#'
#' @param x A (sparse) dummy matrix
#' @param y Vector of input values (contributors)
#' @param n Number of contributors to be found
#' @param decreasing Ordering parameter. Smallest contributors found when \code{FALSE}. 
#' @param index Indices to \code{y} returned when TRUE
#' @param groups When non-NULL, major contributions after aggregation within groups.
#'               Cannot be combined with `index = TRUE`. 
#'               The missing group category is excluded.
#' @param return2 When `TRUE`, two matrices are returned, `value` and `id`.
#'        The latter contains indices when `group` is `NULL` and otherwise a character matrix of groups. 
#'
#' @return Matrix with lagest contributions in first column, second largest in second column and so on.  
#'         Alternative output when using parameters `index` or `return2`.
#' @export
#' @importFrom SSBtools SortRows As_TsparseMatrix
#' @importFrom Matrix drop0
#' @importFrom methods new
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
#' 
#' a <- ModelMatrix(z, formula = ~age + geo, crossTable = TRUE)
#' 
#' cbind(as.data.frame(a$crossTable), MaxContribution(a$modelMatrix, z$ths_per, 1))
#' cbind(a$crossTable, MaxContribution(a$modelMatrix, z$ths_per, 10))
#' cbind(a$crossTable, MaxContribution(a$modelMatrix, z$ths_per, 10, index = TRUE))
#' 
#' # Both types of output can be achieved with return2 = TRUE)
#' identical(MaxContribution(a$modelMatrix, z$ths_per, 10, return2 = TRUE),
#'           list(value =  MaxContribution(a$modelMatrix, z$ths_per, 10), 
#'                id =  MaxContribution(a$modelMatrix, z$ths_per, 10, index = TRUE)))
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
#' # Group info obtained with return2 = TRUE
#' k9_id <- cbind(b$crossTable, MaxContribution(b$modelMatrix, z$ths_per, 10, groups = gr9, 
#'                                              return2 = TRUE)$id)
#' k9_id[c(4, 13, 17, 33), ]
#' 
#' 
#' # Verify similarity
#' z$y <- z$ths_per + (1:nrow(z))/100  # to avoid equal values
#' id1 <- MaxContribution(b$modelMatrix, z$y, 10, index = TRUE)
#' id1[!is.na(id1)] <- paste0("g", id1[!is.na(id1)])
#' mc2 <- MaxContribution(b$modelMatrix, z$y, 10, groups = gr18, return2 = TRUE)
#' id2 <- mc2$id
#' identical(id1, id2)
#' 
MaxContribution <- function(x, y, n = 1, decreasing = TRUE, 
                            index = FALSE, groups = NULL,
                            return2 = FALSE) {
  
  if (!is.null(groups)) {
    if (index) {
      stop("index when groups is not implemented")
    }
    return(MaxContributionGroups(x = x, y = y, n = n, decreasing = decreasing, groups = groups, return2 = return2))
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
  
  if (return2) {
    id <- maxC
    maxC[] <- y[maxC]
    return(list(value = maxC, id = id))
  }
  
  if (index) 
    return(maxC)
  
  maxC[] <- y[maxC]
  
  maxC
}



MaxContributionGroups <- function(x, y, n = 1, decreasing = TRUE, groups, return2) {
  
  if (length(groups) != nrow(x)) {
    stop("Incorrect length of groups")
  }
  
  if (anyNA(groups)) {
    rows <- !is.na(groups)
    groups <- groups[rows]
    x <- x[rows, , drop = FALSE]
    y <- y[rows]
  }
  
  if (return2) {
    fgroups <- factor(groups)
    groups <- as.integer(fgroups)
    fgroups <- levels(fgroups)
  } else {
    groups <- as.integer(factor(groups))
  }
  
  xT <- As_TsparseMatrix(x) # xT <- as(drop0(x), "dgTMatrix")
  
  # Old slow code 
  # xM <- data.frame(y = -decreasing * y[xT@i + 1], col = xT@j + 1, gr = groups[xT@i + 1])  # row = xT@i + 1 
  # xM <- aggregate(xM["y"], xM[c("col", "gr")], sum)[c("y", "col", "gr")]
  
  # New code 
  gT <- new("dgTMatrix", i = 0:(nrow(x) - 1L), j = groups - 1L, x = -as.numeric(decreasing) * y, Dim = c(nrow(xT), max(groups)))
  gT <- As_TsparseMatrix(crossprod(gT, xT))
  xM <- data.frame(y = gT@x, col = gT@j + 1, gr = gT@i + 1)
  
  xM <- as.matrix(xM)  # Needed since empty index below 
  
  xM <- SortRows(xM)  # better to rewrite to order with decreasing parameter 
  
  if (FALSE) { # Check difference between xM generated by old and new method 
    xMold <- data.frame(y = -decreasing * y[xT@i + 1], col = xT@j + 1, gr = groups[xT@i + 1])  # row = xT@i + 1 
    xMold <- aggregate(xMold["y"], xMold[c("col", "gr")], sum)[c("y", "col", "gr")]
    xMold <- as.matrix(xMold)
    xMold <- SortRows(xMold)
    cat("\n max(abs(xM-xMold)) = ", max(abs(xM - xMold)), "\n")
  }
  
  xM[, "y"] <- -decreasing * xM[, "y"]
  
  seqCol <- seq_len(ncol(x))
  
  maxC <- matrix(NA_integer_, ncol(x), n)
  if (return2) {
    id <- matrix(NA_character_, ncol(x), n)
  }
  
  for (i in seq_len(n)) {
    if (i > 1) {
      xM[ma, "col"] <- 0
    }
    ma <- match(seqCol, xM[, "col"])
    maxC[, i] <- xM[ma, "y"]
    if (return2) {
      id[, i] <- fgroups[xM[ma, "gr"]]
    }
  }
  
  if (return2) {
    return(list(value = maxC, id = id))
  }
  
  maxC
}


