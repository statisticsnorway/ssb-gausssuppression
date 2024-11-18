max_contribution <- function(x, y, n = 1, decreasing = TRUE, groups, return2) {
  
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
  
  xT <- As_TsparseMatrix(x) 
  
  gT <- new("dgTMatrix", i = 0:(nrow(x) - 1L), j = groups - 1L, x = -as.numeric(decreasing) * y, Dim = c(nrow(xT), max(groups)))
  gT <- As_TsparseMatrix(crossprod(gT, xT))
  xM <- data.frame(y = gT@x, col = gT@j + 1, gr = gT@i + 1)
  
  xM <- as.matrix(xM)  # Needed since empty index below 
  
  xM <- SortRows(xM) 
  
  xM[, "y"] <- -decreasing * xM[, "y"]
  
  seqCol <- seq_len(ncol(x))
  
  if (return2) {
    nContributors <- as.vector(table_all_integers(xM[, "col"], ncol(x)))
  }
  
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
    return(list(value = maxC, id = id, nContributors = nContributors))
  }
  
  maxC
}


