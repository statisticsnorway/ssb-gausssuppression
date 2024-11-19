


max_contribution <- function(x, 
                             y, 
                             n = 1, 
                             id = NULL, 
                             output = "y", 
                             drop = TRUE,
                             decreasing = TRUE) {
  
  out <- vector("list", 3)
  names(out) <- c("y", "id", "n_contr")
  
  output <- names(out) %in% output
  names(output) <- names(out)
  
  if (is.null(id)) {
    id <- seq_len(nrow(x))
    fid <- id
    id_input <- FALSE
  } else {
    id_input <- TRUE
    
    if (length(id) != nrow(x)) {
      stop("Incorrect length of id")
    }
    
    if (anyNA(id)) {
      rows <- !is.na(id)
      id <- id[rows]
      x <- x[rows, , drop = FALSE]
      y <- y[rows]
    }
    
    if (output[["id"]]) {
      fid <- factor(id)
      id <- as.integer(fid)
      fid <- levels(fid)
    } else {
      id <- as.integer(factor(id))
    }
  }
  
  xT <- As_TsparseMatrix(x) 
  
  gT <- new("dgTMatrix", i = 0:(nrow(x) - 1L), j = id - 1L, x = -as.numeric(decreasing) * y, Dim = c(nrow(xT), max(id)))
  gT <- As_TsparseMatrix(crossprod(gT, xT))
  xM <- data.frame(y = gT@x, col = gT@j + 1, gr = gT@i + 1)
  
  xM <- as.matrix(xM)  # Needed since empty index below 
  
  xM <- SortRows(xM) 
  
  xM[, "y"] <- -decreasing * xM[, "y"]
  
  seqCol <- seq_len(ncol(x))
  
  if (output[["n_contr"]]) {
    out$n_contr <- as.vector(table_all_integers(xM[, "col"], ncol(x)))
  }
  
  out$y <- matrix(NA_integer_, ncol(x), n)
  
  if (output[["id"]]) {
    out$id <- matrix(ifelse(id_input, NA_character_, NA_integer_), ncol(x), n)
  }
  
  for (i in seq_len(n)) {
    if (i > 1) {
      xM[ma, "col"] <- 0
    }
    ma <- match(seqCol, xM[, "col"])
    out$y[, i] <- xM[ma, "y"]
    if (output[["id"]]) {
      out$id[, i] <- fid[xM[ma, "gr"]]
    }
  }
  
  if (drop & sum(output) == 1) {
    return(out[[which(output)]])
  }
  
  out
}


