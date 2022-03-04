# combination of primary functions 
Primary <- function(primary, crossTable, ...) {
  num <- NULL
  pri <- 1L
  n <- nrow(crossTable)    # This line is why crossTable is parameter
  if (is.function(primary)) {
    primary <- c(primary)  # This is a list
  }
  for (i in seq_along(primary)) {
    a <- primary[[i]](crossTable = crossTable, ...)
    if (is.list(a)) {
      if (is.null(num)) {
        num <- a[[2]]
      } else {
        num <- cbind(num, a[[2]])
      }
      a <- a[[1]]
    }
    if (!is.logical(a)) { # Indices instead are allowed/possible  
      aInd <- a
      a <- rep(FALSE, n)
      a[aInd] <- TRUE
    }
    if (length(a) != n)
      stop("wrong length of primary function output")
    pri <- pri * as.integer(!a)    # zeros (=TRUE since !) and NA’s are preserved
  }
  pri <- !as.logical(pri)
  pri[is.na(pri)] <- FALSE    # No suppression when any NA
  
  if (is.null(num)) {
    return(pri)
  }
  list(primary = pri, numExtra = num)
}