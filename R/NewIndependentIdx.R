
# Fast way to compute updated GaussIndependent-output after one extra removed
NewIndependentIdx <- function(x, 
                              idx_independent,  # independent cols 
                              idx_new_remove,   # one of idx_independent to be removed 
                              idx_removed = integer(0), # Removed cols. These are ignored.
                              tolEI = (.Machine$double.eps)^(1/2)) {
  
  independent_new <- rep(FALSE, ncol(x))
  independent_new[idx_independent] <- TRUE
  independent_new[idx_new_remove] <- FALSE
  
  dependent <- rep(TRUE, ncol(x))
  dependent[idx_independent] <- FALSE
  dependent[idx_removed] <- FALSE
  
  xdrop <- x[, idx_new_remove, drop = FALSE]
  xnew <- x[, independent_new, drop = FALSE]
  
  y <- x[, dependent, drop = FALSE]
  
  dep1 <- ExtraIndependent1(xnew, y, q = xdrop, tolEI = tolEI)
  
  if (!length(dep1)) {
    return(which(independent_new))  #return(dep1)
  }
  dep2 <- ExtraIndependent2(xnew, y[, dep1, drop = FALSE], tolEI = tolEI)
  
  dep12 <- dep1[dep2]
  sort(c(which(independent_new), which(dependent)[dep12]))
}



FitResid <- function(x, y, tolEI = NULL) {
  yHat <- x %*% solve(crossprod(x), crossprod(x, y))
  yResid <- y - yHat
  if (is.null(tolEI)) {
    return(yResid)
  }
  yResid[abs(yResid) < tolEI] <- 0
  drop0(yResid)
}

# Find y's correlated with x after a single x (called q) removed
ExtraIndependent1 <- function(x, y, q, tolEI) {
  qRes <- FitResid(x, q, tolEI = NULL)
  crossY <- crossprod(y, qRes)
  crossY <- crossY * max(abs(qRes))/crossprod(qRes)[1, 1]  # scale as fitted Y
  crossY[abs(crossY) < tolEI] <- 0
  crossY <- as.vector(drop0(crossY))
  which(abs(crossY) > 0)
}

# know that all y's correlated with x, but not correlations between y
ExtraIndependent2 <- function(x, y, tolEI) {
  a <- integer(0)
  for (i in seq_len(ncol(y))) {
    yi <- y[, i, drop = FALSE]
    if (sum(abs(FitResid(x, yi, tolEI = tolEI))) > 0) {
      # even i=1 tested for extra safety
      a <- c(a, i)
      x <- cbind(x, yi)
    }
  }
  a
}
