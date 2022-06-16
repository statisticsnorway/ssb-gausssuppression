

#' Linked tables by full \code{\link{GaussSuppressionFromData}} iterations  
#' 
#' \code{\link{AdditionalSuppression}} is called several times as in \code{\link{ChainedSuppression}}
#' 
#' This function is created as a spin-off from `AdditionalSuppression` and `ChainedSuppression`. 
#' The calculations run `GaussSuppressionFromData` from the input each time. 
#' There is no doubt that this can be done more efficiently. 
#' 
#' A consequence of this lazy implementation is that, in output, `primary` and `suppressed` are identical.  
#' 
#' Note that there is a residual risk when suppression linked tables by iterations.  
#'
#' @param ...  Arguments to `GaussSuppressionFromData` that are kept constant.
#' @param withinArg A list of named lists. Arguments to `GaussSuppressionFromData` that are not kept constant. 
#' @param maxIterLinked Maximum number of `GaussSuppressionFromData` calls for each table. 
#'
#' @return List of data frames
#' @export
#' 
#' @note In this function, the parameters `makeForced`  and `forceNotPrimary` to  `AdditionalSuppression` are forced to be `FALSE`.
#'
#' @examples
#' 
#' z1 <- SSBtoolsData("z1")
#' z2 <- SSBtoolsData("z2")
#' z2b <- z2[3:5]  # As in ChainedSuppression example 
#' names(z2b)[1] <- "region"
#' 
#' # The two region hierarchies as two linked tables
#' a <- LazyLinkedTables(z2, freqVar = 5, withinArg = list(
#'        list(dimVar = c(1, 2, 4)), 
#'        list(dimVar = c(1, 3, 4))))
#' 
#' # As 'f' and 'e' in ChainedSuppression example. 
#' # 'A' 'annet'/'arbeid' suppressed in b[[1]], since suppressed in b[[3]].
#' b <- LazyLinkedTables(withinArg = list(
#'        list(data = z1, dimVar = 1:2, freqVar = 3, maxN = 5), 
#'        list(data = z2b, dimVar = 1:2, freqVar = 3, maxN = 5), 
#'        list(data = z2, dimVar = 1:4, freqVar = 5, maxN = 1)))
#' 
LazyLinkedTables <- function(..., withinArg = NULL, maxIterLinked = 1000) {
  
  if (is.null(withinArg)) {
    return(GaussSuppressionFromData(...))
  }
  
  SuppressedIsPrimary <- function(z) {
    sum(z$suppressed) == sum(z$primary)
  }
  
  n <- length(withinArg)
  maxJ <- n * (maxIterLinked - 1L)
  
  a <- ChainedSuppression(makeForced = FALSE, forceNotPrimary = FALSE, ..., withinArg = withinArg)
  
  nSuppressedIsPrimary <- as.integer(SuppressedIsPrimary(a[[n]]))
  
  parentFrame <- parent.frame()
  sysCall <- sys.call()
  sysCall[[1]] <- as.name("AdditionalSuppression")
  sysCall[["withinArg"]] <- NULL
  sysCall <- as.list(sysCall)
  
  for (j in seq_len(maxJ)) {
    i <- 1L + ((j - 1L)%%n)
    a[[i]] <- eval(as.call(c(sysCall, list(suppressedData = a, makeForced = FALSE, forceNotPrimary = FALSE), withinArg[[i]])), envir = parentFrame)
    
    if (SuppressedIsPrimary(a[[i]])) {
      nSuppressedIsPrimary <- nSuppressedIsPrimary + 1L
    } else {
      nSuppressedIsPrimary <- 0L
    }
    if (nSuppressedIsPrimary == n) {
      return(a)
    }
    
  }
  stop("Iteration limit exceeded")
}

