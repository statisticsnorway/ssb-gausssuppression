

#' ChainedSuppression
#'
#' @param ...  dots 
#' @param withinArg withinArg 
#'
#' @return list
#' @export
#'
#' @examples
#' 
#' z1 <- SSBtoolsData("z1")
#' z2 <- SSBtoolsData("z2")
#' z2b <- z2[3:5]
#' names(z2b)[1] <- "region"
#' 
#' # As GaussSuppressionFromData when a single element within withinArg
#' a1 <- ChainedSuppression(z1, 1:2, 3, maxN = 5)
#' a2 <- ChainedSuppression(z1, withinArg = list(list(dimVar = 1:2, freqVar = 3, maxN = 5)))
#' identical(a1, a2[[1]])
#' 
#' 
#' b <- ChainedSuppression(z1, freqVar = 3, withinArg = list(
#'        list(dimVar = 1,   maxN = 55), 
#'        list(dimVar = 2,   maxN = 55), 
#'        list(dimVar = 1:2, maxN = 5)))
#' 
#' d <- ChainedSuppression(withinArg = list(
#'        list(data = z1,  dimVar = 1:2, freqVar = 3, maxN = 5), 
#'        list(data = z2,  dimVar = 1:4, freqVar = 5, maxN = 1)))
#' 
#' f <- ChainedSuppression(withinArg = list(
#'        list(data = z1,  dimVar = 1:2, freqVar = 3, maxN = 5), 
#'        list(data = z2b, dimVar = 1:2, freqVar = 3, maxN = 5), 
#'        list(data = z2,  dimVar = 1:4, freqVar = 5, maxN = 1)))
#' 
#' e <- ChainedSuppression(withinArg = list(
#'        list(data = z1,  dimVar = 1:2, freqVar = 3, maxN = 5), 
#'        list(data = z2b, dimVar = 1:2, freqVar = 3, maxN = 5), 
#'        list(data = z2,  dimVar = 1:4, freqVar = 5, maxN = 1)), 
#'        makeForced = FALSE, forceNotPrimary = FALSE)
#'        
ChainedSuppression <- function(..., withinArg = NULL) {
  if (is.null(withinArg)) {
    return(GaussSuppressionFromData(...))
  }
  if (!is.null(withinArg)) {
    if (!is.list(withinArg)) {
      stop("withinArg must be a list when non-NULL")
    }
  }
  parentFrame <- parent.frame()
  sysCall <- sys.call()
  sysCall[[1]] <- as.name("AdditionalSuppression")
  sysCall[["withinArg"]] <- NULL
  sysCall <- as.list(sysCall)
  
  for (i in seq_along(withinArg)) {
    if (is.null(names(withinArg[[i]]))) {
      if (!is.data.frame(withinArg[[i]])) {
        stop("non-named element of withinArg must be a data frame")
      }
      
    } else {
      withinArg[[i]] <- eval(as.call(c(sysCall, list(suppressedData = withinArg[seq_len(i - 1)]), withinArg[[i]])), envir = parentFrame)
    }
  }
  withinArg
}


   