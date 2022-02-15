

#' Repeated GaussSuppression  with forwarding of previous results
#' 
#' \code{\link{AdditionalSuppression}} is called several times. Each time with all previous  results as `suppressedData`.
#'
#' @param ...  Arguments to `AdditionalSuppression`/`GaussSuppressionFromData` that are kept constant.
#' @param withinArg A list of named lists. Arguments to `AdditionalSuppression`/`GaussSuppressionFromData` that are not kept constant. 
#'                  List elements with suppressed data are also allowed. 
#'
#' @return List of data frames
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
#' # b[[3]] include results from b[[1]] and b[[2]]
#' b <- ChainedSuppression(z1, freqVar = 3, withinArg = list(
#'        list(dimVar = 1,   maxN = 55), 
#'        list(dimVar = 2,   maxN = 55), 
#'        list(dimVar = 1:2, maxN = 5)))
#' 
#' # d[[2]] is same as b1 in AdditionalSuppression examples
#' d <- ChainedSuppression(withinArg = list(
#'        list(data = z1,  dimVar = 1:2, freqVar = 3, maxN = 5), 
#'        list(data = z2,  dimVar = 1:4, freqVar = 5, maxN = 1)))
#' 
#' # Common variable names important. 
#' # Therefore kostragr renamed to region in z2b. 
#' f <- ChainedSuppression(withinArg = list(
#'        list(data = z1,  dimVar = 1:2, freqVar = 3, maxN = 5), 
#'        list(data = z2b, dimVar = 1:2, freqVar = 3, maxN = 5), 
#'        list(data = z2,  dimVar = 1:4, freqVar = 5, maxN = 1)))
#' 
#' # Parameters so that only suppressions are forwarded. 
#' # This is first iteration in linked tables by iterations. 
#' e <- ChainedSuppression(withinArg = list(
#'        list(data = z1,  dimVar = 1:2, freqVar = 3, maxN = 5), 
#'        list(data = z2b, dimVar = 1:2, freqVar = 3, maxN = 5), 
#'        list(data = z2,  dimVar = 1:4, freqVar = 5, maxN = 1)), 
#'        makeForced = FALSE, forceNotPrimary = FALSE)
#'        
#' # "A" "annet"/"arbeid" could be suppressed here, but not in f since f[[1]]      
#' e[[3]][which(e[[3]]$suppressed != f[[3]]$suppressed), ]        
#'        
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


   