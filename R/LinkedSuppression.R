 

#' Consistent Suppression of Linked Tables 
#'
#' @inheritParams AdditionalSuppression
#' @param data The `data` argument to `fun`. When NULL `data` must be included in  `withinArg`.
#' @param ... Arguments to `fun` that are not kept constant.
#' @param withinArg A list of named lists. Arguments to `fun` that are not kept constant.
#' @param linkedGauss  See \link{parameter_linkedGauss}. 
#' @param iterBackTracking See \link{parameter_linkedGauss}.
#' @param whenEmptyUnsuppressed Parameter to \code{\link[SSBtools]{GaussSuppression}}.
#'
#' @return List of data frames
#' @export
#'
#' @examples
#'  
#' #### Similar to parameter_linkedGauss example
#' # Trick "sector4 - sector4" and "geo - geo" to ensure same names in output 
#' output <- LinkedSuppression(data = SSBtoolsData("magnitude1"),
#'                  fun = SuppressDominantCells, 
#'                  withinArg = list(list(formula = ~(geo + eu) * sector2 + sector4 - sector4), 
#'                                   list(formula = ~eu:sector4 - 1 + geo - geo), 
#'                                   list(formula = ~geo + eu + sector4 - 1)), 
#'                  dominanceVar  = "value", 
#'                  pPercent = 10, 
#'                  contributorVar = "company",
#'                  singletonMethod = "none", 
#'                  linkedGauss = "back-tracking") 
#' 
#' ####  Similar to LazyLinkedTables example:
#' z1 <- SSBtoolsData("z1")
#' z2 <- SSBtoolsData("z2")
#' z2b <- z2[3:5]  # As in ChainedSuppression example 
#' names(z2b)[1] <- "region" 
#' # As 'f' and 'e' in ChainedSuppression example. 
#' # 'A' 'annet'/'arbeid' suppressed in b[[1]], since suppressed in b[[3]].
#' b <- LinkedSuppression(fun = SuppressSmallCounts,
#'      linkedGauss = "back-tracking",  
#'      withinArg = list(
#'        list(data = z1, dimVar = 1:2, freqVar = 3, maxN = 5), 
#'        list(data = z2b, dimVar = 1:2, freqVar = 3, maxN = 5), 
#'        list(data = z2, dimVar = 1:4, freqVar = 5, maxN = 1)))
LinkedSuppression <- function(fun, 
                              data = NULL, 
                              ..., 
                              withinArg = NULL, 
                              linkedGauss,
                              iterBackTracking = Inf,
                              whenEmptyUnsuppressed = NULL) {
  SSBtools::CheckInput(linkedGauss, type = "character", alt = c("local", "consistent", "back-tracking"), okNULL = FALSE)
  if (linkedGauss == "consistent") {
    stop("not yet implemented")
  }
  if (is.null(withinArg)) {
    return(fun(...))
  }
  if (!is.null(withinArg)) {
    if (!is.list(withinArg)) {
      stop("withinArg must be a list when non-NULL")
    }
  }
  parentFrame <- parent.frame()
  sysCall <- as.list(sys.call())[-1]
  
  removeArgs <- c("fun", "linkedGauss", "withinArg")
  if (is.null(data)) {
    removeArgs <- c(removeArgs, "data")
  }
  
  sysCall <- c(sysCall["fun"], sysCall[!(names(sysCall) %in% removeArgs)])
  
  env_list <- vector("list", length(withinArg))
  
  for (i in seq_along(withinArg)) {
    if (is.null(names(withinArg[[i]]))) {
      if (!is.data.frame(withinArg[[i]])) {
        stop("non-named element of withinArg must be a data frame")
      }
    } else {
      env_list[[i]] <- eval(as.call(c(sysCall, withinArg[[i]], output = "pre_gauss_env")), envir = parentFrame)
    }
  }
  
  primary_list <- lapply(env_list, function(x) x$primary)
  secondary_list <- rep(list(integer(0)), length(withinArg))
  totCode_list <- vector("list", length(withinArg))
  
  suppressedData <- lapply(env_list, function(x) x$crossTable)
  
  for (i in seq_along(suppressedData)) {
    suppressedData[[i]]$suppressed <- primary_list[[i]]
    totCode_list[[i]] <- FindTotCode2(env_list[[i]]$x, crossTable = env_list[[i]]$crossTable)
  }
  
  n <- length(withinArg)
  
  
  if (linkedGauss == "back-tracking") {
    maxJ <- iterBackTracking * n
  } else {
    maxJ <- n
  }
  
  j <- 0L
  nSuppressedIsPrimary <- 0L
  while (j < maxJ) {
    j <- j + 1L
    i <- 1L + ((j - 1L)%%n)
    cat(i, "\n")
    
    if (linkedGauss == "back-tracking") {
      suppressedData[[i]]$suppressed[PrimaryFromSuppressedData(x = env_list[[i]]$x, 
                                                               crossTable = env_list[[i]]$crossTable, 
                                                               suppressedData = suppressedData, 
                                                               totCode = totCode_list[[i]])] <- TRUE 
    }
    
    secondary_list[[i]] <- GaussSuppression(x = env_list[[i]]$x, candidates = env_list[[i]]$candidates, primary = suppressedData[[i]]$suppressed, forced = env_list[[i]]$forced, hidden = env_list[[i]]$hidden,
                                            singleton = env_list[[i]]$singleton, singletonMethod = env_list[[i]]$singletonMethod, printInc = env_list[[i]]$printInc, whenEmptyUnsuppressed = whenEmptyUnsuppressed, xExtraPrimary = env_list[[i]]$xExtraPrimary,
                                            unsafeAsNegative = TRUE)   # dot-dot-dot not include for now 
    suppressedData[[i]]$suppressed[secondary_list[[i]]] <- TRUE
    
    if (length(secondary_list[[i]]) == 0) {
      nSuppressedIsPrimary <- nSuppressedIsPrimary + 1L
    } else {
      nSuppressedIsPrimary <- 0L
    }
    if (nSuppressedIsPrimary == n) {
      break
    }
    
  }
  
  if (linkedGauss == "back-tracking" & nSuppressedIsPrimary != n) {
    warning("Iteration limit exceeded")
  }
  
  for (i in seq_along(suppressedData)) {
    secondary <- suppressedData[[i]]$suppressed & !primary_list[[i]]
    env_list[[i]]$secondary <- which(secondary)
  }
  
  for (i in seq_along(suppressedData)) {
    environment(TailGaussSuppressionFromData) <- env_list[[i]]
    suppressedData[[i]] <- TailGaussSuppressionFromData()
  }
  
  suppressedData
}
