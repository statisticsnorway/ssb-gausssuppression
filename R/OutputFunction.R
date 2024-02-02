


# Toy output function
# 
# Example of using special temporary feature.
# Possible development function as input.
#
#  SuppressDominantCells(data = SSBtoolsData("magnitude1"), 
#                        numVar = "value", 
#                        formula = ~sector2 * geo + sector4 * eu, 
#                        contributorVar = "company", 
#                        n = 1:2, k = c(80, 99), 
#                        output = ToyOutputFunction, 
#                        words = rownames(mtcars))

ToyOutputFunction <- function(..., words = c("Yes", "No")) {
  no <- sample.int(length(words), nrow((crossTable)), replace = TRUE)
  num <- cbind(num, data.frame(no = no, words = words[no]))
  environment(TailGaussSuppressionFromData) <- environment()
  return(TailGaussSuppressionFromData(...))
}



# Output function using ComputeIntervals 
# 
# By using special temporary feature.
#
#
#  SuppressDominantCells(data = SSBtoolsData("magnitude1"), 
#                        numVar = "value", 
#                        formula = ~sector2 * geo + sector4 * eu, 
#                        contributorVar = "company", 
#                        n = 1:2, k = c(80, 99), 
#                        output = GaussSuppression:::OutputIntervals, # This line can be dropped 
#                        lpPackage = "Rsymphony")

OutputIntervals <- function(..., 
                            minVal = NULL, 
                            gaussI = TRUE,
                            allInt = FALSE,
                            sparseConstraints = TRUE,
                            dominanceVar = NULL,
                            intervalVar = NULL) {
  
  if (is.null(lpPackage)) {
    lpPackage <- "lpSolve"
  }
  
  if (identical(intervalVar, freqVar) | ncol(num) == 0) {
    z <- freq
  } else {
    if (is.null(intervalVar)) {
      if (is.null(dominanceVar)) {
        intervalVar <- names(num)[1]
      } else {
        intervalVar <- dominanceVar
      }
    }
    z <- num[[intervalVar]]
  }
  
  suppressed__ <- rep(FALSE, m)
  suppressed__[primary] <- TRUE
  suppressed__[secondary] <- TRUE
  suppressed__[hidden] <- TRUE     # in interval computation, hidden similar to secondary
  suppressed__[forced] <- FALSE
  
  gauss_intervals <- ComputeIntervals(
    x = x,
    z = z,
    primary = primary,
    suppressed = suppressed__,
    minVal = minVal,
    allInt = allInt,
    sparseConstraints = sparseConstraints,
    lpPackage = lpPackage,
    gaussI = gaussI
  )
  
  num <- cbind(num, as.data.frame(gauss_intervals))
  
  environment(TailGaussSuppressionFromData) <- environment()
  return(TailGaussSuppressionFromData(...))
}


# To avoid check problems
utils::globalVariables(c("candidates", "crossTable", "data", "forced", 
                         "forcedInOutput", "freq", "freqVar", "hidden",
                         "m", "output", "primary", "printInc", "secondary", 
                         "singleton", "singletonMethod", "unsafeInOutput",
                         "weightVar", "x", "xExtraPrimary"))
                         
# Copy of bottom code of GaussSuppressionFromData
# Can be useful inside a `output`-function. See ToyOutputFunction.
TailGaussSuppressionFromData = function(...){
  
  
  unsafePrimary <- -secondary[secondary < 0]
  secondary <- secondary[secondary > 0]
  
  if(output=="outputGaussSuppression_x"){
    return(list(secondary = secondary, candidates = candidates, primary = primary, forced = forced, hidden = hidden, singleton = singleton, singletonMethod = singletonMethod, printInc = printInc, xExtraPrimary = xExtraPrimary, x = x))
  }
  if(output=="outputGaussSuppression"){
    return(list(secondary = secondary, candidates = candidates, primary = primary, forced = forced, hidden = hidden, singleton = singleton, singletonMethod = singletonMethod, printInc = printInc, xExtraPrimary = xExtraPrimary))
  }
  
  suppressed <- rep(FALSE, m)
  suppressed[primary] <- TRUE
  primary <- suppressed
  suppressed[secondary] <- TRUE
  suppressed[hidden] <- NA
  suppressed[forced] <- FALSE
  
  
  if (length(freq)) {
    freq <- matrix(freq)
    colnames(freq) <- freqVar
  }
  if (length(weight)) {
    weight <- matrix(weight)
    colnames(weight) <- weightVar
  }
  
  if (ncol(num)) {
    colnames_num_in_fw <- colnames(num) %in% c(freqVar, weightVar)
    if (any(colnames_num_in_fw)) {
      num <- num[, !colnames_num_in_fw, drop = FALSE]
    }
  }
  
  forcedInOut <- NA
  if (is.null(forced)) {
    if (forcedInOutput == "always") {
      forced <- rep(FALSE, m)
      forcedInOut <- TRUE
    } else {
      forcedInOut <- FALSE
    }
  } else {
    if (forcedInOutput == "always") {
      forcedInOut <- TRUE
    }
    if (forcedInOutput == "ifNonNULL") {
      forcedInOut <- TRUE
    }
    if (forcedInOutput == "ifany") {
      forcedInOut <- any(forced)
    }
    if (forcedInOutput == "no") {
      forcedInOut <- FALSE
    }
  }
  if (is.na(forcedInOut)) {
    warning('Wrong forcedInOutput input treated as "ifNonNULL"')
    forcedInOut <- TRUE
  }
  
  
  unsafeInOut <- NA
  if (unsafeInOutput == "ifForcedInOutput") {
    unsafeInOut <- forcedInOut
  }
  if (unsafeInOutput == "always") {
    unsafeInOut <- TRUE
  }
  if (unsafeInOutput == "ifany") {
    unsafeInOut <- length(unsafePrimary) > 0
  }
  if (unsafeInOutput == "no") {
    unsafeInOut <- FALSE
  }
  if (is.na(unsafeInOut)) {
    warning('Wrong unsafeInOutput input treated as "ifForcedInOutput"')
    unsafeInOut <- forcedInOut
  }
  if (unsafeInOut) {
    unsafe <- rep(FALSE, m)
    unsafe[unsafePrimary[unsafePrimary <= m]] <- TRUE
    if (any(unsafe & !primary)) {
      warning("Calculation of unsafe failed. Non-primary found.")
    }
    unsafe <- matrix(unsafe)
    colnames(unsafe) <- "unsafe"
  } else {
    unsafe <- matrix(0, m, 0)
  }
  
  
  if (forcedInOut) {
    forced <- matrix(forced)
    colnames(forced) <- "forced"
  } else {
    forced <- matrix(0, m, 0)
  }
  
  publish <- cbind(as.data.frame(crossTable), freq, num, weight, primary = primary, forced, unsafe, suppressed = suppressed)
  rownames(publish) <- NULL
  
  startCol <- attr(x, "startCol", exact = TRUE)
  if (!is.null(startCol)) {
    attr(publish, "startRow") <- startCol
  }
  
  attr(publish, "totCode") <- FindTotCode2(x, crossTable)
  
  
  if(output == "all"){
    if( length(unsafePrimary) > 0){
      unsafe = x[, unsafePrimary[unsafePrimary <= m], drop = FALSE] # reuse object name unsafe here
      if(any(unsafePrimary > m) & !is.null(xExtraPrimary)){
        unsafePxEx = unsafePrimary[unsafePrimary > m] - m
        unsafePxEx = unsafePxEx[unsafePxEx <= ncol(xExtraPrimary)]
        unsafe = cbind(unsafe, xExtraPrimary[, unsafePxEx, drop = FALSE])
      }
      
    } else {
      unsafe = NULL
    }
    return(list(publish = publish, inner = data, x = x, xExtraPrimary = xExtraPrimary, unsafe = unsafe))
  }
  
  if (output == "publish_inner_x") {
    return(list(publish = publish, inner = data, x = x))
  }
  
  if (output == "publish_inner") {
    return(list(publish = publish, inner = data))
  }
  
  if (output == "publish_x") {
    return(list(publish = publish, x = x))
  }
  
  publish
}