



OutputIntervalIteration <- function(..., 
                            minVal = NULL, 
                            allInt = FALSE,
                            maxIterInterval = 50,
                            allIntervals = TRUE,
                            allDims) {
  
  if (is.null(lpPackage)) {
    lpPackage <- "lpSolve"
  }
  
  if (!is.null(forced)) {
    stop("forced when interval iteration not implemented")
  }
  
  rangeLimits <- RangeLimitsDefault(..., primary = primary, num = num, freq = freq, freqVar = freqVar)
  
  if (ncol(rangeLimits) != 1) {
    stop("Only single intervalVar implemented")
  }
  
  intervalVar <- colnames(rangeLimits)
  colnames(rangeLimits) <- paste("rlim", colnames(rangeLimits), sep = "_")
  num <- cbind(num, as.data.frame(rangeLimits))
  
  if (intervalVar == c(freqVar, "")[1]) { # since freqVar may be NULL
    z <- freq
  } else {
    z <- num[[intervalVar]]
  }
  
  suppressed <- rep(FALSE, m)
  suppressed[primary] <- TRUE
  suppressed[hidden] <- TRUE     # in interval computation, hidden similar to  secondary
  
  suppressed_numeric <- rep(0, m)
  suppressed_numeric[primary] <- 0.5
  
  risky <- primary
  
  
  for(i in seq_len(maxIterInterval)){
    cat("\n\n===================================================================\n")
    cat("========  Interval iteration  ", i, "    ==============================\n")
    cat("===================================================================\n\n\n")
    if(i>1){              
      # To calls to avoid possible error:  argument "whenEmptyUnsuppressed" matched by multiple actual arguments 
      if(hasArg("whenEmptyUnsuppressed") | !structuralEmpty){
        secondary <- GaussSuppression(x = x, candidates = candidates, primary = suppressed, forced = forced, hidden = hidden, singleton = NULL, singletonMethod = "none", printInc = printInc, xExtraPrimary = xExtraPrimary, 
                                      unsafeAsNegative = TRUE, ...)
      } else {
        secondary <- GaussSuppression(x = x, candidates = candidates, primary = suppressed, forced = forced, hidden = hidden, singleton = NULL, singletonMethod = "none", printInc = printInc, whenEmptyUnsuppressed = NULL, xExtraPrimary = xExtraPrimary, 
                                      unsafeAsNegative = TRUE, ...)
      }
    }
    suppressed[secondary] <- TRUE
    suppressed_numeric[secondary] = i
    
    if (i == maxIterInterval) {
      maxIterIntervalReached = TRUE
      break
    }
    
    gauss_intervals <- ComputeIntervals(
      x = x,
      z = z,
      primary = risky,
      suppressed = suppressed,
      minVal = minVal,
      allInt = allInt,
      lpPackage = lpPackage
    )
    gauss_ranges <- gauss_intervals[, 2] - gauss_intervals[, 1]
    risky_old <- risky
    risky <- (gauss_ranges - rangeLimits[, 1]) < 0
    risky[!risky_old] <- FALSE
    
    
    if (allIntervals) {
      colnames(gauss_intervals) <- paste(colnames(gauss_intervals), i, sep = "_")
      num <- cbind(num, as.data.frame(gauss_intervals))
    }
    
    if (!any(risky)) {
      break
    }
    
    primary_from_risky <- PrimaryFromRiskyDefault(x = x, y = z, risky = which(risky), candidates[candidates %in% which(!suppressed)],  allDims =  allDims)
    
    cat("\n risky: \n")
    o <- order(z[risky])
    print(paste0(sapply(z[risky][o], adjust_precision),"(", 
                 sapply(gauss_intervals[risky, 1][o], adjust_precision), ",",  
                 sapply(gauss_intervals[risky, 2][o], adjust_precision), ")") )
    
    
    suppressed[primary_from_risky] <- TRUE
    
    suppressed_numeric[primary_from_risky] <- i + 0.5
    
  }
  
  
  gauss_intervals <- ComputeIntervals(
    x = x,
    z = z,
    primary = primary,
    suppressed = suppressed,
    minVal = minVal,
    allInt = allInt,
    lpPackage = lpPackage
  )
  
  
  gauss_ranges <- gauss_intervals[, 2] - gauss_intervals[, 1]
  risky <- (gauss_ranges - rangeLimits[, 1]) < 0
  risky[!primary] <- FALSE
  
  secondary <- which(suppressed & !primary)
  
  num <- cbind(num, as.data.frame(gauss_intervals))
  num <- cbind(num, suppressed_numeric = suppressed_numeric)
  
  if (any(risky)) {
    num <- cbind(num, risky = risky)
    message("maxIterInterval reached")
    warning("risky primary cells in output")
  }
  
  environment(TailGaussSuppressionFromData) <- environment()
  return(TailGaussSuppressionFromData(...))
}



# Made by ChatGPT 
# Function to adjust precision based on the magnitude of the number
adjust_precision <- function(number) {
  if(number < 0.001) {
    return(formatC(number, format = "f", digits = 7))
  } else if(number < 1) {
    return(formatC(number, format = "f", digits = 4))
  } else if(number < 10) {
    return(formatC(number, format = "f", digits = 3))
  } else if(number < 100) {
    return(formatC(number, format = "f", digits = 2))
  } else {
    return(formatC(number, format = "f", digits = 1))
  }
}









