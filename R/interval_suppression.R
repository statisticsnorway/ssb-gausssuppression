
interval_suppression <- function(x, 
                                 candidates,
                                 primary,
                                 secondary, 
                                 forced,
                                 hidden,
                                 singleton,
                                 singletonMethod,
                                 ...,
                                 xExtraPrimary,
                                 whenEmptyUnsuppressed,
                                 cell_grouping,
                                 lpPackage,
                                 rangeLimits,
                                 z,
                                 printInc,
                                 printXdim,
                                 minVal = NULL,
                                 allInt = FALSE,
                                 gaussIFix = FALSE){
  
  if (length(forced)) {
    stop("forced when interval iteration not implemented")
  }
  
  #rangeLimits <- RangeLimitsDefault(..., primary = primary, num = num, freq = freq, freqVar = freqVar)
  
  if (ncol(rangeLimits) != 1) {
    stop("Only single intervalVar implemented")
  }
  
  intervalVar <- colnames(rangeLimits)
  colnames(rangeLimits) <- paste("rlim", colnames(rangeLimits), sep = "_")
  num <- as.data.frame(rangeLimits)
  
  m <- ncol(x)
  
  # Make sure primary is logical
  primary_input <- primary
  primary <- rep(FALSE, m)
  primary[primary_input] <- TRUE 
    
  suppressed <- rep(FALSE, m)
  suppressed[primary] <- TRUE
  suppressed[secondary] <- TRUE
  suppressed[hidden] <- TRUE     # in interval computation, hidden similar to  secondary
  
  suppressed_integer <- rep(0L, m)
  suppressed_integer[primary] <- 1L
  suppressed_integer[secondary] <- 2L
  
  
  gauss_intervals <- ComputeIntervals(
    x = x,
    z = z,
    primary = primary,
    suppressed = suppressed,
    minVal = minVal,
    allInt = allInt,
    lpPackage = lpPackage,
    cell_grouping = cell_grouping
  )
  
  gauss_ranges <- gauss_intervals[, 2] - gauss_intervals[, 1]
  risky <- (gauss_ranges - rangeLimits[, 1]) < 0
  risky[!primary] <- FALSE
  
  risky[is.na(risky)] <- FALSE    #  MISSING SET TO FALSE HERE, MISSING WILL CAUSE WARNING LATER
  
  
  colnames(gauss_intervals) <- paste(colnames(gauss_intervals), "1", sep = "_")
  num <- cbind(num, as.data.frame(gauss_intervals))
  
  newPrimary <- FixRiskyIntervals(
    x = x,
    z = z,
    primary = risky, 
    suppressed = suppressed,
    candidates = candidates, 
    minVal = minVal,
    allInt = allInt,
    gaussI = gaussIFix,
    lpPackage = lpPackage,
    rangeLimits =  rangeLimits[risky, 1],
    cell_grouping = cell_grouping
  )
  
  primary2 <- primary
  primary2[newPrimary] <- TRUE
  
  suppressed_integer[newPrimary] <- 3L
  suppressed1 <- suppressed
  suppressed2 <- suppressed1
  suppressed2[primary2] <- TRUE
  suppressed <- suppressed2
  
  if (any(cell_grouping>0)) {
    ncol_old <- ncol(x)
    x_ <- cbind(x, x0diff(x, repeated_as_integer(cell_grouping)))
    forced_ <- c(forced, SeqInc(ncol_old + 1, ncol(x_)))
  } else {
    x_ <- x
    forced_ <- forced  
  }
  

  secondary <- GaussSuppression(x = x_, candidates = candidates, primary = suppressed, forced = forced_, hidden = hidden, singleton = NULL, singletonMethod = "none", printInc = printInc, whenEmptyUnsuppressed = NULL, xExtraPrimary = xExtraPrimary, 
                                ...)
  
  suppressed_integer[secondary] <- 4L
  
  
  suppressed[secondary] <- TRUE
  
  
  gauss_intervals <- ComputeIntervals(
    x = x,
    z = z,
    primary = primary,
    suppressed = suppressed,
    minVal = minVal,
    allInt = allInt,
    lpPackage = lpPackage,
    cell_grouping = cell_grouping
  )
  
  
  gauss_ranges <- gauss_intervals[, 2] - gauss_intervals[, 1]
  risky <- (gauss_ranges - rangeLimits[, 1]) < 0
  risky[!primary] <- FALSE
  
  if (any(is.na(risky))) {
    warning(paste("Missing values in final risk calculation"))
    risky[is.na(risky)] <- FALSE
  }
  
  if(any(risky)){
    warning(paste("Still", sum(risky) ,"risky (Algorithm may iterate in the future)."))
  }
  
  num <- cbind(num, as.data.frame(gauss_intervals))
  
  num <- cbind(num, suppressed_integer = suppressed_integer)
  
  secondary <- which(suppressed & !primary)   # Suppressed is re-calculated in TailGaussSuppressionFromData
  
  list(secondary, num)
}

