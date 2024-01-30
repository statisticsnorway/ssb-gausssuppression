


#' Toy output function
#' 
#' Example of using special temporary feature.
#' Possible development function as input.
#'
#' @param ... 
#' @param words 
#'
#' @return
#' @keywords internal
#' @export
#'
#' @examples
#'  SuppressDominantCells(data = SSBtoolsData("magnitude1"), 
#'                        numVar = "value", 
#'                        formula = ~sector2 * geo + sector4 * eu, 
#'                        contributorVar = "company", 
#'                        n = 1:2, k = c(80, 99), 
#'                        output = ToyOutputFunction, 
#'                        words = rownames(mtcars))
ToyOutputFunction <- function(..., words = c("Yes", "No")) {
  no <- sample.int(length(words), nrow((crossTable)), replace = TRUE)
  num <- cbind(num, data.frame(no = no, words = words[no]))
  environment(TailGaussSuppressionFromData) <- environment()
  return(TailGaussSuppressionFromData(...))
}




# Copy of bottom code of GaussSuppressionFromData
# Can be useful inside a `output`-function. See ToyOutputFunction.
TailGaussSuppressionFromData = function(...){
  
  unsafePrimary <- -secondary[secondary < 0]
  secondary <- secondary[secondary > 0]
  
  
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
  
  
  publish
}