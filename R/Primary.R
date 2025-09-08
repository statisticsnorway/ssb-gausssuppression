# combination of primary functions 
Primary <- function(primary, crossTable, x, eachPrimary = FALSE, ...) {
  num <- NULL
  pri <- 1L
  xExtraPrimary <- NULL # primary as matrix
  n <- nrow(crossTable)    # This line is why crossTable is parameter
  # n <- ncol(x),  crossTable instead of x since x can be omitted (GaussSuppressionTwoWay), 
  # x is parameter since nrow(x) below  
  if (is.function(primary)) {
    primary <- c(primary)  # This is a list
  }
  for (i in seq_along(primary)) {
    a <- primary[[i]](crossTable = crossTable, x = x, ...)
    if (is.list(a) & !is.data.frame(a)) {
      if (is.null(num)) {
        num <- a[[2]]
      } else {
        num <- cbind(num, a[[2]])
      }
      a <- a[[1]]
    }
    
    # Usual output from a primary function is a logical vector (recommended) 
    # or a vector of indices (duplicates are allowed). 
    # Below it is made sure that single column matrix or data.fram also works. 
    # KDisclosurePrimary is special and returns a matrix (xExtraPrimary) of 0s and 1s.  
    # Below it is made sure that matrix is interpreted correctly. 
    ### START INTERPRET CORRECTLY
    if (is.data.frame(a)) {
      if (ncol(a) != 1) {
        stop("Data frame primary output must have a single column ")
      }
      a <- a[[1]]
    }
    
    if (is.matrix(a)) {
      if (ncol(a) == 1) {
        if (!is.logical(a) & nrow(a) == nrow(x) &  max(a) == 1) {
          if (min(a) == 1) {
            warning("Primary output interpreted as xExtraPrimary (rare case of doubt)")
          }
        } else {
          a <- as.vector(a)
        }
      } else {
        if (nrow(a) != nrow(x)) {
          stop("Primary output cannot be interpreted (wrong matrix dimension)")
        }
      }
    }
    ### END INTERPRET CORRECTLY
    
    if (is.null(dim(a)) ){ # One way to test non-matrix  (both matrix and Matrix) 
      if (!is.logical(a)) { # Indices instead are allowed/possible  
        if (min(a) < 1) {
          stop("0 (or negative) index found in primary output (change to logical?)")
        }
        aInd <- a
        a <- rep(FALSE, n)
        a[aInd] <- TRUE
      }
      if (length(a) != n)
        stop("wrong length of primary function output")
      if (eachPrimary) {
        if (is.numeric(eachPrimary)) {
          pnum <- as.data.frame(matrix(as.integer(a), dimnames = list(NULL, paste0("primary", i))))
        } else {
          pnum <- as.data.frame(matrix(            a, dimnames = list(NULL, paste0("primary", i))))
        }
        if (is.null(num)) {
          num <- pnum
        } else {
          num <- cbind(num, pnum)
        }
      }
      pri <- pri * as.integer(!a)    # zeros (=TRUE since !) and NA’s are preserved
    } else { # When matrix or Matrix 
      xExtraPrimary <- cbind(xExtraPrimary, a)
    }
  }
  pri <- !as.logical(pri)
  pri[is.na(pri)] <- FALSE    # No suppression when any NA
  
  if (is.null(num) & is.null(xExtraPrimary)) {
    return(pri)
  }
  
  list(primary = pri, numExtra = num, xExtraPrimary = xExtraPrimary)
}



#' Default primary function
#'
#' Function for \code{\link{GaussSuppressionFromData}}
#'
#' @param freq Vector of output frequencies 
#' @param maxN Cells with frequency `<= maxN` are set as primary suppressed. 
#'             Can also be a named list or vector, where the value corresponding to `freqVar` will be used 
#'             if available. If not found, the name `"freq"` is tried as an alternative.
#' @param protectZeros When `TRUE`, cells with zero frequency are set as primary suppressed. 
#' @param freqVar Character string used to select the appropriate value from `maxN` if it is a named object. 
#'                see `maxN` above.
#' @param protectionIntervals Logical. When `TRUE`, special interval requirements are included in the output. 
#'   The rule is that the upper bound must be at least 1 above the observed frequency, 
#'   and the total interval width must be at least 2.
#'   The corresponding variables are added with names starting with `upmin_` and `rlim_`                 
#' @param ... Unused parameters 
#'
#' @return primary, \code{\link[SSBtools]{GaussSuppression}} input 
#' @export
PrimaryDefault <- function(freq, maxN = 3, protectZeros = TRUE, 
                           protectionIntervals = FALSE,
                           freqVar, ...) {
  
  if(is.null(maxN))         stop("A non-NULL value of maxN is required.")
  if(is.null(protectZeros)) stop("A non-NULL value of protectZeros is required.")

  maxN <- get_numeric_item(maxN, freqVar, "freq")
    
  primary <- freq <= maxN
  if (!protectZeros) 
    primary[freq == 0] <- FALSE
  
  if (protectionIntervals) {
    intervalLimits <- data.frame(upmin = freq + 1, rlim = 2)
    intervalLimits[!primary, ] <- NA
    colnames(intervalLimits) <- paste(colnames(intervalLimits), freqVar, sep = "_")
    return(list(primary = primary, numExtra = intervalLimits))
  }
  
  primary
}

# Extracts numeric value(s) from a named list or vector.
# Tries 'var_names' first, then 'alt_names' if provided.
# If input is a single value, it is returned as a numeric vector.
# Always returns a vector (via unlist); 'txt' is used for context in error messages.
#  examples
#    maxN <- c(freq = 5, char1 = 10, char2 = 20, char3 = 30)  # or maxN <- list(freq = 5, ..... 
#    get_numeric_item(maxN, "ant", "freq")
#    get_numeric_item(maxN, c("char1", "char3"))
get_numeric_item <- function(obj, var_names, alt_names = NULL, txt = "maxN") {
  if (length(obj) == 1) {
    return(unlist(obj))
  }
  obj <- as.list(obj)
  if (is.null(names(obj))) {
    stop(paste("Name(s) of", txt, "needed:"))
  }
  if (identical(var_names, alt_names)) {
    alt_names <- NULL
  }
  if (all(var_names %in% names(obj))) {
    return(unlist(obj[var_names]))
  }
  if (!is.null(alt_names) & all(alt_names %in% names(obj))) {
    return(unlist(obj[alt_names]))
  }
  if (is.null(alt_names)) {
    stop(paste("Name(s) of", txt, "needed:", paste0('"', var_names, '"',collapse = ", ")))
  }
  stop(paste("Name(s) of", txt, "needed:", 
             paste0('"', var_names, '"', collapse = ", "), " or ", 
             paste0('"', alt_names, '"', collapse = ", ")))
}



