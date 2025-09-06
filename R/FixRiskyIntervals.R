

#' New primary cells to fix risky intervals
#'
#' Indices to new primary cells are returned 
#'
#' Code in this function started from a copy of \code{\link{ComputeIntervals}}
#' 
#' @inheritParams ComputeIntervals
#' @param candidates `candidates` as indices
#' @param rangeLimits As computed by \code{\link{RangeLimitsDefault}} 
#'
#'
#' @importFrom stats na.omit runif
#' @importFrom utils flush.console
#' @importFrom Matrix colSums t crossprod solve
#' @importFrom SSBtools DummyDuplicated GaussIndependent Reduce0exact As_TsparseMatrix
#'
#' @export
#'
FixRiskyIntervals <-
  function(x,
           z,
           primary,
           suppressed,
           candidates = NULL, 
           minVal = NULL,
           lpPackage = "lpSolve",
           gaussI = FALSE,   # Here important parameter, FALSE for best results, gaussI2 instead below  
           allInt = FALSE,
           sparseConstraints = TRUE, 
           rangeLimits,
           cell_grouping = rep(0, length(z))) {
    

    
    if (!lpPackage %in% c("lpSolve", "Rsymphony", "Rglpk", "highs"))
      stop("Only 'lpSolve', 'Rsymphony' and 'Rglpk' solvers are supported.")
    
    if (!requireNamespace(lpPackage,  quietly = TRUE)) {
      stop(paste0("Package '", lpPackage, "' is not available."))
    }
    
    if (!sparseConstraints) {
      AsMatrix <- as.matrix
    } else {
      AsMatrix <- function(x) x
    }
    
    if (is.logical(primary))
      primary <- which(primary)
    
    if (!length(primary)) {
      return(integer(0))
    }
    
    if (is.logical(suppressed))
      suppressed <- which(suppressed)
    
    if (is.null(minVal)) {     # secondary not needed
      secondary <- integer(0)  # removing is more efficient 
    } else {
      secondary <- suppressed[!(suppressed %in% primary)]
    }
    
    candidates <- unique(c(candidates, seq_len(ncol(x))))
    
    rangeLimits_ <- matrix(NA, ncol(x), ncol(rangeLimits), dimnames = list(NULL, names(rangeLimits)))
    rangeLimits_[primary, ] <- as.matrix(rangeLimits)
    rangeLimits_ <- rangeLimits_[candidates, , drop = FALSE]
    
    rearrange <- order(candidates)
    
    primary_notordered <- primary
    
    x <- x[, candidates, drop = FALSE]
    z <- z[candidates]
    cell_grouping <- cell_grouping[candidates]
    primary <- match(primary, candidates)
    suppressed <- match(suppressed, candidates)
    
    
    input_ncol_x <- ncol(x)
    
    published <- seq_len(ncol(x))
    published <- published[!(published %in% suppressed)]
    
  
    cell_grouping <- repeated_as_integer(cell_grouping)

    if(any(cell_grouping != 0)){
      x <- cbind(x, x0diff(x, cell_grouping))
      z <- c(z, rep(0, ncol(x) - input_ncol_x))
      rangeLimits_ <- rbind(rangeLimits_, matrix(NA, ncol(x) - input_ncol_x, ncol(rangeLimits_)))
      
      avoid_duplicate_computation <- TRUE   # Similar code as in ComputeIntervals()
      if (avoid_duplicate_computation) {
          duplicated_cell_grouping <- which(cell_grouping != 0 & duplicated(cell_grouping))
          published_in_duplicated_cell_grouping <- published %in% duplicated_cell_grouping
          if (any(published_in_duplicated_cell_grouping)) {
            published <- published[!published_in_duplicated_cell_grouping]
          }
          primary_in_duplicated_cell_grouping <- primary %in% duplicated_cell_grouping
          if (any(primary_in_duplicated_cell_grouping)) {
            primary <- primary[!primary_in_duplicated_cell_grouping]
          }
      }
    }
    
    candidates_published <- candidates[published]
    
    cell_diff = SeqInc(input_ncol_x + 1, ncol(x))
    
    if (!length(c(published, cell_diff))) {
      cat("Infinity intervals without using a solver ...\n")
      lo <- rep(NA_integer_, input_ncol_x)
      up <- lo
      lo[primary] <- c(minVal, 0)[1]
      up[primary] <- Inf
      return(cbind(lo = lo, up = up))
    }
    
    # Reorder since first match important in DummyDuplicated
    x <- x[, c(published, cell_diff, primary, secondary), drop = FALSE]
    z <- z[c(published, cell_diff, primary, secondary)]
    
    rangeLimits_ <- rangeLimits_[c(published, cell_diff, primary, secondary), , drop = FALSE] 
    
    published2 <- seq_len(length(published))
    cell_diff2 <- length(published) + seq_len(length(cell_diff))
    primary2 <- length(published) + length(cell_diff) + seq_len(length(primary))
    secondary2 <-
      length(published) + length(cell_diff) + length(primary) + seq_len(length(secondary))
    
    cat("(", dim(x)[1], "*", length(published2), sep = "")
    
    
    # Reduce problem by duplicated columns  now only published
    idxDD <-  seq_len(ncol(x)) 
    
    #ddd = DummyDuplicated(x[, published2,drop=FALSE], idx = TRUE, rnd = TRUE)
    idxDD[seq_len(length(published2))] <- DummyDuplicated(x[ , published2,drop=FALSE], idx = TRUE, rnd = TRUE)
    
    
    
    idxDDunique <- unique(idxDD)
    
    if (length(idxDDunique) < length(idxDD)) {
      x <- x[, idxDDunique, drop = FALSE]
      z <- z[idxDDunique]
      
      rangeLimits_ <- rangeLimits_[idxDDunique, , drop = FALSE]
      
      candidates_published3 <- candidates_published[idxDDunique] 
      
      published3 <- which(idxDDunique %in% published2)
      cell_diff3 <- which(idxDDunique %in% cell_diff2)
      primary3 <- which(idxDDunique %in% primary2)
      secondary3 <- which(idxDDunique %in% secondary2)
      cat("-DDcol->", dim(x)[1], "*", length(published3), sep = "")
    } else {
      candidates_published3 <- candidates_published
      published3 <- published2
      cell_diff3 <- cell_diff2
      primary3 <- primary2
      secondary3 <- secondary2
    }
    
    # Reduce problem by duplicated rows
    ddt <- DummyDuplicated(x, rnd = TRUE, rows = TRUE)
    if (any(ddt)) {
      x <- x[!ddt, , drop = FALSE]
      cat("-DDrow->", dim(x)[1], "*", length(published3), "->", sep = "")
    }
    
    # Vectors of limits to be filled inn
    lo <- rep(NA_integer_, ncol(x))
    up <- lo
    
    
    ## 
    ## # Reduce problem by Reduce0exact       ###############   cannot use reduceByColSums now. Also with Reduce0exact zeros cannot be extra primary.   
    ## a <-
    ##   Reduce0exact(x[, published3, drop = FALSE], matrix(z[published3]))  ###### , reduceByColSums = TRUE)
    ## 
    ## cat("-0exact->", dim(a$x)[1], "*", dim(a$x)[2], sep = "")
    ## 
    
    # A function as Reduce0exact, but nothing done
    # Just to get output on same form so that old code works 
    ReduceNothing <- function(x, z) {
      list(x = x, 
           z = z, 
           yKnown = rep(FALSE, nrow(x)), 
           y = matrix(0, nrow(x), 1), 
           zSkipped = rep(FALSE, ncol(x)))
    }
    a <- ReduceNothing(x[, published3, drop = FALSE], matrix(z[published3]))  
    
    
    candidates_published4 <- candidates_published3[!a$zSkipped]
    
    # Reduce problem by duplicated columns again
    dd <- DummyDuplicated(a$x, rnd = TRUE)
    if (any(dd)) {
      a$x <- a$x[,!dd, drop = FALSE]
      a$z <- a$z[!dd]
      cat("-DDcol2->", dim(a$x)[1], "*", dim(a$x)[2], sep = "")
    }
    
    if (gaussI) {
      #ord <- order(colSums(a$x))   # Positive effect of this ordering?
      #a$x <- a$x[, ord, drop = FALSE]
      #a$z <- a$z[ord, drop = FALSE]
      
      gi <- GaussIndependent(a$x)
      a$x <- a$x[, gi$columns, drop = FALSE]
      a$z <- a$z[gi$columns]
      cat("-GaussI->", dim(a$x)[1], "*", dim(a$x)[2], sep = "")
      
      candidates_published5 <- candidates_published4[gi$columns]
    } else {
      candidates_published5 <- candidates_published4
    }
    
    cat(")\n")
    
    gaussI2 <- TRUE #  parameter?
    
    extra_suppressed = integer(0) 
    
    
    
    # There's been a lot of back and forth regarding parameters and 
    # whether they should be logical values or indices. 
    # As it stands, there's likely room for improvement, 
    # including better naming conventions and deciding between 
    # using logical or index values.
    
    # Here written as same general code as below
    if (!gaussI & gaussI2) {
      # Without gaussI above, then gaussI here instead
      extra_from_gaussI_I <- rep(FALSE, length(a$z))
      extra_suppressed_I <- rep(FALSE, length(a$z))
      extra_suppressed_I[extra_suppressed] <- TRUE
      extra_from_gaussI_I[!extra_suppressed_I] <- !(GaussIndependent(a$x[, !extra_suppressed_I, drop = FALSE], printInc = TRUE))$columns
      extra_from_gaussI <- which(extra_from_gaussI_I)
    } else {
      extra_from_gaussI <- integer(0)
    }
    
    # Smallest i computed so far where primary3 is not risky
    best_ok <- rep(0, length(primary3))
    
    # count number of extra suppressed when computing at i
    nsup <- rep(-1L, length(a$z))
    
    # Each i value two times according to SmartOrder. 
    # The last values are in order and is used for final calculations.
    # By calculating at other i values in advance, 
    # a lot of interval calculations can be skipped,
    # as we already know the intervals are sufficiently wide.
    
    so <- c(SmartOrder(length(a$z)), length(a$z))  # Extra at end to check final after possible suppression  
    
    # so <- c(-seq_len(length(a$z)), length(a$z)) # This is slow sequential alternative without SmartOrder.
    
    verboseFix <- TRUE
    nline <- 10
    
    nz <- length(a$z)
    
    for (k in seq_along(so)) {
      
      i <- abs(so[k])
      final <- so[k] < 0
      check <- best_ok < i  # Not all intervals need to be computed
      
      if (any(check) & (!final | nsup[i] != sum(extra_suppressed))) {
        
        if (verboseFix) {
          if (nline == 10) {
            cat("\n")
            nline <- 0
          }
          nline <- nline + 1
          cat(nz + 1L - i)
        }
        # cat("----", i, "---------\n")
        
        risky <- RiskyInterInterval(a, 
                                    x, 
                                    primary3, 
                                    secondary3, 
                                    minVal, 
                                    allInt,
                                    lpPackage,
                                    sparseConstraints,
                                    AsMatrix,
                                    cell_diff3 = cell_diff3,
                                    check,
                                    verbose = FALSE, 
                                    extra_suppressed = c(extra_suppressed, extra_from_gaussI),
                                    rangeLimits_,
                                    i)
        
        best_ok[!risky & check] <- i
        if (verboseFix) {
          if(any(risky)) cat("-") else cat("+")
        }
        
      }
      
      
      if (final) {
        if (min(best_ok) < i) {
          extra_suppressed <- c(extra_suppressed, i)
          
          if (!gaussI & gaussI2) {
            # Without gaussI above, then gaussI here instead # Løse det delvis med regresjon
            if (FALSE) {        # Old slow code. New GaussIndependent each time. 
              extra_from_gaussI_I <- rep(FALSE, length(a$z))  # Går bra siden allerede uahvhengige kol  
              extra_suppressed_I <- rep(FALSE, length(a$z))   # Dersom mer en 1 ny blir funnet så må det gjøre en ekte gaussI
              extra_suppressed_I[extra_suppressed] <- TRUE    # Trenger ikke ta med avhengige col som er < i
              extra_from_gaussI_I[!extra_suppressed_I] <- !(GaussIndependent(a$x[, !extra_suppressed_I, drop = FALSE], printInc = TRUE))$columns
              extra_from_gaussI <- which(extra_from_gaussI_I)
            } else {            # New code. Updated GaussIndependent-output computed by NewIndependentIdx
              idx_independent <- seq_len(length(a$z))
              idx_independent <- idx_independent[!(idx_independent %in% extra_suppressed)]
              idx_independent <- idx_independent[!(idx_independent %in% extra_from_gaussI)]
              idx_independent <- sort(c(idx_independent, i))
              extra_suppressed_old <- extra_suppressed[seq_len(length(extra_suppressed) - 1)]  # tar bort i igjen
              newIndependentIdx <- NewIndependentIdx(a$x, idx_independent, idx_new_remove = i, idx_removed = extra_suppressed_old)
              extra_from_gaussI_I <- rep(TRUE, length(a$z))
              extra_from_gaussI_I[extra_suppressed] <- FALSE
              extra_from_gaussI_I[newIndependentIdx] <- FALSE
              extra_from_gaussI <- which(extra_from_gaussI_I)
            }
          } else {
            extra_from_gaussI <- integer(0)
          }
          
          if (verboseFix) {
            # cat("\n=== ", i, " out of", length(a$z), "==  ", length(extra_suppressed), " new primary ====\n")
            cat("\n  ",nz + 1L - i, ": ", length(extra_suppressed), " new, ",  "(", adjust_precision(a$z[i]),  ") ",  sep ="")
            nline <- 0
          }
        }
      }
      if (!final) {
        nsup[i] <- sum(extra_suppressed)
      }
    }
    if (verboseFix) cat("\n")
    if (min(best_ok) < length(a$z)) {
      warning("still problematic intervals")
    }
    newPrimary <- candidates_published5[extra_suppressed]
    newPrimary
  }


RiskyInterInterval <- function(a, 
                               x, 
                               primary3, 
                               secondary3, 
                               minVal, 
                               allInt,
                               lpPackage,
                               sparseConstraints,
                               AsMatrix,
                               cell_diff3,
                               check,
                               verbose, 
                               extra_suppressed,
                               rangeLimits_,
                               i) {
  
  cols <- seq_len(i)
  cols <- cols[!(cols %in% extra_suppressed)]
  
  ai <- a
  ai$x <- ai$x[, cols, drop = FALSE]
  ai$z <- ai$z[cols]
  
  intervals1 <- ComputeIntervals1(ai, x, primary3, secondary3, minVal, allInt, lpPackage, sparseConstraints, AsMatrix, check, verbose,
                                  cell_diff3 = cell_diff3)

  
  gauss_ranges <- intervals1$up[primary3] - intervals1$lo[primary3]
  
  risky <- (gauss_ranges - rangeLimits_[primary3, "rlim"]) < 0
  risky[is.na(risky)] <- FALSE
  risky
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


