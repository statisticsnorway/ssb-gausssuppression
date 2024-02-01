

#' Function for calculating intervals for suppressed tables.
#'
#' This function solves linear programs to determine interval boundaries
#' for suppressed cells.
#'
#' This function is still experimental.
#' 
#' Default in for `bounds` parameter in `Rsymphony_solve_LP`: 
#' _The default for each variable is a bound between 0 and `Inf`._
#' Details in `lpSolve`: _Note that every variable is assumed to be `>= 0`!_
#'
#' @param x ModelMatrix, as output from SSBtools::ModelMatrix
#' @param z numerical vector with length ncol(x). Corresponds to table cell values
#' @param primary Vector indicating primary suppressed cells. Can be logical or
#' integer. If integer vector, indicates the columns of x which are considered
#' primary suppressed.
#' @param suppressed Vector indicating all suppressed cells. Can be logical or
#' integer. If integer vector, indicates the columns of x which are considered
#' suppressed.
#' @param minVal a known minimum value for table cells. Default NULL.
#' Note that 'minVal' is interpreted as the limiting value for all suppressed cells. 
#' Specifying 'minVal=0' would be redundant, as a minimum value of 0 is anyway 
#' assumed for inner cells (see details).
#' @param lpPackage The name of the package used to solve linear programs. Currently, only
#' 'lpSolve' (default) and 'Rsymphony' are supported.
#' @param gaussI Boolean vector. If TRUE (default), GaussIndependent is used to
#' reduce size of linear program.
#' @param  allInt Integer variables when TRUE. 
#' See `all.int` parameter in `lpSolve` and `types` parameter in `Rsymphony`
#'
#' @importFrom stats na.omit runif
#' @importFrom utils flush.console
#' @importFrom Matrix colSums t crossprod
#' @importFrom SSBtools DummyDuplicated GaussIndependent Reduce0exact As_TsparseMatrix
#'
#' @export
#'
#' @author Øyvind Langsrud and Daniel Lupp
ComputeIntervals <-
  function(x,
           z,
           primary,
           suppressed,
           minVal = NULL,
           lpPackage = "lpSolve",
           gaussI = TRUE,
           allInt = FALSE) {
    if (!lpPackage %in% c("lpSolve", "Rsymphony"))
      stop("Only 'lpSolve' and 'Rsymphony' solvers are supported.")
    
    if (!require(lpPackage, character.only = TRUE,  quietly = TRUE)) {
      stop(paste0("Package '", lpPackage, "' is not available."))
    }
    
    if (lpPackage == "lpSolve") {
      AsMatrix <- as.matrix
    } else {
      AsMatrix <- function(x) x
    }
    
    if (is.logical(primary))
      primary <- which(primary)
    if (is.logical(suppressed))
      suppressed <- which(suppressed)
    
    if (is.null(minVal)) {     # secondary not needed
      secondary <- integer(0)  # removing is more efficient 
    } else {
      secondary <- suppressed[!(suppressed %in% primary)]
    }
    
    input_ncol_x <- ncol(x)
    
    published <- seq_len(ncol(x))
    published <- published[!(published %in% suppressed)]
    
    # Reorder since first match important in DummyDuplicated
    x <- x[, c(published, primary, secondary), drop = FALSE]
    z <- z[c(published, primary, secondary)]
    
    published2 <- seq_len(length(published))
    primary2 <- length(published) + seq_len(length(primary))
    secondary2 <-
      length(published) + length(primary) + seq_len(length(secondary))
    
    cat("(", dim(x)[1], "*", length(published2), sep = "")
    
    
    # Reduce problem by duplicated columns
    idxDD <- DummyDuplicated(x, idx = TRUE, rnd = TRUE)
    idxDDunique <- unique(idxDD)
    
    if (length(idxDDunique) < length(idxDD)) {
      x <- x[, idxDDunique, drop = FALSE]
      z <- z[idxDDunique]
      
      published3 <- which(idxDDunique %in% published2)
      primary3 <- which(idxDDunique %in% primary2)
      secondary3 <- which(idxDDunique %in% secondary2)
      cat("-DDcol->", dim(x)[1], "*", length(published3), sep = "")
    } else {
      published3 <- published2
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
    
    
    # Reduce problem by Reduce0exact
    a <-
      Reduce0exact(x[, published3, drop = FALSE], matrix(z[published3]), reduceByColSums = TRUE)
    
    cat("-0exact->", dim(a$x)[1], "*", dim(a$x)[2], sep = "")
    
    # Reduce problem by duplicated columns again
    dd <- DummyDuplicated(a$x, rnd = TRUE)
    if (any(dd)) {
      a$x <- a$x[,!dd, drop = FALSE]
      a$z <- a$z[!dd]
      cat("-DDcol2->", dim(a$x)[1], "*", dim(a$x)[2], sep = "")
    }
    
    if (gaussI) {
      ord <- order(colSums(a$x))   # Positive effect of this ordering?
      a$x <- a$x[, ord, drop = FALSE]
      a$z <- a$z[ord, drop = FALSE]
      
      gi <- GaussIndependent(a$x)
      a$x <- a$x[, gi$columns, drop = FALSE]
      a$z <- a$z[gi$columns]
      cat("-GaussI->", dim(a$x)[1], "*", dim(a$x)[2], sep = "")
    }
    
    cat(")\n")
    
    # Make lp-input from Reduce0exact solution
    f.con <- AsMatrix(t(a$x))
    if (lpPackage == "lpSolve")
      f.dir <- rep("=", nrow(f.con))
    else
      f.dir <- rep("==", nrow(f.con))
    f.rhs <- as.vector((as.matrix(a$z)))
    
    if (!is.null(minVal)) {
      f.con <-
        rbind(f.con, AsMatrix(t(x[!a$yKnown, c(primary3, secondary3), drop = FALSE])))
      f.dir <-
        c(f.dir, rep(">=", length(primary3) + length(secondary3)))
      f.rhs <-
        c(f.rhs, rep(minVal, length(primary3) + length(secondary3)))
    }
    
    cat("\n")
    
    if (lpPackage == "lpSolve") {
      cat("Using lpSolve for intervals...\n")
    } else
      cat("Using Rsymphony for intervals...\n")
    
    for (j in seq_along(primary3)) {
      if (j %% max(1, length(primary3) %/% 50) == 0) {
        cat("-")
        flush.console()
      }
      i <- primary3[j]
      flush.console()
      f.obj <- as.vector(x[!a$yKnown, i])

      # x is before Reduce0exact
      # adapted to Reduce0exact solution by !a$yKnown
      if (lpPackage == "lpSolve") {
        lo[i] <- LpVal("min", f.obj, f.con, f.dir, f.rhs, all.int = allInt)
        up[i] <-
          LpVal("max", f.obj, f.con, f.dir, f.rhs, all.int = allInt)
      } else {
        lo[i] <- RsymphVal(
          max = FALSE,
          obj = f.obj,
          mat = f.con,
          dir = f.dir,
          rhs = f.rhs,
          types = c("C", "I")[1 + allInt]
        )
        up[i] <- RsymphVal(
          max = TRUE,
          obj = f.obj,
          mat = f.con,
          dir = f.dir,
          rhs = f.rhs,
          types = c("C", "I")[1 + allInt]
        )
      }
    }
    # Add values "removed" by Reduce0exact
    # a$y: A version of y (freq here) with known values correct and others zero
    addKnown <- as.vector(Matrix::crossprod(x, a$y))
    lo <- lo + addKnown
    up <- up + addKnown
    
    # Transfer to before  "Reduce problem by duplicated columns"
    ma <- match(idxDD, idxDDunique)
    lo <- lo[ma]
    up <- up[ma]
    
    
    # Make sure lo and up long enough since secondary2
    # was set to integer(0)
    if (is.null(minVal)) {
      lo_output <- rep(NA_integer_, input_ncol_x)
      up_output <- lo_output
    } else {
      lo_output <- lo
      up_output <- up
    }
    
    # Transfer to before "Reorder since ..."
    lo_output[c(published, primary, secondary)] <-
      lo[c(published2, primary2, secondary2)]
    up_output[c(published, primary, secondary)] <-
      up[c(published2, primary2, secondary2)]
    
    cat("\n")
    
    cbind(lo = lo_output, up = up_output)
  }
# lp wrapper


LpVal <- function(...) {
  lpobj <- lpSolve::lp(...)
  c(lpobj$objval, NA, NaN,-Inf)[lpobj$status + 1]
}


RsymphVal <- function(...) {
  lpobj <- Rsymphony::Rsymphony_solve_LP(...)
  c(lpobj$objval, NA)[lpobj$status + 1]
}
