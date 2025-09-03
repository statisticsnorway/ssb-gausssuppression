

#' Function for calculating intervals for suppressed tables.
#'
#' This function solves linear programs to determine interval boundaries
#' for suppressed cells.
#' 
#' Default in for `bounds` parameter in `Rsymphony_solve_LP` and `Rglpk_solve_LP`: 
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
#' @param lpPackage The name of the package used to solve linear programs. Currently, 
#' 'lpSolve' (default), 'Rsymphony',  'Rglpk' and 'highs'  are supported.
#' @param gaussI Boolean vector. If TRUE (default), GaussIndependent is used to
#' reduce size of linear program.
#' @param  allInt Integer variables when TRUE. 
#' See `all.int` parameter in `lpSolve` and `types` parameter in `Rsymphony` and `Rglpk`. 
#' @param  sparseConstraints When TRUE, a sparse constraint matrix will be input to the 
#' solver. In the case of `lpSolve`, the sparse matrix is represented in triplet form 
#' as a dense matrix with three columns, and the `dense.const` parameter is utilized.
#' @param cell_grouping Numeric vector indicating group membership.
#'   Cells with the same value that is not 0 belong to the same group.
#'   A value of 0 indicates that the cell is not a member of any group.
#'   Members of the same group are assumed to have the same z-value and this
#'   is included as a condition when calculating intervals.
#'
#' @importFrom stats na.omit runif
#' @importFrom utils flush.console
#' @importFrom Matrix colSums t crossprod
#' @importFrom SSBtools DummyDuplicated GaussIndependent Reduce0exact As_TsparseMatrix
#' @importFrom methods as
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
           allInt = FALSE,
           sparseConstraints = TRUE,
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
    if (is.logical(suppressed))
      suppressed <- which(suppressed)
    
    input_ncol_x <- ncol(x)
    published <- seq_len(ncol(x))
    published <- published[!(published %in% suppressed)]
    
    cell_grouping <- repeated_as_integer(cell_grouping)
    
    avoid_duplicate_computation <- TRUE
    
    if (avoid_duplicate_computation) {
      avoid_duplicate_computation <- FALSE
      if (any(cell_grouping != 0)) {
        duplicated_cell_grouping <- which(cell_grouping != 0 & duplicated(cell_grouping))
        published_in_duplicated_cell_grouping <- published %in% duplicated_cell_grouping
        if (any(published_in_duplicated_cell_grouping)) {
          published <- published[!published_in_duplicated_cell_grouping]
        }
        primary_in_duplicated_cell_grouping <- primary %in% duplicated_cell_grouping
        if (any(primary_in_duplicated_cell_grouping)) {
          avoid_duplicate_computation <- TRUE
          primary_cg_duplicated <- primary[primary_in_duplicated_cell_grouping]
          primary <- primary[!primary_in_duplicated_cell_grouping]
        }
      }
    }
    
    if (is.null(minVal)) {     # secondary not needed
      secondary <- integer(0)  # removing is more efficient 
    } else {
      secondary <- suppressed[!(suppressed %in% primary)]
    }
    
    if(any(cell_grouping != 0)){
      x <- cbind(x, x0diff(x, cell_grouping))
      z <- c(z, rep(0, ncol(x) - input_ncol_x))
    }
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
    
    published2 <- seq_len(length(published))
    cell_diff2 <- length(published) + seq_len(length(cell_diff))
    primary2 <- length(published) + length(cell_diff) + seq_len(length(primary))
    secondary2 <-
      length(published) + length(cell_diff) + length(primary) + seq_len(length(secondary))
    
    cat("(", dim(x)[1], "*", length(published2), sep = "")
    
    
    # Reduce problem by duplicated columns
    idxDD <- DummyDuplicated(x, idx = TRUE, rnd = TRUE)
    idxDDunique <- unique(idxDD)
    
    if (length(idxDDunique) < length(idxDD)) {
      x <- x[, idxDDunique, drop = FALSE]
      z <- z[idxDDunique]
      published3 <- which(idxDDunique %in% published2)
      cell_diff3 <- which(idxDDunique %in% cell_diff2)
      primary3 <- which(idxDDunique %in% primary2)
      secondary3 <- which(idxDDunique %in% secondary2)
      cat("-DDcol->", dim(x)[1], "*", length(published3), sep = "")
    } else {
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
    
    intervals1 = ComputeIntervals1(a, 
                                 x, 
                                 primary3, 
                                 secondary3, 
                                 minVal, 
                                 allInt,
                                 lpPackage,
                                 sparseConstraints,
                                 AsMatrix,
                                 cell_diff3 = cell_diff3) 
    
  
    # Transfer to before  "Reduce problem by duplicated columns"
    ma <- match(idxDD, idxDDunique)
    lo <- intervals1$lo[ma]
    up <- intervals1$up[ma]
    
    
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
    
    output <- cbind(lo = lo_output, up = up_output)
    
    if (avoid_duplicate_computation) {
      ma <- match(cell_grouping[primary_cg_duplicated], cell_grouping)
      output[primary_cg_duplicated, ] <- output[ma, , drop = FALSE]
    }
    
    output
}
# lp wrapper






ComputeIntervals1 <- function(a, 
                             x, 
                             primary3, 
                             secondary3, 
                             minVal, 
                             allInt,
                             lpPackage,
                             sparseConstraints,
                             AsMatrix,
                             check = rep(TRUE, length(primary3)),
                             verbose = TRUE, 
                             cell_diff3 = integer(0)) {
  # Vectors of limits to be filled inn
  lo <- rep(NA_integer_, ncol(x))
  up <- lo
  
  
  if (lpPackage == "lpSolve"){
    eqtxt <- "="
  } else{
    eqtxt <- "=="  
  }
  
  # Make lp-input from Reduce0exact solution
  f.con <- AsMatrix(t(a$x))
  f.dir <- rep(eqtxt, nrow(f.con))
  f.rhs <- as.vector((as.matrix(a$z)))
  
  
  if(length(cell_diff3)){
    
    f.con <-
      rbind(f.con, AsMatrix(t(x[!a$yKnown, cell_diff3, drop = FALSE])))
    f.dir <-
      c(f.dir, rep(eqtxt, length(cell_diff3)))
    f.rhs <-
      c(f.rhs, -as.vector(Matrix::crossprod(x[ , cell_diff3, drop = FALSE], a$y))) # a$y: A version of y (freq here) with known values correct and others zero
    
  }
  
  
  
  if (!is.null(minVal)) {
    f.con <-
      rbind(f.con, AsMatrix(t(x[!a$yKnown, c(primary3, secondary3), drop = FALSE])))
    f.dir <-
      c(f.dir, rep(">=", length(primary3) + length(secondary3)))
    f.rhs <-
      c(f.rhs, rep(minVal, length(primary3) + length(secondary3)))
  }
  
  # To prevent "lpSolve" and possibly other packages 
  # that require non-empty rows from failing.
  zero_row_f.con <- SSBtools:::zero_col(f.con, rows = TRUE)
  if (any(zero_row_f.con)) {
    # A test could be added to check whether max(abs(f.rhs[zero_row_f.con])) is 0.
    # Precision errors must be taken into account (i.e. check if close to 0).
    # Still, there is a higher risk that such a test fails due to numerical issues
    # than that the input itself is actually wrong. In any case, the input data
    # is assumed to be correct.
    f.con <- f.con[!zero_row_f.con, , drop = FALSE]
    f.dir <- f.dir[!zero_row_f.con]
    f.rhs <- f.rhs[!zero_row_f.con]
  }
  
  if (verbose) cat("\n")
  
  if (lpPackage == "lpSolve") {
    if (sparseConstraints) {
      f.con <- As_TsparseMatrix(t(f.con)) # With t() result identical to lpSolve example   
      f.con <- cbind(f.con@j + 1, f.con@i + 1, f.con@x)
    }
    if (verbose) cat("Using lpSolve for intervals...\n")
  } 
  if (lpPackage == "Rsymphony" | lpPackage == "highs") {
    if (sparseConstraints) {
      f.con <- As_dgCMatrix(f.con) 
    }
    if (verbose) cat("Using", lpPackage ,"for intervals...\n")
  }
  
  if (lpPackage == "Rglpk") {
    if (sparseConstraints) {
      f.con <- As_TsparseMatrix(f.con)  
      f.con <-  slam::simple_triplet_matrix(   # Rglpk depends on slam
        f.con@i + 1L, f.con@j + 1L, f.con@x, nrow = nrow(f.con), ncol = ncol(f.con))
    }
    if (verbose) cat("Using Rglpk for intervals...\n")
  }
  
  if (lpPackage == "lpSolve") {
    if (!sparseConstraints) PackageVal = LpVal
    if (sparseConstraints) PackageVal = LpVal_sparse
  } 
  if (lpPackage == "Rsymphony") PackageVal = RsymphVal
  if (lpPackage == "Rglpk") PackageVal = RglpkVal
  if (lpPackage == "highs") PackageVal = highsVal
  
  
  for (j in which(check)) {  # for (j in seq_along(primary3)) {
    if (verbose) if (j %% max(1, length(primary3) %/% 50) == 0) {
      cat("-")
      flush.console()
    }
    i <- primary3[j]
    flush.console()
    f.obj <- as.vector(x[!a$yKnown, i])
    
    lo[i] <- PackageVal(
      max = FALSE,
      obj = f.obj,
      mat = f.con,
      dir = f.dir,
      rhs = f.rhs,
      types = c("C", "I")[1 + allInt]
    )
    up[i] <- PackageVal(
      max = TRUE,
      obj = f.obj,
      mat = f.con,
      dir = f.dir,
      rhs = f.rhs,
      types = c("C", "I")[1 + allInt]
    )
    
}

# Add values "removed" by Reduce0exact
# a$y: A version of y (freq here) with known values correct and others zero
addKnown <- as.vector(Matrix::crossprod(x, a$y))
lo <- lo + addKnown
up <- up + addKnown

list(lo = lo, up = up)
}


LpVal <- function(max, obj, mat, dir, rhs, types) {
    lpobj <- lpSolve::lp(direction = c("min", "max")[1 + max],
                         objective.in = obj,
                         const.mat = mat,
                         const.dir = dir,
                         const.rhs = rhs,
                         all.int = types == "I")  
  c(lpobj$objval, NA, NaN,-Inf)[lpobj$status + 1]
}


LpVal_sparse <- function(max, obj, mat, dir, rhs, types) {
  lpobj <- lpSolve::lp(direction = c("min", "max")[1 + max],
                       objective.in = obj,
                       dense.const = mat,
                       const.dir = dir,
                       const.rhs = rhs,
                       all.int = types == "I")  
  c(lpobj$objval, NA, NaN,-Inf)[lpobj$status + 1]
}


RsymphVal <- function(...) {
  lpobj <- Rsymphony::Rsymphony_solve_LP(...)
  c(lpobj$objval, NA)[lpobj$status + 1]
}



RglpkVal <- function(...) {
  
  lpobj <- Rglpk::Rglpk_solve_LP(...)
  
  # This part written by ChatGPT
  if(lpobj$status == 0) {  # Optimal solution found
    output <- lpobj$optimum
  } else if(lpobj$status == 2 || lpobj$status == 3) {  # Infinite solution or no feasible solution
    output <- Inf
  } else {  # Other errors or situations
    output <- NA
  }
  output
}


highsVal <- function(max, obj, mat, dir, rhs, types) {

  L <- obj
  
  lower <- rep(0, length(L))  
  upper <- rep(Inf, length(L))  
  
  A <- mat
  
  rhs <- rhs            
  lhs <- rhs
  rhs[dir == ">="] = Inf
  lhs[dir == "<="] = -Inf
  
  maximum <- max
  
  if(length(types) == 1){
    types <- rep(types, length(L))
  }
  
  solution <- highs::highs_solve(
    Q = NULL,  
    L = L,
    lower = lower,
    upper = upper,
    A = A,
    lhs = lhs,
    rhs = rhs,
    types = types,
    maximum = maximum
  )
  
  if(solution$status_message == "Optimal") {  
    output <- solution$objective_value  
  } else {
    output <- NA  
  }
  output
}


# As_dgCMatrix(matrix(c(1, 2, 2, 1), nrow = 2))
# As_dgCMatrix(Matrix(c(1, 2, 2, 1), nrow = 2))
As_dgCMatrix = function(x){
  as(drop0(x),  "generalMatrix")
}


