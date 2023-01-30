


#' Function to be used within `KDisclosurePrimary`
#' 
#' 
#' @inheritParams SuppressKDisclosure
#' @inheritParams DominanceRule
#'
#' @param removeIncomplete When `TRUE`,  the input code contributions are checked and incomplete entries are removed.
#' @param returnNewCrossTable When `TRUE`, the crossTable corresponding to the created x-matrix is also returned (in a list)
#' @param noInner When `TRUE`,  more efficient generation of model matrix.  (removing inner cells according to colSums not needed)
#' 
#' @author Øyvind Langsrud
#' @keywords internal
#' @export
#'
X_from_mc <- function(x, crossTable, mc_hierarchies, removeIncomplete = FALSE, returnNewCrossTable = FALSE, noInner = FALSE, ...) {
  if (is.null(mc_hierarchies))
    return(NULL)
  
  mcHier <- AutoHierarchies(mc_hierarchies, data = crossTable)
  dimVar <- names(crossTable)
  
  # Removes rows where mapsTo codes are in crossTable
  # Sets missing or empty (after removal) dimensions to "rowFactor"
  for (i in seq_along(dimVar)) {
    if (is.null(mcHier[[dimVar[i]]])) {
      mcHier[[dimVar[i]]] <- "rowFactor"
    } else {
      rem <- mcHier[[dimVar[i]]]$mapsTo %in% unique(crossTable[[dimVar[i]]])
      mcHier[[dimVar[i]]] <- mcHier[[dimVar[i]]][!rem, , drop = FALSE]
      if (!nrow(mcHier[[dimVar[i]]])) {
        mcHier[[dimVar[i]]] <- "rowFactor"
      }
    }
  }
  
  if(noInner){
    ModelMatrix_here <- ModelMatrixNoInner
  } else {
    ModelMatrix_here <- ModelMatrix
  }
  
  if (returnNewCrossTable | removeIncomplete) {
    mm <- ModelMatrix_here(data = crossTable, hierarchies = mcHier, removeEmpty = TRUE, crossTable = TRUE)
    x2 <- mm$modelMatrix
    crossTable2 <- mm$crossTable
    rm(mm)
  } else {
    x2 <- ModelMatrix_here(data = crossTable, hierarchies = mcHier, removeEmpty = TRUE, crossTable = FALSE)
    crossTable2 <- NULL
  }
  
  if(!noInner){   # With noInner this can be avoided. Then removing "not-inner 1-cells" is avoided.
    # colSums(x2) == 1 means copy from x
    colSums_x2_1 <- colSums(x2) > 1
    x2 <- x2[, colSums_x2_1, drop = FALSE]
    crossTable2 <- crossTable2[colSums_x2_1, , drop = FALSE]
  }
  
  if (removeIncomplete) {
    hc <- HierarchyContributors(data = crossTable, x = x2, crossTable = crossTable2, hierarchies = mcHier, inputInOutput = TRUE)
    ok_min_max <- rowSums(hc$min != hc$max) == 0
    ok_ac_n <- rowSums(hc$ac != hc$n) == 0
    if (any(!ok_min_max)) {
      message("min_max elimination")
      # print(cbind(crossTable2, hc$max - hc$min)[!ok_min_max, , drop = FALSE])
    }
    if (any(!ok_ac_n)) {
      message("ac_n elimination")
      # print(cbind(crossTable2, hc$ac - hc$n)[!ok_ac_n, , drop = FALSE])
    }
    x2 <- x2[, ok_min_max & ok_ac_n, drop = FALSE]
    crossTable2 <- crossTable2[ok_min_max & ok_ac_n, , drop = FALSE]
  }
  
  # the trick here
  cx <- x %*% x2
  
  cx <- cx[, colSums(cx) != 0, drop = FALSE]
  
  if (returnNewCrossTable) {
    return(list(x = cx, crossTable = crossTable2))
  }
  cx
}

#' @rdname X_from_mc
#' @export
X_from_mc_remove <- function(..., removeIncomplete = TRUE) {
  X_from_mc(..., removeIncomplete = removeIncomplete)
}  

#' @rdname X_from_mc
#' @export
X_from_mc_noinner <- function(..., noInner = TRUE) {
  X_from_mc(..., noInner = noInner)
}


#' @rdname X_from_mc
#' @export
X_from_mc_remove_noinner <- function(..., removeIncomplete = TRUE, noInner = TRUE) {
  X_from_mc(..., removeIncomplete = removeIncomplete, noInner = noInner)
} 


ModelMatrixNoInner <- function(data, hierarchies, removeEmpty, crossTable, 
                               init_modelMatrix = Matrix::Matrix(0, nrow(data), 0), init_crossTable = as.data.frame(matrix(0, ncol = length(hierarchies), nrow = 0, dimnames = list(NULL, names(hierarchies))))) {
  ind_hi <- match(TRUE, !sapply(hierarchies, is.character))
  if (is.na(ind_hi)) {
    if (crossTable) {
      return(list(modelMatrix = init_modelMatrix, crossTable = init_crossTable))
    } else {
      return(init_modelMatrix)
    }
  }
  inputInOutput <- rep_len(TRUE, length(hierarchies))
  inputInOutput[ind_hi] <- FALSE
  mm <- ModelMatrix(data, hierarchies, removeEmpty = removeEmpty, crossTable = crossTable, inputInOutput = inputInOutput)
  
  if (crossTable) {
    init_modelMatrix <- cbind(init_modelMatrix, mm$modelMatrix)
    init_crossTable <- rbind(init_crossTable, mm$crossTable)
  } else {
    init_modelMatrix <- cbind(init_modelMatrix, mm)
  }
  hierarchies[[ind_hi]] <- "rowFactor"
  ModelMatrixNoInner(data, hierarchies, removeEmpty, crossTable, init_modelMatrix, init_crossTable)
}











