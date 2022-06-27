


#' Possible `mc_function` to be used within `KDisclosurePrimary`
#' 
#' 
#' @inheritParams SuppressKDisclosure
#' @inheritParams DominanceRule
#'
#' @param removeIncomplete When `TRUE`,  the input code contributions are checked and incomplete entries are removed.
#' @param returnNewCrossTable When `TRUE`, the crossTable corresponding to the created x-matrix is also returned (in a list)
#' 
#' @keywords internal
#' @export
#'
X_from_mc <- function(x, crossTable, mc_hierarchies, removeIncomplete = FALSE, returnNewCrossTable = FALSE, ...) {
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
  
  if (returnNewCrossTable | removeIncomplete) {
    mm <- ModelMatrix(data = crossTable, hierarchies = mcHier, removeEmpty = TRUE, crossTable = TRUE)
    x2 <- mm$modelMatrix
    crossTable2 <- mm$crossTable
    rm(mm)
  } else {
    x2 <- ModelMatrix(data = crossTable, hierarchies = mcHier, removeEmpty = TRUE)
    crossTable2 <- NULL
  }
  
  # colSums(x2) == 1 means copy from x
  colSums_x2_1 <- colSums(x2) > 1
  x2 <- x2[, colSums_x2_1, drop = FALSE]
  crossTable2 <- crossTable2[colSums_x2_1, , drop = FALSE]
  
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

