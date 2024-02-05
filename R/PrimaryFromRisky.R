
#' Additional primary cells based on risky primary cells
#' 
#' The algorithm uses parent-child relationships found from the model matrix (`x`)
#'
#' @param x The model matrix
#' @param y A vector of numeric values with a length equal to `nrow(x)`
#' @param risky Indices to columns in `x` corresponding to primary cells classified 
#'              as risky (interval limits not reached)
#' @param candidates Indices to columns in `x` that are candidates for becoming 
#'                   additional primary cells. Higher order cells must be included 
#'                   so that parent-child relationships are seen.
#' @param allDims When TRUE, a primary cell is added for each dimension.  
#'                can be specified as a vector of length  `length(risky)`               
#'
#' @details
#' 
#' For a single `risky` cell,  the algorithm can be formulated as:
#' * Consider this cell as a `child` and identify all `parents` that are present in `candidates`.
#' * Remove parents who are also parents of other parents (i.e., eliminate higher-level parents).
#' * Identify the children of these remaining parents that are included in `candidates`.
#' * Select the child that has the smallest value in the numeric variable (`y`).
#' 
#' For several `risky` cells, coordination takes place. See the comment below the examples.
#' 
#' @return Additional primary cells as indices to columns in `x`.
#' @export
#' 
#' @examples
#' 
#' # Example inspired by suppression with maxN = 5
#' d1 <- SSBtoolsData("d1")
#' mm <- SSBtools::ModelMatrix(d1, dimVar = 1:2, crossTable = TRUE)
#' x <- mm$modelMatrix
#' y <- t(x) %*% d1$freq
#' 
#' risky <- c(13, 15, 40, 45)
#' candidates <- c(1:12, 14, 16, 17, 19, 21, 21, 24, 26:37, 39, 42, 44)
#' 
#' info <- rep("", length(y))
#' info[risky ] <- "risky"
#' info[candidates] <- "c"
#' cbind(mm$crossTable, y=as.vector(y), info)
#' 
#' PrimaryFromRiskyDefault(x = x, y = y, risky = risky, candidates = candidates)
#' PrimaryFromRiskyDefault(x = x, y = y, risky = 40, candidates = candidates)
#' 
#' # The last solution (39) is not included in the first (28, 35). 
#' # This is because 39 is not needed when 35 is already included.
#' 
PrimaryFromRiskyDefault <- function(x, y, risky, candidates, allDims = FALSE) {
  
  allDims = rep_len(allDims, length(risky))
  
  pc1 <- FindParentChildInd(x, risky, candidates)
  pc1 <- SortRows(pc1)
  
  parentCandidates <- unique(pc1$parent)
  
  pc2 <- FindParentChildInd(x, candidates, parentCandidates)
  
  if (!nrow(pc2)) {
    return(parentCandidates)  # no children 
  }
  
  pc2 <- cbind(y = y[pc2$child], pc2)
  pc2 <- SortRows(pc2)
  
  newPrimary <- integer(0)
  for (i in seq_along(risky)) {
    newPrimary <- c(newPrimary,    # rev to keep just the one with larges y. Only used to order  
                    rev(SingleNewPrimary(pc2[pc2$parent %in% pc1$parent[pc1$child %in% risky[i]], , drop = FALSE],
                                         allDims = allDims[i]))[1])
  }
  
  # reorder risky
  ord <- order(y[newPrimary], decreasing = TRUE)
  risky <- risky[ord]
  
  
  # New run after new order. This time using the `previously` parameter.  
  # Here, 39 in the example is excluded.
  newPrimary <- integer(0)
  for (i in seq_along(risky)) {
    newPrimary <- c(newPrimary, 
                    SingleNewPrimary(pc2[pc2$parent %in% pc1$parent[pc1$child %in% risky[i]], , drop = FALSE], 
                                     previously = newPrimary, allDims = allDims[i]))
  }
  
  if (anyDuplicated(newPrimary)) {
    warning("Bug in algorithm")
  }
  
  sort(newPrimary)
}



SingleNewPrimary <- function(..., allDims = FALSE) {
  if(allDims){
    return(SingleNewPrimaryAllDims(...))
  } 
  SingleNewPrimarySingleDim(...)
}


SingleNewPrimarySingleDim <- function(pc2, previously = integer(0)) {
  child_in_parent <- pc2$child[pc2$child %in% pc2$parent]
  grandparent <- unique(pc2$parent[pc2$child %in% child_in_parent])
  pc2 <- pc2[!(pc2$parent %in% grandparent), , drop = FALSE]
  if (any(pc2$child %in% previously)) {
    return(integer(0))  # If a solution is found in previously
  }
  pc2$child[1]
}



SingleNewPrimaryAllDims <- function(pc2, previously = integer(0)) {
  child_in_parent <- pc2$child[pc2$child %in% pc2$parent]
  grandparent <- unique(pc2$parent[pc2$child %in% child_in_parent])
  pc2 <- pc2[!(pc2$parent %in% grandparent), , drop = FALSE]
  child_in_previously <- pc2$child %in% previously
  if (any(child_in_previously)) {
    parent_in_previously <- unique(pc2$parent[child_in_previously])
    pc2 <- pc2[!(pc2$parent %in% parent_in_previously), , drop = FALSE]
    # cat('\n pc2: \n') print(pc2)
    if (!nrow(pc2)) {
      return(integer(0))
    }
  }
  ma <- match(unique(pc2$parent), pc2$parent)
  pc2 <- pc2[ma, , drop = FALSE]
  unique(pc2$child)
}



# Similar to part of GaussSuppression:::FindDifferenceCells
# and SSBtools:::FindDiffMatrix
FindParentChild <- function(x, y = x) {
  xty <- As_TsparseMatrix(crossprod(x, y))
  colSums_y_xty_j_1 <- colSums(y)[xty@j + 1]
  # finds children in x and parents in y
  r <- colSums(x)[xty@i + 1] == xty@x & 
    colSums_y_xty_j_1     != xty@x 
  data.frame(child = xty@i[r] + 1L, parent = xty@j[r] + 1L)
}


FindParentChildInd <- function(x, childInd, parentInd) {
  pc <- FindParentChild(x[, childInd, drop = FALSE], x[, parentInd, drop = FALSE])
  data.frame(child = childInd[pc$child], parent = parentInd[pc$parent])
}

