

#' @rdname SuppressDirectDisclosure
#' @importFrom Matrix colSums crossprod
#' @importFrom methods as
#' @export
SuppressDirectDisclosure2 <- function(data,
                                      dimVar = NULL,
                                      freqVar,
                                      coalition = 1,
                                      secondaryZeros = coalition,
                                      candidates = DirectDisclosureCandidates,
                                      ...) {
    if (hasArg("primaryDims"))
      warning('Parameter "primaryDims" is not supported by SuppressDirectDisclosure2.')
    if (hasArg("unknowns"))
      warning('Parameter "unknowns" is not supported by SuppressDirectDisclosure2.')
    if (hasArg("unknown.threshold"))
      warning('Parameter "unknown.threshold"  is not supported by SuppressDirectDisclosure2.')
    if (hasArg("suppressSmallCells"))
      warning('Parameter "suppressSmallCells" is not supported by SuppressDirectDisclosure2.')
    if (is.logical(secondaryZeros)) {
      if (secondaryZeros)
        secondaryZeros <- coalition
      else
        secondaryZeros <- 0
    }
    GaussSuppressionFromData(data,
                             dimVar,
                             freqVar,
                             primary = FindDisclosiveCells2,
                             protectZeros = FALSE,
                             secondaryZeros = secondaryZeros,
                             coalition = coalition,
                             candidates = candidates,
                             ...)
  }



FindDisclosiveCells2 <- function(x, freq, coalition = 1, ...) {
  # t(x) %*% x on triplet form (via dgeMatrix needed)
  k <- as(as(crossprod(x), "dgCMatrix"), "dgTMatrix")
  
  colSums_x <- colSums(x)
  
  # hierarchical relation where the column is the child of the row
  r <- colSums_x[k@j + 1] == k@x & colSums_x[k@i + 1] != k@x
  i <- k@i[r] + 1
  j <- k@j[r] + 1
  
  # i, j are candidate pairs of relationships
  a <- cbind(i = i,
             j = j,
             diff = freq[i] - freq[j])
  
  # remove relationships where child has 0 frequency
  a <- a[freq[j] > 0,]
  
  # children with distance to parent <= coalition
  unique(a[a[, 3] <= coalition, 2])
}

# function for hierarchies and unknowns. Largely untested. Current shortcomings:
# User MUST provide primaryDimList containing hierarchical breakdown of all
# categorical variables. Can provide multiple dimlists per variable (IN THEORY,
# DOES NOT CURRENTLY WORK). This is necessary, since automatic extraction of a
# hierarchy from an arbitrary ModelMatrix is NP-hard, so infeasible in practice.
FindDisclosiveCells3 <- function(x,
                                 ct,
                                 primaryDimList,
                                 freq,
                                 unknowns,
                                 coalition = 1) {
    k <- crossprod(x)
    k <- as(k, "dgTMatrix")
    colSums_x <- colSums(x)
    # row i is child of column j in r
    r <- colSums_x[k@i + 1] == k@x & colSums_x[k@j + 1] != k@x
    k@x <- k@x[r]
    k@j <- k@j[r]
    k@i <- k@i[r]
    reducedk <- TransitiveReduction(k)
    
    # named list (by variable/hierarchy) containing populated unknown cells
    unks <-
      sapply(names(ct), function(y)
        which(ct[, y] %in% unknowns[[y]]))
    unks <-
      sapply(names(unks), function(y)
        unks[[y]][freq[unks[[y]]] > 0])
    
    child_parent <- cbind(child = reducedk@i + 1,
                          parent = reducedk@j + 1,
                          diff = freq[reducedk@j + 1] - freq[reducedk@i + 1])
    # ignore parents that are "unknown"
    child_parent <-
      child_parent[!(child_parent[, 2] %in% unlist(unks)),]
    
    parents <- unique(child_parent[, 2])
    child_sums <- vector(mode = "list", length = nrow(ct))
    # for each parent, a named list (for each variable/hierarchy) of child sums
    # that equals parent
    child_sums[parents] <- lapply(parents,
                                  function(y)
                                    find_sub_sums(y, ct, x, primaryDimList))
    
    primary <- rep(FALSE, nrow(ct))
    for (parent in parents) {
      for (name in names(child_sums[[parent]])) {
        sums <- child_sums[[parent]][[name]]
        if (!length(sums))
          next
        else if (any(sums %in% unks[[name]])) {
          # remove child parent relationship for sums that contain populated
          # unknown child, since otherwise we suppress disclosure of unknown
          child_parent <- child_parent[!(child_parent[, 1] %in% sums &
                                           child_parent[, 2]  == parent),]
        }
        else {
          dominant_children <- child_parent[, 1][child_parent[, 1] %in% sums &
                                                 child_parent[, 2] == parent &
                                                 child_parent[, 3] <= coalition &
                                                 freq[child_parent[, 1]]]
          primary[dominant_children] <- TRUE
          
        }
      }
    }
    primary
}

# computes transitive reduction of a graph represented by a dgTMatrix
TransitiveReduction <- function(matrix) {
  vorder <- order(colSums(matrix), decreasing = TRUE)-1
  for (node in vorder) {
    for (neigh1 in matrix@i[matrix@j == node & matrix@x != 0]) {
      reductive <- ModelMatrixDFS(neigh1, matrix)
      matrix@x[matrix@i %in% reductive & matrix@j == node] <- 0
    }
  }
  ind <- matrix@x != 0
  matrix@x <- matrix@x[ind]
  matrix@j <- matrix@j[ind]
  matrix@i <- matrix@i[ind]
  matrix
}

# depth first search from node in graph represented by dgTMatrix
ModelMatrixDFS <- function(node, matrix) {
  reachable <- NULL
  for (neighbor in matrix@i[matrix@j == node]) {
    reachable <- unique(c(reachable, neighbor, ModelMatrixDFS(neighbor, matrix)))
  }
  reachable
}

# given a published parent cell, return all combinations of child cells that sum
# to parent
find_sub_sums <- function(parent,
                          ct,
                          x,
                          dimlists) {
  res <- list()
  parent_row <- ct[parent,]
  for (var in names(ct)) {
    other.vars <- names(ct)[names(ct) != var]
    vardls <- which(!is.na(match(names(dimlists), var)))
    for (dl in vardls) {
      dimlist <- dimlists[[dl]]
      subcats <- find_sub_category(dimlist, parent_row[[var]])
      matches <-
        which(!is.na(SSBtools::Match(ct[other.vars], parent_row[other.vars])))
      matches <- matches[which(ct[matches, var] %in% subcats)]
      if (!length(matches))
        matches <- NULL
      add <- list(matches)
      names(add) <- var
      res <- append(res, add)
    }
  }
  res
}

# given a category and a dimlist, returns all subcategories
find_sub_category <- function(dimlist, 
                              category) {
  row <- which(dimlist$codes == category)
  if (row == nrow(dimlist))
    return(NULL)
  else if (nchar(dimlist$levels[row]) > nchar(dimlist$levels[row + 1]))
    return(NULL)
  same_level <-
    which(!is.na(match(dimlist$levels, dimlist$levels[row])))
  i1 <- match(row, same_level)
  from <- same_level[i1] + 1
  if (i1 < length(same_level))
    to <- same_level[i1 + 1] - 1
  else
    to <- nrow(dimlist)
  if (from <= to)
    return(dimlist$codes[seq(from = from, to = to)])
  else
    NULL
}