

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
                                 crossTable,
                                 primaryDimList,
                                 freq,
                                 unknowns,
                                 coalition = 1, ...) {
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
      sapply(names(crossTable), function(y)
        which(crossTable[, y] %in% unknowns[[y]]))
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
    child_sums <- vector(mode = "list", length = nrow(crossTable))
    # for each parent, a named list (for each variable/hierarchy) of child sums
    # that equals parent
    child_sums[parents] <- lapply(parents,
                                  function(y)
                                    find_sub_sums(y, crossTable, x, primaryDimList))
    
    primary <- rep(FALSE, nrow(crossTable))
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
# function returning all nodes in tree that are not root or leaf
extract_inner_nodes <- function(dimList) {
  result <- NULL
  for (i in seq_len(nrow(dimList) - 1)) {
    not_total <- nchar(dimList[i, "levels"]) >= 2
    parent <- nchar(dimList[i, "levels"]) < nchar(dimList[i + 1, "levels"])
    if (not_total & parent)
      result <- c(result, dimList[i, "codes"])
  }
  result
}
ExtendModelMatrix <- function(data, mc= NULL, formula = NULL, hierarchies = NULL, ...) {
  mm <- ModelMatrix(data, formula = formula, hierarchies = hierarchies, crossTable = TRUE, ...)
  x <- mm$modelMatrix
  ct <- mm$crossTable
  if (!is.null(mc)){
    mc.mm <- ModelMatrix(data, hierarchies = mc, crossTable=  TRUE)
    mc.mm <<- mc.mm
    mc.x <- mc.mm$modelMatrix[, find_mc_cells(mc.mm$crossTable, mc)]
    mm <- cbind(x, mc.x)
    attr(mm, "mc.index") <- ncol(x) + 1
    return(list(modelMatrix = mm, crossTable = ct))
  }
  mm
}

find_mc_cells <- function(mc.crossTable, mc.dimlist) {
  unique_vars <- unique(names(mc.dimlist))
  mc.labels <- sapply(unique_vars,
                      function(x)
                        extract_inner_nodes(Reduce(rbind,
                                                   mc.dimlist[which(x == names(mc.dimlist))])))
  mc.labels <- mc.labels[sapply(mc.labels, function (x) !is.null(x))]
  # gives all in hierarchy, not only "relevant" cells
  mc.indices <- unique(Reduce(c, sapply(names(mc.labels),
         function(x) which(!is.na(match(mc.crossTable[[x]], mc.labels[[x]]))))))
 #  mc.indices <- 
 # mc.indices
}



FDC <- function(x,
                freq,
                coalition = 1,...) {
  mc.index <- attr(x, "mc.index")
  k <- crossprod(x)
  k <- as(k, "dgTMatrix")
  colSums_x <- colSums(x)
  # row i is child of column j in r
  r <- colSums_x[k@i + 1] == k@x & colSums_x[k@j + 1] != k@x
  k@x <- k@x[r]
  k@j <- k@j[r]
  k@i <- k@i[r]
  # k <- TransitiveReduction(k)
  
  child_parent <- cbind(child = k@i + 1,
                        parent = k@j + 1,
                        diff = freq[k@j + 1] - freq[k@i + 1])
  child_parent <- child_parent[freq[child_parent[,2]] > 0 & freq[child_parent[,1]] > 0,]
  disclosures <- child_parent[child_parent[,3] <= coalition, ]
  disclosures <<- disclosures
  primary_matrix <- as(apply(disclosures, 1, function(row) x[,row[2]] - x[,row[1]]), "dgTMatrix")
  colnames(primary_matrix) <- apply(disclosures[,2:1], 1, function(x) paste(x, collapse = "-"))
  primary_matrix
}