
#' @rdname SuppressDirectDisclosure
#' @importFrom Matrix colSums crossprod
#' @importFrom methods as 
#' @export
SuppressDirectDisclosure2 <- function(data, dimVar = NULL, freqVar, coalition = 1, 
                                      secondaryZeros = coalition,
                                      candidates = DirectDisclosureCandidates, ...){
  
  if (hasArg("primaryDims"))        warning('Parameter "primaryDims" is not supported by SuppressDirectDisclosure2.')
  if (hasArg("unknowns"))           warning('Parameter "unknowns" is not supported by SuppressDirectDisclosure2.')
  if (hasArg("unknown.threshold"))  warning('Parameter "unknown.threshold"  is not supported by SuppressDirectDisclosure2.')
  if (hasArg("suppressSmallCells")) warning('Parameter "suppressSmallCells" is not supported by SuppressDirectDisclosure2.')
  if (is.logical(secondaryZeros)) {
    if (secondaryZeros) secondaryZeros <- coalition
  else secondaryZeros <- 0
  }
  GaussSuppressionFromData(data, dimVar, freqVar, 
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
  a <- cbind(i = i, j = j, diff = freq[i] - freq[j])
  
  # remove relationships where child has 0 frequency
  a <- a[freq[j] > 0, ]
  
  # children with distance to parent <= coalition
  unique(a[a[, 3] <= coalition, 2])
}
