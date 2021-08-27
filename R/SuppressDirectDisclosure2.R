
#' @rdname SuppressDirectDisclosure
#' @importFrom Matrix colSums crossprod
#' @importFrom methods as 
#' @export
SuppressDirectDisclosure2 <- function(data, dimVar, freqVar, coalition = 1, ...){
  GaussSuppressionFromData(data, dimVar, freqVar, 
                           primary = FindDisclosiveCells2,
                           protectZeros = FALSE,
                           secondaryZeros = TRUE,
                           coalition = coalition,
                           ...)
}



FindDisclosiveCells2 <- function(x, freq, coalition = 1, ...) {
  
  # t(x) %*% x on triplet form (via dgeMatrix needed)
  k <- as(as(crossprod(x), "dgeMatrix"), "dgTMatrix")
  
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
