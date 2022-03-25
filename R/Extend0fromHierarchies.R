Extend0fromHierarchies <- function(data, freqName, hierarchies, dimVar, extend0all, ...) {
  
  hierarchies <- AutoHierarchies(hierarchies = hierarchies, data = data, ...)
  dVar <- dimVar  # as in code before separate function
  dVar_ <- names(hierarchies)
  
  if (extend0all) {
    varGroups <- as.list(dVar_)  # This is standard in Extend0 when !hierarchical
    for (i in seq_along(varGroups)) {
      varGroups[[i]] <- unique(data[varGroups[[i]]])  # This is standard in Extend0
      mapsFrom <- unique(hierarchies[[dVar_[i]]]$mapsFrom)
      mapsTo <- unique(hierarchies[[dVar_[i]]]$mapsTo)
      mapsExtra <- mapsFrom[!(mapsFrom %in% mapsTo)]
      mapsExtra <- mapsExtra[!(mapsExtra %in% varGroups[[i]][[1]])]
      if (length(mapsExtra)) {
        extra_varGroups_i <- varGroups[[i]][rep(1, length(mapsExtra)), , drop = FALSE]
        extra_varGroups_i[[1]] <- mapsExtra
        varGroups[[i]] <- rbind(varGroups[[i]], extra_varGroups_i)
      }
    }
  } else {
    varGroups <- NULL
  }
  
  nrowPreExtend0 <- nrow(data)
  
  data <- Extend0(data, freqName = freqName, dimVar = dVar_, varGroups = varGroups, extraVar = TRUE, hierarchical = FALSE)
  
  # Set to NA instead of 0 for possible numeric dimVar not in hierarchy after AutoHierarchies (above)
  if (length(dVar_) < length(dVar)) {
    extra_dVar <- dVar[!(dVar %in% dVar_)]
    extra_dVar <- extra_dVar[sapply(data[1, extra_dVar], is.numeric)]
    if (length(extra_dVar)) {
      newrows <- SeqInc(nrowPreExtend0 + 1L, nrow(data))
      if (length(newrows)) {
        data[newrows, extra_dVar] <- NA
      }
    }
  }
  list(data = data, hierarchies = hierarchies)
}
