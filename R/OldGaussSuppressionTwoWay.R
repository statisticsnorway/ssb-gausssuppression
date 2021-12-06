
#' @rdname GaussSuppressionTwoWay
#' @export
OldGaussSuppressionTwoWay = function(data, dimVar = NULL, freqVar=NULL, numVar = NULL,  weightVar = NULL, charVar = NULL, #  freqVar=NULL, numVar = NULL, name
                                    hierarchies, formula = NULL,
                           maxN = 3, 
                           protectZeros = TRUE, 
                           secondaryZeros = FALSE,
                           candidates = CandidatesDefault,
                           primary = PrimaryDefault,
                           forced = NULL,                                                             # Parameter not treated yet
                           hidden = NULL,                                                             # Parameter not treated yet
                           singleton = SingletonDefault,                                              # Parameter not treated yet 
                           singletonMethod = ifelse(secondaryZeros, "anySumNOTprimary", "anySum"),    # Parameter not treated yet
                           printInc = TRUE,
                           output = "publish",                                                        
                           preAggregate = is.null(freqVar),
                           colVar = names(hierarchies)[1],
                           removeEmpty = NULL,
                           inputInOutput = TRUE,
                           ...){ 
  
  if (is.null(hierarchies)) {
    stop("Hierarchies must be specified")
  }
  
  if(is.null(removeEmpty)){
    removeEmpty_in_x <- TRUE
    removeEmpty <- FALSE
  } else {
    removeEmpty_in_x <- removeEmpty 
  }
  
  if(removeEmpty){
    stop("removeEmpty=TRUE is not implementyed. Usen NULL and remove rows where iN_dEx is 0")
  }
  
  
  total = "Total"
  hierarchies <- AutoHierarchies(hierarchies = hierarchies, data = data, total = total, 
                                     hierarchyVarNames = c(mapsFrom = "mapsFrom", mapsTo = "mapsTo", sign = "sign", level = "level"))
  
  if(!(output %in% c("publish", "inner", "publish_inner", "publish_inner_x", "publish_x", "inner_x", "input2functions" )))
    stop('Allowed values of parameter output are "publish", "inner", "publish_inner", "publish_inner_x", "publish_x", "inner_x", and "input2functions".')
  
  
  innerReturn <- output %in% c("inner", "publish_inner", "publish_inner_x", "inner_x")

  force(preAggregate)
  
  dimVar <- names(data[1, dimVar, drop = FALSE])
  freqVar <- names(data[1, freqVar, drop = FALSE])
  numVar <- names(data[1, numVar, drop = FALSE])
  weightVar <- names(data[1, weightVar, drop = FALSE])
  charVar <- names(data[1, charVar, drop = FALSE])
  
  if (preAggregate | innerReturn | (is.null(hierarchies) & is.null(formula) & !length(dimVar))) {
    if (printInc & preAggregate) {
      cat("[preAggregate ", dim(data)[1], "*", dim(data)[2], "->", sep = "")
      flush.console()
    }
    if (!is.null(hierarchies)) {
      dVar <- names(hierarchies)
    } else {
      if (!is.null(formula)) {
        dVar <- row.names(attr(delete.response(terms(as.formula(formula))), "factors"))
      } else {
        if (length(dimVar)){
          dVar <- dimVar
        } else {
          freqPlusVarName <- c(freqVar, numVar, weightVar, charVar)
          if (!length(freqPlusVarName)){
            dVar <- names(data)
          } else {
            dVar <- names(data[1, !(names(data) %in% freqPlusVarName), drop = FALSE])
          }
        }
      }
    }
    dVar <- unique(dVar)
    
    if (!length(dimVar)){
      dimVar <- dVar
    }
    
    if (preAggregate) {
      if (!length(freqVar)) {
        if ("freq" %in% names(data)) {
          freqVar <- "f_Re_qVa_r"
        } else {
          freqVar <- "freq"
        }
        data[[freqVar]] <- 1L # entire data.frame is copied into memory when adding 1s. Improve?  
      } 
      data <- aggregate(data[unique(c(freqVar, numVar, weightVar))], data[unique(c(dVar, charVar))], sum)
      if (printInc) {
        cat(dim(data)[1], "*", dim(data)[2], "]\n", sep = "")
        flush.console()
      }
    } else {
      data <- data[unique(c(dVar, charVar, freqVar, numVar, weightVar))]
    }
  }
  
  if(innerReturn){
    attr(data, "freqVar") <- freqVar
  }
  

  if (output == "inner") {
    return(data)
  }
  
  # New code starts from here
  
  
  rowVar <- names(hierarchies)[!(names(hierarchies) %in% colVar)]
  
  
  # Trick with index-input
  data$iN_dEx <- seq_len(nrow(data))
  
  # Two HierarchyCompute runs. 
  
  # matrixComponents output with "index"
  hc1 <- HierarchyCompute(data, hierarchies = hierarchies, valueVar = "iN_dEx", colVar = colVar, output = "matrixComponents", inputInOutput = inputInOutput, reduceData = removeEmpty_in_x)
  
  if( !all(range(diff(sort(as(hc1$hcRow$valueMatrix,"dgTMatrix")@x))) == c(1L, 1L))){
    stop("Index method failed. Duplicated combinations?")
  }
  
  
  # All numerical variables including "index"
  hc2 <- HierarchyCompute(data, hierarchies = hierarchies, valueVar = c("iN_dEx", freqVar, numVar, weightVar), colVar = colVar, inputInOutput = inputInOutput, reduceData = removeEmpty_in_x)
  
  
  if (is.function(primary) | is.list(primary))  
    primary <-     Primary(primary = primary, 
                           crossTable = hc2[names(hierarchies)], # x = x,    ## x not possible here
                           freq = hc2[[freqVar]], 
                           num = hc2[numVar], 
                           weight = hc2[[weightVar]], 
                           maxN = maxN, protectZeros = protectZeros, secondaryZeros = secondaryZeros, data = data, freqVar = freqVar, numVar = numVar, weightVar = weightVar, charVar = charVar, dimVar = dimVar, ...)
  
  
  totalRow <- which.max(rowSums(hc1$hcRow$dataDummyHierarchy))
  totalCol <- which.max(rowSums(hc1$hcCol$dataDummyHierarchy))
  
  
  nRowOutput <- nrow(hc1$hcRow$dataDummyHierarchy)
  nColOutput <- nrow(hc1$hcCol$dataDummyHierarchy)
  
  idxTotalCol <- seq_len(nRowOutput) + (nRowOutput * (totalCol - 1))
  idxTotalRow <- totalRow + (seq_len(nColOutput) - 1) * nRowOutput
  
  value_dgT <- as(hc1$hcRow$valueMatrix, "dgTMatrix")
  
  #data[value_dgT@x[match(unique(value_dgT@j), value_dgT@j)], unique(colVar), drop = FALSE]
  
  
  dataRow <- aggregate(data[unique(c(freqVar, numVar, weightVar))], data[rowVar], sum)
  ma <- Match(dataRow[rowVar], hc1$hcRow$fromCrossCode)
  if( any(range(diff(sort(ma))) != c(1L, 1L)) ){
    stop("Matching failed")
  }
  dataRow <- dataRow[ ma, , drop = FALSE]
  
  dataCol <- aggregate(data[unique(c(freqVar, numVar, weightVar))], data[colVar], sum)
  ma <- Match(dataCol[colVar], data[value_dgT@x[match(unique(value_dgT@j), value_dgT@j)], colVar, drop = FALSE])
  if( any(range(diff(sort(ma))) != c(1L, 1L)) ){
    stop("Matching failed")
  }
  dataCol <- dataCol[ ma, , drop = FALSE]
  
  
  
  xRow <- t(hc1$hcRow$dataDummyHierarchy)
  xCol <- t(hc1$hcCol$dataDummyHierarchy)
  
  
  if (!length(freqVar)) {
    freqRow <- NULL
    freqCol <- NULL
  } else {
    freqRow <- hc2[idxTotalCol, freqVar, drop = TRUE]
    freqCol <- hc2[idxTotalRow, freqVar, drop = TRUE]
  }
  
  if (!length(numVar)) {
    numRow <- NULL
    numCol <- NULL
  } else {
    numRow <- hc2[idxTotalCol, numVar, drop = FALSE]
    numCol <- hc2[idxTotalRow, numVar, drop = FALSE]
  }
  
  
  if (!length(weightVar)) {
    weightRow <- NULL
    weightCol <- NULL
  } else {
    weightRow <- hc2[idxTotalCol, weightVar, drop = TRUE]
    weightCol <- hc2[idxTotalRow, weightVar, drop = TRUE]
  }
  
  
  
  if (is.function(candidates)){ # An alternative is two functions as input
    candidatesROW <-  candidates(crossTable = hc2[idxTotalCol, rowVar , drop = FALSE], 
                                 x = xRow, freq = freqRow, num = numRow, weight = weightRow, maxN = maxN, protectZeros = protectZeros, secondaryZeros = secondaryZeros, 
                                 data = dataRow, freqVar = freqVar, numVar = numVar, weightVar = weightVar, charVar = charVar, dimVar = dimVar, ...)
    candidatesCol <-  candidates(crossTable = hc2[idxTotalRow, colVar , drop = FALSE], 
                                 x = xCol, freq = freqCol, num = numCol, weight = weightCol, maxN = maxN, protectZeros = protectZeros, secondaryZeros = secondaryZeros, 
                                 data = dataCol, freqVar = freqVar, numVar = numVar, weightVar = weightVar, charVar = charVar, dimVar = dimVar, ...)
    
  }
  
  
  if (is.list(primary)) {
    numExtra <- primary[[2]]
    primary <- primary[[1]]
  } else {
    numExtra <- matrix(0, nrow(hc2), 0)
  }
  
  supprMatrix <- matrix(primary, ncol = nColOutput)
  
  supprSumCol_old <- rowSums(supprMatrix)
  supprSumRow <- colSums(supprMatrix)
  supprSumRow_old <- 0L * supprSumRow
  
  # t(xRow) %*% hc1$hcRow$valueMatrix %*%  xCol
  
  # When TRUE: Output corresponding to zero data rows in input will never be secondary suppressed  
  
  xRow_i <- xRow
  xCol_i <- xCol
  
  while (sum(supprSumRow) > sum(supprSumRow_old)) {
    
    for (i in seq_len(nColOutput)) {
      if (supprSumRow[i] > supprSumRow_old[i]) {
        
        cat("col", i, ",", supprSumRow[i] - supprSumRow_old[i], "extra : ")
        
        if(removeEmpty_in_x){
          rr <- as.vector(as.matrix(hc1$hcRow$valueMatrix %*%  xCol[, i, drop=FALSE])) > 0
          if(any(!rr)){
            cat("-",sum(!rr)," ", sep="")
          }
          xRow_i <- xRow[rr, ,drop=FALSE] 
        }
        
        secondary <- GaussSuppression(x = xRow_i, candidates = candidatesROW, 
                                      primary = supprMatrix[, i], 
                                      forced = NULL, hidden = NULL, singleton = NULL, singletonMethod = "none",
                                      printInc = printInc, whenEmptySuppressed = NULL, whenEmptyUnsuppressed = NULL, ...)
        
        supprMatrix[secondary, i] <- TRUE
      }
    }
    
    supprSumRow_old <- colSums(supprMatrix)
    supprSumCol <- rowSums(supprMatrix)
    
    
    for (i in seq_len(nRowOutput)) {
      if (supprSumCol[i] > supprSumCol_old[i]) {
        
        cat("row", i, ",", supprSumCol[i] - supprSumCol_old[i], "extra : ")
        
        if(removeEmpty_in_x){
          rr <- as.vector(as.matrix(t(hc1$hcRow$valueMatrix) %*%  xRow[, i, drop=FALSE])) > 0
          if(any(!rr)){
            cat("-",sum(!rr)," ", sep="")
          }
          xCol_i <- xCol[rr, ,drop=FALSE] 
        }
        
        secondary <- GaussSuppression(x = xCol_i, candidates = candidatesCol, 
                                      primary = supprMatrix[i, ], 
                                      forced = NULL, hidden = NULL, singleton = NULL, singletonMethod = "none",
                                      printInc = printInc, whenEmptySuppressed = NULL, whenEmptyUnsuppressed = NULL, ...)
        
        supprMatrix[i, secondary] <- TRUE
      }
    }
    
    supprSumCol_old <- rowSums(supprMatrix)
    supprSumRow <- colSums(supprMatrix)
    
  }
  
  
  #list(hc1 = hc1, hc2 = hc2, dataRow = dataRow, dataCol = dataCol, 
  #     freqRow = freqRow, freqCol = freqCol, 
  #     numRow = numRow, numCol = numCol, 
  #     weightRow = weightRow, weightCol = weightCol,
  #     hc2[idxTotalCol, , drop = FALSE],
  #     hc2[idxTotalRow, , drop = FALSE], candidatesROW =candidatesROW, candidatesCol =candidatesCol, primary=primary,  supprMatrix= supprMatrix
  #     )

  if(removeEmpty){
    cbind(hc2, primary = primary, numExtra, suppressed = as.vector(supprMatrix))
  }
  
  
  cbind(hc2, primary = primary, numExtra, suppressed = as.vector(supprMatrix))
  
  
}


