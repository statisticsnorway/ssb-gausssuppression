#' Two-way iteration variant of \code{\link{GaussSuppressionFromData}}  
#' 
#' @description
#' Internally, data is organized in a two-way table. 
#' 
#' Use parameter `colVar` to choose hierarchies for columns (others will be rows). Iterations start by column by column suppression.
#' The algorithm utilizes \code{\link{HierarchyCompute2}}. 
#' 
#' With two-way iterations, larger data can be handled, but there is a residual risk.
#' The method is a special form of linked-table iteration. 
#' Separately, the rows and columns are protected by \code{\link{GaussSuppression}} and they have common suppressed cells.
#' 
#' @details
#' The supplied functions for generating \code{\link{GaussSuppression}} input behave as in \code{\link{GaussSuppressionFromData}} with some exceptions.
#' When `candidatesFromTotal` is `TRUE` (default) the candidate function will be run locally once for rows and once for columns. Each time based on column or row totals.
#' The global x-matrix will only be generated if one of the functions supplied needs it.
#' Non-NULL singleton can only be supplied as a function. This function will be run locally within the algorithm before each call to \code{\link{GaussSuppression}}.  
#' 
#' Note that a difference from `GaussSuppressionFromData` is that parameter `removeEmpty` is set to `TRUE` by default.
#' 
#' Another difference is that duplicated combinations is not allowed. Normally duplicates are avoided by setting `preAggregate` to `TRUE`.
#' When the `charVar` parameter is used, this can still be a problem. See the examples for a possible workaround.
#'
#' @param data 	  Input data as a data frame
#' @param dimVar The main dimensional variables and additional aggregating variables. This parameter can be  useful when hierarchies and formula are unspecified. 
#' @param freqVar A single variable holding counts (name or number).
#' @param numVar  Other numerical variables to be aggregated 
#' @param weightVar weightVar Weights (costs) to be used to order candidates for secondary suppression
#' @param charVar Other variables possibly to be used within the supplied functions
#' @param hierarchies List of hierarchies, which can be converted by \code{\link{AutoHierarchies}}.
#'        Thus, the variables can also be coded by `"rowFactor"` or `""`, which correspond to using the categories in the data.
#' @param formula A model formula
#' @param maxN  Suppression parameter. Default: Cells having counts `<= maxN` are set as primary suppressed. 
#' @param protectZeros Suppression parameter. Default when TRUE: Empty cells (count=0) are set as primary suppressed.  
#' @param secondaryZeros Suppression parameter.
#' @param candidates GaussSuppression input or a function generating it (see details) Default: \code{\link{CandidatesDefault}}
#' @param primary    GaussSuppression input or a function generating it (see details) Default: \code{\link{PrimaryDefault}}
#' @param forced     GaussSuppression input or a function generating it (see details)
#' @param hidden     GaussSuppression input or a function generating it (see details)
#' @param singleton  NULL or a function generating GaussSuppression input (logical vector not possible) Default: \code{\link{SingletonDefault}}
#' @param singletonMethod \code{\link{GaussSuppression}} input
#' @param printInc        \code{\link{GaussSuppression}} input
#' @param output One of `"publish"` (default), `"inner"`.
#'               Here "inner" means input data (possibly pre-aggregated). 
#' @param preAggregate When `TRUE`, the data will be aggregated within the function to an appropriate level. 
#'        This is defined by the dimensional variables according to `dimVar`, `hierarchies` or `formula` and in addition `charVar`.
#' @param  colVar  Hierarchy variables for the column groups (others in row group).  
#' @param  removeEmpty	When TRUE (default) empty output corresponding to empty input is removed. 
#'                      When NULL, removal only within the algorithm (x  matrices) so that such empty outputs are never secondary suppressed.
#' @param inputInOutput Logical vector (possibly recycled) for each element of hierarchies.
#'         TRUE means that codes from input are included in output. Values corresponding to \code{"rowFactor"} or \code{""} are ignored.
#' @param candidatesFromTotal When TRUE (default), same candidates for all rows and for all columns, computed from row/column totals.          
#' @param ... Further arguments to be passed to the supplied functions.
#'
#' @return Aggregated data with suppression information
#' 
#' @importFrom Matrix t rowSums
#' @importFrom SSBtools AutoHierarchies HierarchyCompute HierarchyCompute2 Hierarchies2ModelMatrix
#' @importFrom methods formalArgs
#' @export
#' 
#' @examples 
#' z3 <- SSBtoolsData("z3")
#' 
#' dimListsA <- SSBtools::FindDimLists(z3[, 1:6])
#' dimListsB <- SSBtools::FindDimLists(z3[, c(1, 4, 5)])
#' 
#' set.seed(123)
#' z <- z3[sample(nrow(z3),250),]
#' 
#' out1 <- GaussSuppressionTwoWay(z, freqVar = "ant", hierarchies = dimListsA, 
#'                                colVar = c("hovedint"))
#' out2 <- GaussSuppressionTwoWay(z, freqVar = "ant", hierarchies = dimListsA, 
#'                                colVar = c("hovedint", "mnd"))
#' out3 <- GaussSuppressionTwoWay(z, freqVar = "ant", hierarchies = dimListsB, 
#'                                colVar = c("region"))
#' out4 <- GaussSuppressionTwoWay(z, freqVar = "ant", hierarchies = dimListsB, 
#'                                colVar = c("hovedint", "region"))
#'                                
#' # "mnd" not in  hierarchies -> duplicated combinations in input 
#' # Error when  preAggregate is FALSE: Index method failed. Duplicated combinations?
#' out5 <- GaussSuppressionTwoWay(z, freqVar = "ant", hierarchies = dimListsA[1:3], 
#'                                protectZeros = FALSE, colVar = c("hovedint"), preAggregate = TRUE)
#' 
#' 
#' # charVar needed -> Still problem when preAggregate is TRUE
#' # Possible workaround by extra hierarchy 
#' out6 <- GaussSuppressionTwoWay(z, freqVar = "ant", charVar = "mnd2",
#'                                hierarchies = c(dimListsA[1:3], mnd2 = "Total"), # include charVar 
#'                                inputInOutput = c(TRUE, TRUE, FALSE),  # FALSE -> only Total 
#'                                protectZeros = FALSE, colVar = c("hovedint"),
#'                                preAggregate = TRUE,  
#'                                hidden = function(x, data, charVar, ...) 
#'                                  as.vector((t(x) %*% as.numeric(data[[charVar]] == "M06M12")) == 0))                                
GaussSuppressionTwoWay = function(data, dimVar = NULL, freqVar=NULL, numVar = NULL,  weightVar = NULL, charVar = NULL, #  freqVar=NULL, numVar = NULL, name
                                    hierarchies, formula = NULL,
                           maxN = 3, 
                           protectZeros = TRUE, 
                           secondaryZeros = FALSE,
                           candidates = CandidatesDefault,
                           primary = PrimaryDefault,
                           forced = NULL,                                                             
                           hidden = NULL,                                                             
                           singleton = SingletonDefault,                                              
                           singletonMethod = ifelse(secondaryZeros, "anySumNOTprimary", "anySum"),    
                           printInc = TRUE,
                           output = "publish",                                                        
                           preAggregate = is.null(freqVar),
                           colVar = names(hierarchies)[1],
                           removeEmpty = TRUE,
                           inputInOutput = TRUE,
                           candidatesFromTotal = TRUE, 
                           ...){ 
  if (is.null(hierarchies)) {
    stop("Hierarchies must be specified")
  }
  
  if (is.null(removeEmpty)) {
    removeEmpty_in_x <- TRUE
    removeEmpty <- FALSE
  } else {
    removeEmpty_in_x <- removeEmpty
  }
  
  if (removeEmpty) {
    colSelect <- "removeEmpty"
    rowSelect <- "removeEmpty"
  } else {
    colSelect <- NULL
    rowSelect <- NULL
    }
  
  
  total <- "Total"
  hierarchies <- AutoHierarchies(hierarchies = hierarchies, data = data, total = total, 
                                     hierarchyVarNames = c(mapsFrom = "mapsFrom", mapsTo = "mapsTo", sign = "sign", level = "level"))
  
  if(!(output %in% c("publish", "inner")))
    stop('Allowed values of parameter output are "publish" and "inner"')
  
  
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
  hc1 <- HierarchyCompute(data, hierarchies = hierarchies, valueVar = "iN_dEx", colVar = colVar,
                          colSelect = colSelect, rowSelect = rowSelect,
                          output = "matrixComponents", inputInOutput = inputInOutput, reduceData = removeEmpty_in_x)
  
  if( !all(range(diff(sort(as(hc1$hcRow$valueMatrix,"dgTMatrix")@x))) == c(1L, 1L))){
    extratext <- ""
    if (!preAggregate) {
      extratext <- "  Try preAggregate=TRUE?"
    } else {
      if (!is.null(charVar)){
        extratext <- "  Try workaround (see examples)?"
      }
    }
    stop(paste0("Index method failed. Duplicated combinations?", extratext))
  }
  
  outputMatrix <- hc1$hcRow$dataDummyHierarchy %*% hc1$hcRow$valueMatrix %*% t(hc1$hcCol$dataDummyHierarchy)
  
  value_dgT <- as(drop0(hc1$hcRow$valueMatrix), "dgTMatrix")
  value_i <- value_dgT
  
  if(removeEmpty){
    
    dgTframe_mT <- as(drop0(outputMatrix), "dgTMatrix")
    dgTframe <- AsDgTframe(dgTframe_mT, x = FALSE, frame = FALSE)
    
    freq_num_weight <- matrix(1, nrow(dgTframe), 0)
    freqVar_numVar_weightVar <- c(freqVar, numVar, weightVar)
    
    for (i in seq_along(c(freqVar, numVar, weightVar))) {
      value_i@x <- as.numeric(data[value_dgT@x, freqVar_numVar_weightVar[i]])
      freq_num_weight <- cbind(freq_num_weight, DgTframeNewValue(dgTframe, hc1$hcRow$dataDummyHierarchy %*% value_i %*% t(hc1$hcCol$dataDummyHierarchy)))
    }
    colnames(freq_num_weight) <- freqVar_numVar_weightVar
    
    hc2 <- cbind(hc1$hcCol$codeFrame[dgTframe[, "col"], , drop = FALSE], 
                 hc1$hcRow$toCrossCode[dgTframe[, "row"], , drop = FALSE], as.data.frame(freq_num_weight))
    rownames(hc2) <- NULL
    
  } else {
    # All numerical variables including "index"
    hc2 <- HierarchyCompute(data, hierarchies = hierarchies, valueVar = c(freqVar, numVar, weightVar), colVar = colVar, 
                            colSelect = colSelect, rowSelect = rowSelect,
                            inputInOutput = inputInOutput, reduceData = removeEmpty_in_x)
    dgTframe <- NULL
  }
   
  if (is.function(singleton)){ # freqVar must exist when singleton-function  
    value_i@x <- as.numeric(data[value_dgT@x, freqVar])
  }
  
  
  totalRow <- which.max(rowSums(hc1$hcRow$dataDummyHierarchy))
  totalCol <- which.max(rowSums(hc1$hcCol$dataDummyHierarchy))
  
  nRowOutput <- nrow(hc1$hcRow$dataDummyHierarchy)
  nColOutput <- nrow(hc1$hcCol$dataDummyHierarchy)
  

  if(removeEmpty){
    idxTotalRow <- which(dgTframe[,"row"]==totalRow)
    idxTotalCol <- which(dgTframe[,"col"]==totalCol)
  } else {
    idxTotalCol <- seq_len(nRowOutput) + (nRowOutput * (totalCol - 1))
    idxTotalRow <- totalRow + (seq_len(nColOutput) - 1) * nRowOutput
  }
  
  dataRow <- aggregate(data[unique(c(freqVar, numVar, weightVar))], data[rowVar], sum)
  
  dataCol <- aggregate(data[unique(c(freqVar, numVar, weightVar))], data[colVar], sum)
  
  
  xRow <- t(hc1$hcRow$dataDummyHierarchy)
  xCol <- t(hc1$hcCol$dataDummyHierarchy)
  
  
  if (!length(freqVar)) {
    freqRow <- NULL
    freqCol <- NULL
    freq <- NULL
  } else {
    freqRow <- hc2[idxTotalCol, freqVar , drop = TRUE]
    freqCol <- hc2[idxTotalRow, freqVar , drop = TRUE]
    freq = hc2[[freqVar]]
  }
  
  if (!length(numVar)) {
    numRow <- NULL
    numCol <- NULL
    num <- NULL
  } else {
    numRow <- hc2[idxTotalCol, numVar , drop = FALSE]
    numCol <- hc2[idxTotalRow, numVar , drop = FALSE]
    num = hc2[numVar]
  }
  

  if (!length(weightVar)) {
    weightRow <- NULL
    weightCol <- NULL
    weight <- NULL
  } else {
    weightRow <- hc2[idxTotalCol, weightVar , drop = TRUE]
    weightCol <- hc2[idxTotalRow, weightVar , drop = TRUE]
    weight = hc2[[weightVar]]
  }
  
  
  arg_x <- FALSE
  
  arg_x <- arg_x |  Arg_x(primary)
  if (!candidatesFromTotal)
    arg_x <- arg_x |  Arg_x(candidates)
  arg_x <- arg_x |  Arg_x(forced)
  arg_x <- arg_x |  Arg_x(hidden)
  
  if(arg_x){
    x <- Hierarchies2ModelMatrix(data, hierarchies = hierarchies, select = hc2[names(hierarchies)])
  } else {
    x <- NULL
  }
  
  
  if (is.function(primary) | is.list(primary))  
    primary <-     Primary(primary = primary, 
                           crossTable = hc2[names(hierarchies)], 
                           x = x,    
                           freq = freq , 
                           num = num, 
                           weight = weight, 
                           maxN = maxN, protectZeros = protectZeros, secondaryZeros = secondaryZeros, data = data, freqVar = freqVar, numVar = numVar, weightVar = weightVar, charVar = charVar, dimVar = dimVar, ...)
  
  if (!candidatesFromTotal)
  if (is.function(candidates))  candidates <- candidates(crossTable = hc2[names(hierarchies)], x = x, freq = freq, num = num, weight = weight, maxN = maxN, protectZeros = protectZeros, secondaryZeros = secondaryZeros, data = data, freqVar = freqVar, numVar = numVar, weightVar = weightVar, charVar = charVar, dimVar = dimVar, ...)
  
  if (is.function(forced))         forced <-      forced(crossTable = hc2[names(hierarchies)], x = x, freq = freq, num = num, weight = weight, maxN = maxN, protectZeros = protectZeros, secondaryZeros = secondaryZeros, data = data, freqVar = freqVar, numVar = numVar, weightVar = weightVar, charVar = charVar, dimVar = dimVar, ...)
  
  if (is.function(hidden))         hidden <-      hidden(crossTable = hc2[names(hierarchies)], x = x, freq = freq, num = num, weight = weight, maxN = maxN, protectZeros = protectZeros, secondaryZeros = secondaryZeros, data = data, freqVar = freqVar, numVar = numVar, weightVar = weightVar, charVar = charVar, dimVar = dimVar, ...)
  
  #if (is.function(singleton))   singleton <-   singleton(crossTable = hc2[names(hierarchies)], x = x, freq = freq, num = num, weight = weight, maxN = maxN, protectZeros = protectZeros, secondaryZeros = secondaryZeros, data = data, freqVar = freqVar, numVar = numVar, weightVar = weightVar, charVar = charVar, dimVar = dimVar, ...)
  
  rm(x)
  
  # non-logigal needed in EasySelect
  if (is.logical(hidden)) 
    hidden <- which(hidden) 
  else 
    hidden <- unique(hidden)
  
  if (is.logical(forced)) 
    forced <- which(forced) 
  else 
    forced <- unique(forced)
  
  
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
  
  if(removeEmpty){
    supprMatrix <- dgTframe_mT 
    supprMatrix@x <- as.numeric(primary)
  } else {
    supprMatrix <- matrix(primary, ncol = nColOutput)
  }
  
  supprSumRow <- colSums(supprMatrix)
  supprSumRow_old <- 0L * supprSumRow
  supprSumCol_old <- rep(0L, nrow(supprMatrix))
  
  xRow_i <- xRow
  xCol_i <- xCol
  
  
  if(removeEmpty){
    true <- 1
  } else {
    true <- TRUE
  }
  
  
  iter <- 0
  iterInfo = paste0(sum(supprSumRow), "-primary-suppressed\n")
  if (printInc) cat(iterInfo, "\n")
  
  while (sum(supprSumRow) > sum(supprSumRow_old)) {
    
    iter <- iter + 1 
    
    for (i in seq_len(nColOutput)) {
      if (supprSumRow[i] > supprSumRow_old[i]) {
        
        if (printInc) cat("col", i, ",", supprSumRow[i] - supprSumRow_old[i], "extra : ")
        
        if(removeEmpty_in_x){
          rr <- as.vector(as.matrix(hc1$hcRow$valueMatrix %*%  xCol[, i, drop=FALSE])) > 0
          # if(any(!rr)){
          #   if (printInc) cat("-",sum(!rr)," ", sep="")
          # }
          xRow_i <- xRow[rr, ,drop=FALSE] 
        } else {
          rr <- rep(TRUE, nrow(xRow))
        }
        
        if (is.function(singleton)){
          freqDF <- data.frame(x=as.vector(as.matrix(value_i %*%  xCol[, i, drop=FALSE]))[rr])
          names(freqDF) <- freqVar
          singleton_i <- singleton(data = freqDF, freqVar=freqVar, protectZeros=protectZeros, secondaryZeros=secondaryZeros)
        } else {
          singleton_i <- NULL
        }
        
        
        if (!candidatesFromTotal){
          candidatesROW <- DgTframeSelect(dgTframe = dgTframe, selection = candidates, dim1 = "row", i = i, nRowOutput = nRowOutput, nColOutput = nColOutput)
        }
        secondary <- GaussSuppression(x = xRow_i, candidates = candidatesROW, 
                                      primary = as.logical(supprMatrix[, i]), 
                                      forced = DgTframeSelect(dgTframe = dgTframe, selection = forced, dim1 = "row", i = i, nRowOutput = nRowOutput, nColOutput = nColOutput), 
                                      hidden = DgTframeSelect(dgTframe = dgTframe, selection = hidden, dim1 = "row", i = i, nRowOutput = nRowOutput, nColOutput = nColOutput),
                                      singleton = singleton_i, 
                                      singletonMethod = singletonMethod,
                                      printInc = printInc, whenEmptySuppressed = NULL, whenEmptyUnsuppressed = NULL, ...)
        
        if(length(secondary))
          supprMatrix[secondary, i] <- true
      }
    }
    
    supprSumRow_old <- colSums(supprMatrix)
    supprSumCol <- rowSums(supprMatrix)
    
    
    iterInfo <- paste0(iterInfo, "iter-",iter,"-col-",sum(supprSumCol), "-suppressed\n")
    if (printInc) cat(iterInfo, "\n")
    
    for (i in seq_len(nRowOutput)) {
      if (supprSumCol[i] > supprSumCol_old[i]) {
        
        if (printInc) cat("row", i, ",", supprSumCol[i] - supprSumCol_old[i], "extra : ")
        
        if(removeEmpty_in_x){
          rr <- as.vector(as.matrix(t(hc1$hcRow$valueMatrix) %*%  xRow[, i, drop=FALSE])) > 0
          #if(any(!rr)){
          #  if (printInc) cat("-",sum(!rr)," ", sep="")
          #}
          xCol_i <- xCol[rr, ,drop=FALSE] 
        } else {
          rr <- rep(TRUE, nrow(xCol))
        }
        
        if (is.function(singleton)){
          freqDF <- data.frame(x=as.vector(as.matrix(t(value_i) %*%  xRow[, i, drop=FALSE]))[rr])
          names(freqDF) <- freqVar
          singleton_i <- singleton(data = freqDF, freqVar=freqVar, protectZeros=protectZeros, secondaryZeros=secondaryZeros)
        } else {
          singleton_i <- NULL
        }
        
        if (!candidatesFromTotal){
          candidatesCol <- DgTframeSelect(dgTframe = dgTframe, selection = candidates, dim1 = "col", i = i, nRowOutput = nRowOutput, nColOutput = nColOutput)
        }
        secondary <- GaussSuppression(x = xCol_i, candidates = candidatesCol, 
                                      primary = as.logical(supprMatrix[i, ]), 
                                      forced = DgTframeSelect(dgTframe = dgTframe, selection = forced, dim1 = "col", i = i, nRowOutput = nRowOutput, nColOutput = nColOutput), 
                                      hidden = DgTframeSelect(dgTframe = dgTframe, selection = hidden, dim1 = "col", i = i, nRowOutput = nRowOutput, nColOutput = nColOutput),
                                      singleton = singleton_i, 
                                      singletonMethod = singletonMethod,
                                      printInc = printInc, whenEmptySuppressed = NULL, whenEmptyUnsuppressed = NULL, ...)
        if(length(secondary))
          supprMatrix[i, secondary] <- true
      }
    }
    
    supprSumCol_old <- rowSums(supprMatrix)
    supprSumRow <- colSums(supprMatrix)
    
    iterInfo <- paste0(iterInfo, "iter-",iter,"-row-",sum(supprSumRow), "-suppressed\n")
    if (printInc) cat(iterInfo, "\n")
    
  }  
  
  if(!removeEmpty){
    suppressed = as.vector(supprMatrix)
  } else {
    suppressed = as.logical(DgTframeNewValue(dgTframe,supprMatrix))
  }
  
  suppressed[hidden] <- NA
  
  if (length(freqVar)) names(hc2)[names(hc2) == freqVar] <- "freq"
  if (length(weightVar)) names(hc2)[names(hc2) == weightVar] <- "weight"
  
  cbind(hc2, primary = primary, numExtra, suppressed = suppressed )
  
}

AsDgTframe <- function(m = NULL, mT = NULL, x = TRUE, frame = TRUE) {
  if (is.null(mT)) {
    mT <- as(drop0(m), "dgTMatrix")
  }
  if (frame) {
    Cbind <- data.frame
  } else {
    Cbind <- cbind
  }
  if (x) {
    mF <- Cbind(row = mT@i + 1L, col = mT@j + 1L, x = mT@x)
  } else {
    mF <- Cbind(row = mT@i + 1L, col = mT@j + 1L)
  }
  doSort <- FALSE
  diffmF1 <- diff(mF[, 2])
  if (any(diffmF1 < 0)) {
    doSort <- TRUE
  } else {
    if (any(diff(mF[, 1])[diffmF1 == 0] < 0)) {
      doSort <- TRUE
    }
  }
  # doSort=TRUE
  if (doSort) {
    # mF <- SortRows(mF)
    mF <- mF[order(mF[, 2], mF[, 1]), ]
    warning("sorting needed")
  }
  mF
}

DgTframeNewValue <- function(obj, newM) {
  if (class(obj)[1] == "data.frame") {
    value <- newM[cbind(obj$row, obj$col)]
  } else {
    value <- newM[obj[, c("row", "col")]]
  }
  value
}



# TRUE when "x" is argument to function(s)
Arg_x <- function(fun) {
  if (is.function(fun)) {
    return("x" %in% formalArgs(fun))
  }
  if (!is.list(fun)) {
    return(FALSE) # Not a list of functions 
  }
  for (i in seq_along(fun)) {
    if (!is.function(fun[[i]])) {
      return(FALSE) # Not a list of functions 
    }
    if ("x" %in% formalArgs(fun[[i]])) {
      return(TRUE)
    }
  }
  FALSE
}

DgTframeSelect <- function(dgTframe, selection, dim1, i, nRowOutput, nColOutput) {
  if (!length(selection)){
    return(NULL)
  }
  
  if(is.null(dgTframe)){
    return(EasySelect( selection, dim1, i, nRowOutput, nColOutput))
  }
  
  a <- dgTframe[selection, ]
  
  if (dim1 == "row")
    dim2 <- "col" else dim2 <- "row"
    
    a[, dim1][a[, dim2] == i]
}


EasySelect <- function(selection, dim1, i, nRowOutput, nColOutput){
  if (dim1 == "row"){
    idx <- seq_len(nRowOutput) + (nRowOutput * (i - 1))
  } else {
    idx <- i + (seq_len(nColOutput) - 1) * nRowOutput
  }
  selection[selection %in% idx]
}
  
  





