#' K-disclosure suppression
#'
#' A function for suppressing frequency tables using the k-disclosure method.
#'
#' @param data a data.frame representing the data set
#' @param coalition numeric vector of length one, representing possible size of an
#' attacking coalition. This parameter corresponds to the parameter k in the
#' definition of k-disclosure.
#' @param idVars character vector of variables that can be known to an attacker. 
#' Experimental
#' @param sensitiveVars character vector or list describing sensitive variables or values.
#'  If the input is a character vector, it is must consist of variable names considered sensitive.
#'  If the input is a list, each list element is a vector of sensitive values, 
#'  and the list names are their corresponding variable names. Experimental
#' @param dimVar The main dimensional variables and additional aggregating
#' variables. This parameter can be  useful when hierarchies and formula are
#' unspecified.
#' @param formula A model formula
#' @param hierarchies List of hierarchies, which can be converted by
#' \code{\link{AutoHierarchies}}. Thus, the variables can also be coded by
#' `"rowFactor"` or `""`, which correspond to using the categories in the data.
#' @param freqVar name of the frequency variable in `data`
#' @param mc_hierarchies a hierarchy representing meaningful combinations to be
#' protected. Default value is `NULL`.
#' @param upper_bound numeric value representing minimum count considered safe.
#' Default set to `Inf`
#' @param ... parameters passed to children functions
#' @inheritParams GaussSuppressionFromData 
#'
#' @return A data.frame containing the publishable data set, with a boolean
#' variable `$suppressed` representing cell suppressions.
#' @export
#'
#' @author Daniel P. Lupp

#' @examples
#' # data
#' data <- SSBtools::SSBtoolsData("mun_accidents")
#'
#' # hierarchies as DimLists
#' mun <- data.frame(levels = c("@@", rep("@@@@", 6)),
#' codes = c("Total", paste("k", 1:6, sep = "")))
#' inj <- data.frame(levels = c("@@", "@@@@" ,"@@@@", "@@@@", "@@@@"),
#' codes = c("Total", "serious", "light", "none", "unknown"))
#' dimlists <- list(mun = mun, inj = inj)
#'
#'  inj2 <- data.frame(levels = c("@@", "@@@@", "@@@@@@", "@@@@@@"),
#' codes = c("Total", "injured", "serious", "light"))
#' inj3 <- data.frame(levels = c("@@", "@@@@", "@@@@" ,"@@@@", "@@@@"),
#' codes = c( "shadowtotal", "serious", "light", "none", "unknown"))

#' mc_dimlist <- list(inj = inj2)
#' mc_nomargs <- list(inj = inj3)
#'
#' # Example with formula, no meaningful combination
#' out <- SuppressKDisclosure(data, coalition = 1, freqVar = "freq", formula = ~mun*inj)
#'
#' # Example with hierarchy and meaningful combination
#' out2 <- SuppressKDisclosure(data, coalition = 1, freqVar = "freq",
#' hierarchies = dimlists, mc_hierarchies = mc_dimlist)
#'
#' # Example of table without mariginals, and mc_hierarchies to protect
#' out3 <- SuppressKDisclosure(data, coalition = 1, freqVar = "freq",
#' formula = ~mun:inj, mc_hierarchies = mc_dimlist )
#'
#'
#' # Examples of sensitive vars and values
#' data <- SSBtools::SSBtoolsData("mun_accidents")
#' data$freq <- c(0,5,3,4,1,0,
#'                0,0,2,0,0,6,
#'                4,1,0,4,0,0,
#'                0,0,0,0,0,0)
#' addPrikket <- function(out){
#'   out$prikket <- out$freq
#'   out$prikket[out$suppressed] <- "-"
#'   out
#' }
#' out_v <- SuppressKDisclosure(data, coalition = 1, freqVar = "freq",
#'                             formula = ~mun*inj, sensitiveVars = "inj")
#' out_v <- addPrikket(out_v)
#' reshape2::dcast(out_v, mun~inj, value.var = "freq")
#' reshape2::dcast(out_v, mun~inj, value.var = "prikket")
#'

#'
#' out_v1 <- SuppressKDisclosure(data, coalition = 1, freqVar = "freq",
#'                             formula = ~mun*inj, mc_hierarchies = mc_dimlist,
#'                             sensitiveVars = list(mun =  "k3", inj = "injured"))
#' out_v1 <- addPrikket(out_v1)
#' reshape2::dcast(out_v1, mun~inj, value.var = "prikket")

#' out_v2 <- SuppressKDisclosure(data, coalition = 1, freqVar = "freq",
#'                             formula = ~mun*inj, sensitiveVars = list(inj = "serious", mun = "k3"))
#' out_v2 <- addPrikket(out_v2)
#' reshape2::dcast(out_v2, mun~inj, value.var = "prikket")
#'
#' # example of idVars
#' out_id1 <- SuppressKDisclosure(data, coalition = 1, freqVar = "freq",
#'                             formula = ~mun*inj, idVars = "mun")
#' out_id1 <- addPrikket(out_id1)
#' reshape2::dcast(out_id1, mun~inj, value.var = "prikket")

#' out_id2 <- SuppressKDisclosure(data, coalition = 1, freqVar = "freq",
#'                             formula = ~mun*inj, idVars = "inj")
#' out_id2 <- addPrikket(out_id2)
#' reshape2::dcast(out_id2, mun~inj, value.var = "prikket")
#' 
#' # Same example as out_v, but with cells forced to be published, yielding unsafe table
#' out_unsafe <- SuppressKDisclosure(data, coalition = 1, freqVar = "freq",
#'                             formula = ~mun*inj, sensitiveVars = "inj", 
#'                             forced = c(12,14,15), output = "all")
#' out_unsafe$publish <- addPrikket(out_unsafe$publish)
#' reshape2::dcast(out_unsafe$publish, mun~inj, value.var = "freq")
#' reshape2::dcast(out_unsafe$publish, mun~inj, value.var = "prikket")
#' 
#' # colnames in $unsafe give an indication as to which cells/differences are unsafe
#' colnames(out_unsafe$unsafe)
SuppressKDisclosure <- function(data,
                                coalition = 0,
                                idVars = NULL,
                                sensitiveVars = NULL,
                                dimVar = NULL,
                                formula = NULL,
                                hierarchies = NULL,
                                freqVar = NULL,
                                mc_hierarchies = NULL,
                                upper_bound = Inf,
                                ...,
                                spec = PackageSpecs("kDisclosureSpec")) {
  additional_params <- list(...)
  if (length(additional_params)) {
    if ("singletonMethod" %in% names(additional_params) &
        "none" %in% additional_params[["singletonMethod"]])
      warning(
        "SuppressKDisclosure should use a singleton method for protecting the zero singleton problem. The output might not be safe, consider rerunning with a singleton method (default)."
      )
  }
  GaussSuppressionFromData(
    data,
    hierarchies = hierarchies,
    formula = formula,
    dimVar = dimVar,
    freqVar = freqVar,
    coalition = coalition,
    idVars = idVars,
    sensitiveVars = sensitiveVars,
    mc_hierarchies = mc_hierarchies,
    upper_bound = upper_bound,
    spec = spec,
    ...
  )
}

#' Construct primary suppressed difference matrix
#'
#' Function for constructing model matrix columns representing primary suppressed
#' difference cells
#'
#' @inheritParams SuppressKDisclosure
#' @param crossTable crossTable generated by parent function
#' @param x ModelMatrix generated by parent function
#'
#' @return dgCMatrix corresponding to primary suppressed cells
#' @importFrom methods new
#' @export
#'
#' @author Daniel P. Lupp
KDisclosurePrimary <- function(data,
                               x,
                               crossTable,
                               freqVar,
                               sensitiveVars = NULL,
                               idVars = NULL,
                               mc_hierarchies = NULL,
                               coalition = 1,
                               upper_bound = Inf,
                               ...) {
  mcModelMatrix <- X_from_mc(
    data = data,
    x = x,
    crossTable = crossTable,
    mc_hierarchies = mc_hierarchies,
    freqVar = freqVar,
    coalition = coalition,
    upper_bound = upper_bound,
    returnNewCrossTable = TRUE,
    ...
  )
  x <- cbind(x, mcModelMatrix$x)
  crossTable <- rbind(crossTable, mcModelMatrix$crossTable)
  
  x <- x[, !SSBtools::DummyDuplicated(x, rnd = TRUE), drop = FALSE]
  freq <- as.vector(crossprod(x, data[[freqVar]]))
  FindDifferenceCells(
    x = x,
    freq = freq,
    crossTable = crossTable,
    coalition = coalition,
    sensitiveVars = sensitiveVars,
    idVars = idVars,
    upper_bound = upper_bound
  )
}

# crossprod(x[, sensitivecols], x[, idcols])
FindDifferenceCells <- function(x,
                                freq,
                                crossTable,
                                coalition,
                                sensitiveVars = NULL,
                                idVars = NULL,
                                upper_bound = Inf) {
  publ_x <- crossprod(x)
  publ_x <-
    As_TsparseMatrix(publ_x)
  colSums_x <- colSums(x)
  # row i is child of column j in r
  r <-
    colSums_x[publ_x@i + 1] == publ_x@x &
    colSums_x[publ_x@j + 1] != publ_x@x
  publ_x@x <- publ_x@x[r]
  publ_x@j <- publ_x@j[r]
  publ_x@i <- publ_x@i[r]
  child_parent <- cbind(child = publ_x@i + 1,
                        parent = publ_x@j + 1,
                        diff = freq[publ_x@j + 1] - freq[publ_x@i + 1])
  child_parent <- child_parent[freq[child_parent[, 2]] > 0 &
                                 freq[child_parent[, 1]] > 0 &
                                 freq[child_parent[, 1]] <= upper_bound,]
  disclosures <-
    child_parent[child_parent[, 3] <= coalition, , drop = FALSE]
  
  if (!is.null(idVars)) {
    if (!all(names(crossTable) %in% idVars)) {
      nonIdVars <- names(crossTable)[!(names(crossTable) %in% idVars)]
      # need to replace this with variable totals, instead of hardcoded:
      nonIdTots <- as.list(rep("Total", length(nonIdVars)))
      names(nonIdTots) <- nonIdVars
      nonIdTots <- as.data.frame(nonIdTots)
      idRows <-
        which(!is.na(SSBtools::Match(crossTable[, nonIdVars, drop = FALSE],
                                     nonIdTots)))
      disclosures <-
        disclosures[disclosures[, 2] %in% idRows, , drop = FALSE]
    }
  }
  if (!is.null(sensitiveVars)) {
    sensitiveVals <- NULL
    if (is.list(sensitiveVars)) {
      sensitiveVals <- sensitiveVars
      sensitiveVars <- names(sensitiveVars)
    }
    # check whether parent and child have different sensitiveVars values
    sensitiveRows <- which(apply(disclosures, 1, function(x)
      is.na(
        SSBtools::Match(crossTable[x[1], sensitiveVars, drop = FALSE],
                        crossTable[x[2], sensitiveVars, drop = FALSE])
      )))
    # sensitive disclosure if parent and child are different
    disclosures <- disclosures[sensitiveRows, , drop = FALSE]
    if (!is.null(sensitiveVals)) {
      sensitiveRows <- NULL
      # for each row in disclosures, check whether any sensitiveVal occurs in sensitiveVars
      for (sv in sensitiveVars) {
        sensitiveRows <- c(sensitiveRows,
                           which(
                             apply(disclosures, 1, function(x)
                               crossTable[x[1], sv] %in% sensitiveVals[[sv]])
                           ))
      }
      disclosures <-
        disclosures[unique(sensitiveRows), , drop = FALSE]
    }
  }
  if (nrow(disclosures)) {
    pcells <- apply(crossTable[disclosures[, 2],], 1,
                    function(x)
                      paste0("(", paste(x, collapse = ","), ")"))
    ccells <- apply(crossTable[disclosures[, 1],], 1,
                    function(x)
                      paste0("(", paste(x, collapse = ","), ")"))
    
    diffMatrix <- new(
      "dgTMatrix",
      i = as.integer(c(disclosures[, 1:2]) - 1),
      j = as.integer(rep(0:(
        nrow(disclosures) - 1
      ), 2)),
      x = rep(c(-1, 1), each = nrow(disclosures)),
      Dim = c(ncol(x), nrow(disclosures))
    )
    
    primary_matrix <- x %*% diffMatrix
    colnames(primary_matrix) <- paste(pcells, ccells, sep = "-")
    return(primary_matrix)
  }
  else
    return(rep(FALSE, nrow(crossTable)))
}

# function for creating a dimlist to capture combinations of sensitive values. Future functionality
createSensitiveDimList <- function(sensitiveVar) {
  if (length(sensitiveVar) > 1)
    data.frame(
      levels = c("@", "@@", rep("@@@", length(sensitiveVar))),
      codes = c("Total", paste(sensitiveVar, collapse = ":"), sensitiveVar)
    )
  else
    NULL
}
