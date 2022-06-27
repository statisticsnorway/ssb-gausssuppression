#' K-disclosure suppression
#' 
#' A function for suppressing frequency tables using the k-disclosure method.
#' 
#' Untested for tables with more than two dimensions, likely some small changes
#' needed (the problem will probably be in x_with_mc). Future versions will
#' support disclosive/non-disclosive unknowns.
#'
#' @param data a data.frame representing the data set
#' @param k numeric vector of length one, representing possible size of
#' attacking coalition
#' @param dimVar The main dimensional variables and additional aggregating
#' variables. This parameter can be  useful when hierarchies and formula are
#' unspecified. 
#' @param formula A model formula
#' @param hierarchies List of hierarchies, which can be converted by 
#' \code{\link{AutoHierarchies}}. Thus, the variables can also be coded by 
#' `"rowFactor"` or `""`, which correspond to using the categories in the data.
#' @param freqVar name of the frequency variable in `data`
#' @param mc_hierarchies a hierarchy representing meaningful combinations to be
#' protected
#' @param ... parameters passed to children functions
#'
#' @return A data.frame containing the publishable data set, with a boolean
#' variable `$suppressed` representing cell suppressions.
#' @export
#' 
#' @author Daniel P. Lupp

#' @examples
#' # data
#' mun <- c("k1", "k2", "k3", "k4", "k5", "k6")
#' inj <- c("serious", "light", "none", "unknown")
#' data <- expand.grid(mun, inj)
#' names(data) <- c("mun", "inj")
#' data$freq <- c(4,5,3,4,1,6,
#' 0,0,2,1,0,0,
#' 0,1,1,4,0,0,
#' 0,0,0,0,0,0)
#' 
#' # hierarchies as DimLists
#' mun <- data.frame(levels = c("@", rep("@@", 6)), 
#' codes = c("Total", paste("k", 1:6, sep = "")))
#' inj <- data.frame(levels = c("@", "@@" ,"@@", "@@", "@@"), 
#' codes = c("Total", "serious", "light", "none", "unknown"))
#' inj2 <- data.frame(levels = c("@", "@@", "@@@" ,"@@@", "@@", "@@"), 
#' codes = c("Total", "injured", "serious", "light", "none", "unknown"))
#' dimlists <- list(mun = mun, inj = inj)
#' dimlists2 <- list(mun = mun, inj = inj2)
#' 
#' # Example with formula, without meaningful combinations
#' out <- SuppressKDisclosure(data, k = 1, freqVar = "freq", formula = ~mun*inj)
#' 
#' # Example with hierarchy and meaningful combination
#' out2 <- SuppressKDisclosure(data, k = 1, freqVar = "freq", 
#' hierarchies = dimlists, mc_hierarchies = dimlists2)
SuppressKDisclosure <- function(data,
                                k = 1,
                                dimVar = NULL,
                                formula = NULL,
                                hierarchies = NULL,
                                freqVar = NULL,
                                mc_function = x_with_mc,
                                mc_hierarchies = NULL,
                                upper.bound = Inf,
                                ...) {
  if (is.null(hierarchies) & is.null(formula) & is.null(dimVar))
    stop("You must specify hierarchy, formula, or dimVar.")
  if (!is.function(mc_function))
    stop("Parameter mc_function must be a function.")
  additional_params <- list(...)
  if (length(additional_params)) {
    if ("singletonMethod" %in% names(additional_params) & 
        "none" %in% additional_params[["singletonMethod"]])
    warning("SuppressKDisclosure should use a singleton method for protecting the zero singleton problem. The output might not be safe, consider rerunning with a singleton method (default).")
  }
  GaussSuppressionFromData(data,
                           hierarchies = hierarchies,
                           formula = formula,
                           dimVar = dimVar,
                           freqVar = freqVar,
                           coalition = k,
                           mc_hierarchies = mc_hierarchies,
                           mc_function = mc_function,
                           upper.bound = upper.bound,
                           primary = KDisclosurePrimary,
                           candidates = DirectDisclosureCandidates,
                           protectZeros = FALSE,
                           secondaryZeros = 1,
                           ...)
}

KDisclosurePrimary <- function(data,
                               x,
                               crossTable,
                               mc_function,
                               mc_hierarchies,
                               freqVar,
                               coalition,
                               upper.bound, ...) {
  x <- cbind(x, mc_function(data = data,
                            x = x,
                            crossTable = crossTable,
                            mc_hierarchies = mc_hierarchies,
                            freqVar = freqVar,
                            coalition = coalition,
                            upper.bound = upper.bound,
                            ...
                            ))
  # x <- x_with_mc(x, crossTable, mc_hierarchies = mc_hierarchies)
  freq <- as.vector(crossprod(x, data[[freqVar]]))
  find_difference_cells(x = x,
                        freq = freq,
                        coalition = coalition,
                        upper.bound = upper.bound)
}

x_with_mc <- function(x, crossTable, mc_hierarchies, ...) {
  if (is.null(mc_hierarchies))
    return(NULL)
  unique_vars <- unique(names(mc_hierarchies))
  mc.labels <- sapply(unique_vars,
                      function(x)
                        extract_inner_nodes(Reduce(rbind,
                                                   mc_hierarchies[which(x == names(mc_hierarchies))])))
  mc.labels <- mc.labels[sapply(mc.labels, function (x) !is.null(x))]
  mcHier <- AutoHierarchies(mc_hierarchies)
  dimVar <- names(crossTable)
  for (var in names(mc.labels)) {
    tVar <- dimVar[!(dimVar == var)]
    for (mc in mc.labels[[var]]) {
      mcsubs <- unique(mcHier[[var]]$mapsFrom[mcHier[[var]]$mapsTo == mc])
      mcsubinds <- which(crossTable[[var]] %in% mcsubs)
      unqs <- unique(crossTable[crossTable[[var]] %in% mcsubs, tVar,
                                drop = FALSE])
      mcct <- crossTable[crossTable[[var]] %in% mcsubs, tVar, drop = FALSE]
      mcmatrix <- cbind(mcsubinds, Match(mcct, unqs))
      cx <- sapply(unique(mcmatrix[,2]),
                   function(x) Reduce(c, mcmatrix[mcmatrix[,2] == x,1]))
      cx <- as(Reduce(cbind,
                      apply(cx, 2,
                            function(y)
                              as(matrix(rowSums(x[,y])), "dgTMatrix"))),
               "dgCMatrix")
      colnames(cx) <- paste(apply(unqs,1, function(x) paste(x, collapse = ":")),
                            mc, sep = ":")
      # x <- cbind(x, cx)
    }
  }
  cx
}


x_with_mc2 <- function(x, data, mc_hierarchies) {
  xx <- cbind(x, SSBtools::ModelMatrix(data, hierarchies = mc_hierarchies))
  xx <- xx[, !SSBtools::DummyDuplicated(xx, rnd = TRUE), drop = FALSE]
  xx <- xx[, colSums(xx) != 0, drop = FALSE]
  xx
}


find_difference_cells <- function(x,
                                  freq,
                                  coalition = 1,
                                  upper.bound = Inf) {
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
  child_parent <- child_parent[freq[child_parent[,2]] > 0 &
                                 freq[child_parent[,1]] > 0 & 
                                 freq[child_parent[,1]] <= upper.bound,]
  disclosures <- child_parent[child_parent[,3] <= coalition, ]
  if (nrow(disclosures))
    primary_matrix <- as(apply(disclosures,
                               1,
                               function(row) x[,row[2]] - x[,row[1]]),
                         "dgTMatrix")
  else primary_matrix <- NULL
  # colnames(primary_matrix) <- apply(disclosures[,2:1], 1, function(x) paste(x, collapse = "-"))
  primary_matrix
}

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
