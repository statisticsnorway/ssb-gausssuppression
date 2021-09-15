

#' Cell suppression from input data containing inner cells
#' 
#' 
#' Aggregates are generated followed by 
#' primary suppression followed by 
#' secondary suppression by Gaussian elimination by \code{\link{GaussSuppression}} 
#' 
#' The supplied functions for generating \code{\link{GaussSuppression}} input takes the following arguments: 
#' `crossTable`,  `x`, `freq`, `num`, `weight`, `maxN`, `protectZeros`, `secondaryZeros`, `data`, `freqVar`, `numVar`, `weightVar`, `charVar` and `...`. 
#' where the two first are  \code{\link{ModelMatrix}} outputs (`modelMatrix` renamed to `x`).
#' The vector, `freq`, is aggregated counts (`t(x) %*% data[[freqVar]]`).
#' Similarly, `num`, is a data frame of aggregated numerical variables.   
#' It is possible to supply several primary functions joined by `c`, e.g. (`c(FunPrim1, FunPrim2)`). 
#' All `NA`s returned from any of the functions force the corresponding cells not to be primary suppressed.
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
#' @param singleton  GaussSuppression input or a function generating it (see details) Default: \code{\link{SingletonDefault}}
#' @param singletonMethod \code{\link{GaussSuppression}} input 
#' @param printInc        \code{\link{GaussSuppression}} input
#' @param output One of `"publish"` (default), `"inner"`, `"publish_inner"`, `"publish_inner_x"`, `"publish_x"`, 
#'                      `"inner_x"`, and `"input2functions"` (input to supplied functions). 
#'               Here "inner" means input data (possibly pre-aggregated) and 
#'               "x" means dummy matrix (as input parameter x).   
#' @param x `x` (`modelMatrix`) and `crossTable` can be supplied as input instead of generating it from  \code{\link{ModelMatrix}}
#' @param crossTable See above.  
#' @param preAggregate When `TRUE`, the data will be aggregated within the function to an appropriate level. 
#'        This is defined by the dimensional variables according to `dimVar`, `hierarchies` or `formula` and in addition `charVar`.
#' @param extraAggregate When `TRUE`, the data will be aggregated by the dimensional variables according to `dimVar`, `hierarchies` or `formula`.
#'                       The aggregated data and the corresponding x-matrix will only be as input to the singleton function and \code{\link{GaussSuppression}}. 
#'                       This extra aggregation is useful when parameter `charVar` is used. 
#' @param ... Further arguments to be passed to the supplied functions.
#'
#' @return Aggregated data with suppression information
#' @export
#' @importFrom SSBtools GaussSuppression ModelMatrix
#' @importFrom Matrix crossprod as.matrix
#' @importFrom stats aggregate as.formula delete.response terms
#' @importFrom utils flush.console
#'
#' @examples
#' 
#' z1 <- SSBtoolsData("z1")
#' GaussSuppressionFromData(z1, 1:2, 3)
#' 
#' z2 <- SSBtoolsData("z2")
#' GaussSuppressionFromData(z2, 1:4, 5, protectZeros = FALSE)
#' 
#' 
#' # Data as in GaussSuppression examples
#' df <- data.frame(values = c(1, 1, 1, 5, 5, 9, 9, 9, 9, 9, 0, 0, 0, 7, 7), 
#'                  var1 = rep(1:3, each = 5), var2 = c("A", "B", "C", "D", "E"))
#' 
#' GaussSuppressionFromData(df, c("var1", "var2"), "values")
#' GaussSuppressionFromData(df, c("var1", "var2"), "values", formula = ~var1 + var2, maxN = 10)
#' GaussSuppressionFromData(df, c("var1", "var2"), "values", formula = ~var1 + var2, maxN = 10, 
#'       primary = function(freq, crossTable, maxN, ...) 
#'                    which(freq <= maxN & crossTable[[2]] != "A" & crossTable[, 2] != "C"))
#'                    
#' # Combining several primary functions 
#' # Note that NA & c(TRUE, FALSE) equals c(NA, FALSE)                      
#' GaussSuppressionFromData(df, c("var1", "var2"), "values", formula = ~var1 + var2, maxN = 10, 
#'        primary = c(function(freq, maxN, ...) freq >= 45,
#'                    function(freq, maxN, ...) freq <= maxN,
#'                    function(crossTable, ...) NA & crossTable[[2]] == "C",  
#'                    function(crossTable, ...) NA & crossTable[[1]]== "Total" 
#'                                                 & crossTable[[2]]== "Total"))                    
#'                    
#' # Similar to GaussSuppression examples
#' GaussSuppressionFromData(df, c("var1", "var2"), "values", formula = ~var1 * var2, 
#'        candidates = NULL, singleton = NULL, protectZeros = FALSE, secondaryZeros = TRUE)
#' GaussSuppressionFromData(df, c("var1", "var2"), "values", formula = ~var1 * var2, 
#'        singleton = NULL, protectZeros = FALSE, secondaryZeros = FALSE)
#' GaussSuppressionFromData(df, c("var1", "var2"), "values", formula = ~var1 * var2, 
#'        protectZeros = FALSE, secondaryZeros = FALSE)
#' 
#'               
#' # Examples with zeros as singletons
#' z <- data.frame(row = rep(1:3, each = 3), col = 1:3, freq = c(0, 2, 5, 0, 0, 6:9))
#' GaussSuppressionFromData(z, 1:2, 3, singleton = NULL) 
#' GaussSuppressionFromData(z, 1:2, 3, singletonMethod = "none") # as above 
#' GaussSuppressionFromData(z, 1:2, 3)
#' GaussSuppressionFromData(z, 1:2, 3, protectZeros = FALSE, secondaryZeros = TRUE, singleton = NULL)
#' GaussSuppressionFromData(z, 1:2, 3, protectZeros = FALSE, secondaryZeros = TRUE)      
GaussSuppressionFromData = function(data, dimVar = NULL, freqVar=NULL, numVar = NULL,  weightVar = NULL, charVar = NULL, #  freqVar=NULL, numVar = NULL, name
                                    hierarchies = NULL, formula = NULL,
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
                           output = "publish", x = NULL, crossTable = NULL,
                           preAggregate = is.null(freqVar),
                           extraAggregate = preAggregate & !is.null(charVar), 
                           ...){ 
  
  
  if(!(output %in% c("publish", "inner", "publish_inner", "publish_inner_x", "publish_x", "inner_x", "input2functions" )))
    stop('Allowed values of parameter output are "publish", "inner", "publish_inner", "publish_inner_x", "publish_x", "inner_x", and "input2functions".')
  
  
  innerReturn <- output %in% c("inner", "publish_inner", "publish_inner_x", "inner_x")

  force(preAggregate)
  force(extraAggregate)
  
  dimVar <- names(data[1, dimVar, drop = FALSE])
  freqVar <- names(data[1, freqVar, drop = FALSE])
  numVar <- names(data[1, numVar, drop = FALSE])
  weightVar <- names(data[1, weightVar, drop = FALSE])
  charVar <- names(data[1, charVar, drop = FALSE])
  
  if (preAggregate | extraAggregate | innerReturn | (is.null(hierarchies) & is.null(formula) & !length(dimVar))) {
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
  
  
  if(extraAggregate){
    if (printInc) {
      cat("[extraAggregate ", dim(data)[1], "*", dim(data)[2], "->", sep = "")
      flush.console()
    }
    dataExtra <- aggregate(data[unique(c(freqVar, numVar, weightVar))], data[unique(dVar)], sum)
    if (printInc) {
      cat(dim(dataExtra)[1], "*", dim(dataExtra)[2], "] ", sep = "")
      flush.console()
    }
  }
  
  if(innerReturn){
    attr(data, "freqVar") <- freqVar
  }

  if (output == "inner") {
    return(data)
  }
  
  if (is.null(x)) {
    if (is.null(formula) & is.null(hierarchies)) {
      x <- SSBtools::ModelMatrix(data[, dimVar, drop = FALSE], crossTable = TRUE)
    } else {
      x <- SSBtools::ModelMatrix(data, hierarchies = hierarchies, formula = formula, crossTable = TRUE)
    }
    crossTable <- as.data.frame(x$crossTable)  # fix i ModelMatrix 
    x <- x$modelMatrix
  }
  
  if (output == "inner_x") {
    return(list(inner = data, x = x))
  }
  
  if (!length(freqVar)) {
    freq <- NULL
  } else {
    freq <- as.vector(as.matrix(crossprod(x, as.matrix(data[, freqVar, drop = FALSE]))))
  }
  
  if (!length(numVar)) {
    num <- NULL
  } else {
    num <- as.data.frame(as.matrix(crossprod(x, as.matrix(data[, numVar, drop = FALSE]))))
  }
  
  
  if (!length(weightVar)) {
    weight <- NULL
  } else {
    weight <- as.vector(as.matrix(crossprod(x, as.matrix(data[, weightVar, drop = FALSE]))))
  }
  
  if (output == "input2functions")           return(list(crossTable = crossTable, x = x, freq = freq, num = num, weight = weight, maxN = maxN, protectZeros = protectZeros, secondaryZeros = secondaryZeros, data = data, freqVar = freqVar, numVar = numVar, weightVar = weightVar, charVar = charVar, ...))
  
  if (is.function(candidates)) candidates <-  candidates(crossTable = crossTable, x = x, freq = freq, num = num, weight = weight, maxN = maxN, protectZeros = protectZeros, secondaryZeros = secondaryZeros, data = data, freqVar = freqVar, numVar = numVar, weightVar = weightVar, charVar = charVar, ...)
  
  if (is.function(primary) | is.list(primary))  
               primary <-     Primary(primary = primary, crossTable = crossTable, x = x, freq = freq, num = num, weight = weight, maxN = maxN, protectZeros = protectZeros, secondaryZeros = secondaryZeros, data = data, freqVar = freqVar, numVar = numVar, weightVar = weightVar, charVar = charVar, dimVar = dimVar, ...)
  
  if (is.function(forced))         forced <-      forced(crossTable = crossTable, x = x, freq = freq, num = num, weight = weight, maxN = maxN, protectZeros = protectZeros, secondaryZeros = secondaryZeros, data = data, freqVar = freqVar, numVar = numVar, weightVar = weightVar, charVar = charVar, ...)
  
  if (is.function(hidden))         hidden <-      hidden(crossTable = crossTable, x = x, freq = freq, num = num, weight = weight, maxN = maxN, protectZeros = protectZeros, secondaryZeros = secondaryZeros, data = data, freqVar = freqVar, numVar = numVar, weightVar = weightVar, charVar = charVar, ...)
  
  if (!extraAggregate)
  if (is.function(singleton))   singleton <-   singleton(crossTable = crossTable, x = x, freq = freq, num = num, weight = weight, maxN = maxN, protectZeros = protectZeros, secondaryZeros = secondaryZeros, data = data, freqVar = freqVar, numVar = numVar, weightVar = weightVar, charVar = charVar, ...)
  
  if(extraAggregate){
    if (is.null(formula) & is.null(hierarchies)) {
      xExtra <- SSBtools::ModelMatrix(dataExtra[, dimVar, drop = FALSE], crossTable = TRUE)
    } else {
      xExtra <- SSBtools::ModelMatrix(dataExtra, hierarchies = hierarchies, formula = formula, crossTable = TRUE)
    }
    crossTableExtra <- as.data.frame(xExtra$crossTable)  # fix i ModelMatrix 
    xExtra <- xExtra$modelMatrix
    if (printInc) {
      cat("Checking crossTables ..")
      flush.console()
    }
    if(!isTRUE(all.equal(crossTable, crossTableExtra))){
      stop("crossTables not equal")
    }
    if (is.function(singleton))   singleton <-   singleton(crossTable = crossTable, x = xExtra, freq = freq, num = num, weight = weight, maxN = maxN, protectZeros = protectZeros, secondaryZeros = secondaryZeros, data = dataExtra, freqVar = freqVar, numVar = numVar, weightVar = weightVar, charVar = charVar, ...)
  }
  
  
  m <- ncol(x)
  
  if (is.null(candidates)) candidates <- 1:m
  
  
  if (is.null(freq)) freq <- matrix(0, m, 0)
  if (is.null(num)) num <- matrix(0, m, 0)
  if (is.null(weight)) weight <- matrix(0, m, 0)
  
  if(extraAggregate){
    if (printInc) {
      cat(". Checking (freq, num, weight) ..")
      flush.console()
    }
    if(!isTRUE(all.equal(
      as.matrix(crossprod(xExtra, as.matrix(dataExtra[, c(freqVar, numVar, weightVar), drop = FALSE]))), 
      cbind(freq, num, weight),
      check.attributes = FALSE, check.names = FALSE)))
      stop("(freq, num, weight) all not equal")
    if (printInc) {
      cat(".\n")
      flush.console()
    }
  }
  
  # hack
  if(is.list(primary)){
    num = cbind(num, primary[[2]])
    primary = primary[[1]]
  }
  
  if(extraAggregate){
    secondary <- GaussSuppression(x = xExtra, candidates = candidates, primary = primary, forced = forced, hidden = hidden, singleton = singleton, singletonMethod = singletonMethod, printInc = printInc, ...)
  } else {
    secondary <-      GaussSuppression(x = x, candidates = candidates, primary = primary, forced = forced, hidden = hidden, singleton = singleton, singletonMethod = singletonMethod, printInc = printInc, ...)
  }
  suppressed <- rep(FALSE, m)
  suppressed[primary] <- TRUE
  primary <- suppressed
  suppressed[secondary] <- TRUE
  suppressed[hidden] <- NA

  
  publish <- cbind(as.data.frame(crossTable), freq = freq, num, weight = weight, primary = primary, suppressed = suppressed)
  
  if (output == "publish_inner_x") {
    return(list(publish = publish, inner = data, x = x))
  }
  
  if (output == "publish_inner") {
    return(list(publish = publish, inner = data))
  }
  
  if (output == "publish_x") {
    return(list(publish = publish, x = x))
  }
  
  publish
}



# combination of primary functions 
Primary <- function(primary, crossTable, ...) {
  num <- NULL
  pri <- 1L
  n <- nrow(crossTable)    # This line is why crossTable is parameter
  if (is.function(primary)) {
    primary <- c(primary)  # This is a list
  }
  for (i in seq_along(primary)) {
    a <- primary[[i]](crossTable = crossTable, ...)
    if (is.list(a)) {
      if (is.null(num)) {
        num <- a[[2]]
      } else {
        num <- cbind(num, a[[2]])
      }
      a <- a[[1]]
    }
    if (!is.logical(a)) { # Indices instead are allowed/possible  
      aInd <- a
      a <- rep(FALSE, n)
      a[aInd] <- TRUE
    }
    if (length(a) != n)
      stop("wrong length of primary function output")
    pri <- pri * as.integer(!a)    # zeros (=TRUE since !) and NA’s are preserved
  }
  pri <- !as.logical(pri)
  pri[is.na(pri)] <- FALSE    # No suppression when any NA
  
  if (is.null(num)) {
    return(pri)
  }
  list(primary = pri, numExtra = num)
}



#' PrimaryDefault
#'
#' Function for \code{\link{GaussSuppressionFromData}}
#'
#' @param freq freq 
#' @param x x
#' @param maxN maxN
#' @param protectZeros protectZeros 
#' @param ... Unused parameters 
#'
#' @return primary, \code{\link{GaussSuppression}} input 
#' @export
#' @keywords internal
PrimaryDefault <- function(freq, x, maxN, protectZeros, ...) {
  primary <- freq <= maxN
  if (!protectZeros) 
    primary[freq == 0] <- FALSE
  
  #which(primary)
  primary
}


#' CandidatesDefault
#'
#' Function for \code{\link{GaussSuppressionFromData}}
#'
#' @param freq freq 
#' @param x x
#' @param secondaryZeros secondaryZeros
#' @param ... Unused parameters 
#'
#' @return candidates, \code{\link{GaussSuppression}} input 
#' @export
#' @keywords internal
CandidatesDefault <- function(freq, x, secondaryZeros, weight, ...) {
  if(is.null(weight))
    weight <- 1
  else{
    if(min(weight)<0){
      weight[weight<0] = 0
      warning("Negative weights treated as zero")
    }
    if(min(weight)==0){
      weight <- weight + max(weight)*1E-20
    } 
  }
  tie <- as.matrix(Matrix::crossprod(x, x %*% ((freq+1)*weight)))
  tie <- tie/max(tie)
  freqOrd <- ((freq+1)*weight + 0.99 * tie)[, 1, drop = TRUE]
  if (!secondaryZeros) {
    freqOrd[freq == 0] <- 0.01 + max(freqOrd) + freqOrd[freq == 0]
  }
  candidates <- order(freqOrd, decreasing = TRUE)
  candidates
}
  

#' SingletonDefault
#'
#' Function for \code{\link{GaussSuppressionFromData}}
#'
#' @param data data  
#' @param freqVar freqVar
#' @param protectZeros protectZeros 
#' @param secondaryZeros secondaryZeros
#' @param ... Unused parameters 
#'
#' @return singleton, \code{\link{GaussSuppression}} input 
#' @export
#' @keywords internal
SingletonDefault <- function(data, freqVar, protectZeros, secondaryZeros, ...) {
  if (protectZeros | secondaryZeros){ 
    return(data[[freqVar]] == 0)
  }
  data[[freqVar]] == 1
}




#' Find major contributors to aggregates 
#' 
#' Assuming aggregates are calculated via a dummy matrix by 
#' \code{z = t(x) \%*\% y}, 
#' the \code{n} largest contributors are found (value or index) for each aggregate. 
#'
#' @param x A (sparse) dummy matrix
#' @param y Vector of input values (contributors)
#' @param n Number of contributors to be found
#' @param decreasing Ordering parameter. Smallest contributors found when \code{FALSE}. 
#' @param index Indices to \code{y} returned when TRUE
#'
#' @return Matrix with lagest contributors in first column, second largest in second column and so on.  
#' @export
#' @importFrom SSBtools SortRows
#' @importFrom Matrix drop0
#' @importFrom methods as
#' 
#' @seealso \code{\link{ModelMatrix}}
#'
#' @examples
#' library(SSBtools)
#' 
#' z <- SSBtoolsData("sprt_emp_withEU")
#' z$age[z$age == "Y15-29"] <- "young"
#' z$age[z$age == "Y30-64"] <- "old"
#' 
#' a <- ModelMatrix(z, formula = ~age + geo, crossTable = TRUE)
#' 
#' cbind(as.data.frame(a$crossTable), MaxContribution(a$modelMatrix, z$ths_per, 1))
#' cbind(a$crossTable, MaxContribution(a$modelMatrix, z$ths_per, 10))
#' cbind(a$crossTable, MaxContribution(a$modelMatrix, z$ths_per, 10, index = TRUE))
#' 
#' b <- ModelMatrix(z[, -4], crossTable = TRUE, inputInOutput = c(TRUE, FALSE, TRUE))
#' 
#' cbind(b$crossTable, MaxContribution(b$modelMatrix, z$ths_per, 10))
#' 
MaxContribution <- function(x, y, n = 1, decreasing = TRUE, index = FALSE) {
  
  ordy <- order(y, decreasing = decreasing)
  
  xT <- as(drop0(x[ordy, ]), "dgTMatrix")
  
  xM <- cbind(col = xT@j + 1, row = xT@i + 1)
  
  diffxM1 <- diff(xM[, 1])
  if (any(diffxM1 < 0)) {
  } else {
    if (any(diff(xM[, 2])[diffxM1 == 0] < 0)) {
      xM <- SortRows(xM)
      warning("sorting needed")
    }
  }
  rm(diffxM1)
  
  seqCol <- seq_len(ncol(x))
  
  maxC <- matrix(NA_integer_, ncol(x), n)
  
  for (i in seq_len(n)) {
    if (i > 1) {
      xM[ma, 1] <- 0
    }
    ma <- match(seqCol, xM[, 1])
    maxC[, i] <- ordy[xM[ma, 2]]
  }
  if (index) 
    return(maxC)
  
  maxC[] <- y[maxC]
  
  maxC
}


#' Find the number of unique groups contributing to aggregates 
#' 
#' Assuming aggregates are calculated via a dummy matrix by 
#' \code{z = t(x) \%*\% y}, the the number of unique contributing groups,
#' according to a grouping variable, are found for each aggregate.
#' The missing group category is not counted.  
#'  
#'
#' @param x A (sparse) dummy matrix
#' @param groups Vector of group categories 
#' 
#' @return Vector of numbers of unique groups
#' @export
#' @importFrom SSBtools SortRows
#' @importFrom Matrix drop0
#' @importFrom methods as
#' 
#' @seealso \code{\link{ModelMatrix}}
#'
#' @examples
#' library(SSBtools)
#' 
#' z <- SSBtoolsData("sprt_emp_withEU")
#' z$age[z$age == "Y15-29"] <- "young"
#' z$age[z$age == "Y30-64"] <- "old"
#' z$groups <- c("A", "A", "B", "A", "B", "C")
#' 
#' a <- ModelMatrix(z, formula = ~age*eu + geo + year, crossTable = TRUE)
#' 
#' cbind(as.data.frame(a$crossTable), nGroups = Ncontributors(a$modelMatrix, z$groups))
#' cbind(as.data.frame(a$crossTable), nYears = Ncontributors(a$modelMatrix, z$year))
#' cbind(as.data.frame(a$crossTable), nUnique_ths_per = Ncontributors(a$modelMatrix, z$ths_per))
#' 
Ncontributors <- function(x, groups) {
  
  if(length(groups) != nrow(x)){
    stop("Incorrect length of groups")
  }
  
  if (anyNA(groups)) {
    rows <- !is.na(groups)
    groups <- groups[rows]
    x <- x[rows, , drop = FALSE]
  }
  
  if (ncol(x) == 0) {
    return(integer(0))
  }
  
  if (nrow(x) == 0) {
    return(rep(0L, ncol(x)))
  }
  
  if (ncol(x) == 1) {
    return(length(unique(groups[x[, 1] != 0])))
  }
  
  groups <- as.integer(factor(groups))
  
  if (max(groups) == 1) {
    return(rep(1L, ncol(x)))
  }
  
  ordgroups <- order(groups)
  groups <- groups[ordgroups]
  
  xT <- as(drop0(x[ordgroups, , drop=FALSE]), "dgTMatrix")
  
  xM <- cbind(col = xT@j + 1, row = xT@i + 1)
  
  diffxM1 <- diff(xM[, 1])
  if (any(diffxM1 < 0)) {
  } else {
    if (any(diff(xM[, 2])[diffxM1 == 0] < 0)) {
      xM <- SortRows(xM)
      warning("sorting needed")
    }
  }
  rm(diffxM1)
  
  xM[, 2] <- groups[xM[, 2]]
  
  xM <- xM[!duplicated(xM), ]
  
  tab <- table(c(xM[, 1], seq_len(ncol(x)))) - 1L  # sjekk sortering
  
  if (any(range(diff(as.integer(names(tab)))) != c(1L, 1L))) {
    warning("Sorting problems in output from table")
    return(tab)
  }
  as.vector(tab)
}

#' \code{\link{Ncontributors}} with holding-indicator
#' 
#' The aggregates (columns of \code{x}) are grouped by a holding indicator. 
#' Within each holding group, the number of unique groups (output) is set to be equal. 
#' 
#' A representative within the holding group is used to calculate output by \code{\link{Ncontributors}}. 
#' The one with maximal column sum of \code{x} is chosen as the representative. 
#' Normally this will be an aggregate representing the holding group total. 
#' When holdingInd is NULL (default), the function is equivalent to \code{\link{Ncontributors}}.
#'
#' @param x A (sparse) dummy matrix
#' @param groups      Vector of group categories
#' @param holdingInd  Vector of holding group categories
#' 
#' @return Vector of numbers of unique groups
#' @importFrom Matrix colSums
#' @export
NcontributorsHolding <- function(x, groups, holdingInd=NULL) { # holding-indicator
  if (is.null(holdingInd)){
    return(Ncontributors(x, groups))
  }
  if (length(holdingInd) != ncol(x)) {
    stop("Incorrect length of holdingInd")
  }
  holding <- as.integer(factor(holdingInd))
  
  ordcols <- order(colSums(x), decreasing = TRUE)
  
  ma <- match(seq_len(max(holding)), holding[ordcols])
  
  Ncontributors(x[, ordcols[ma], drop = FALSE], groups)[holding]
}







