


#' GaussSuppressDec
#'
#' @param data Input daata as a data frame 
#' @param ... Further parameters to \code{\link{GaussSuppressionFromData}}
#' @param output output
#' @param digits digits 
#' @param nRep nRep
#' @param rmse rmse
#' @param rndSeed If non-NULL, a random generator seed to be used locally within the function without affecting the random value stream in R. 
#'
#' @return
#' 
#' @importFrom RegSDC SuppressDec
#' @importFrom SSBtools RoundWhole Match
#' @importFrom Matrix crossprod
#' @export
#'
#' @examples
#' z1 <- SSBtoolsData("z1")
#' GaussSuppressDec(z1, 1:2, 3)
#' GaussSuppressDec(z1, freqVar = "ant", formula = ~ region + hovedint, maxN = 10)
GaussSuppressDec = function(data, 
                            ..., 
                            output = NULL, 
                            digits = 9, 
                            nRep = NULL,
                            rmse = pi/3,
                            rndSeed = 123){

  if (!is.null(rndSeed)) {
    if (!exists(".Random.seed")) 
      if (runif(1) < 0) 
        stop("Now seed exists")
    exitSeed <- .Random.seed
    on.exit(.Random.seed <<- exitSeed)
    set.seed(rndSeed)
  }
  
  if(!is.null(output)){
    if(!(output %in% c("all", "both", "inner", "publish")))
      stop('Allowed non-NULL values of parameter output are "all", "both", "inner" and "publish".')
    
  } else {
    output <- ""
  }
  
  if (is.null(nRep)) {
    freqDecNames <- "freqDec"
    nRep <- 1
  } else {
    freqDecNames <- paste0("freqDec", paste(seq_len(nRep)))
  }
  
  a <- GaussSuppressionFromData(data, ..., xReturn = TRUE, innerReturn = TRUE)
  
  dimVarPub <- colnames(a$publish)
  dimVarPub <- dimVarPub[!(dimVarPub %in% c("freq", "primary", "suppressed"))]
  
  innerFreqName <- attr(a$inner, "freqVar")
  
  z <- as.matrix(a$publish["freq"])
  y <- as.matrix(a$inner[innerFreqName])
  
  yDec <- SuppressDec(a$x, z = z, y = y, suppressed = a$publish$suppressed, digits = digits, nRep = nRep, rmse = rmse)
  zDec <- RoundWhole(as.matrix(Matrix::crossprod(a$x, yDec)), digits = digits)
  
  # print(max(abs(zDec - as.matrix(Matrix::crossprod(a$x, yDec)))))
  
  colnames(yDec) <- freqDecNames
  colnames(zDec) <- freqDecNames
  
  
  a$publish <- cbind(a$publish, zDec)
  a$inner <- cbind(a$inner, yDec)
  
  if (any(a$publish$suppressed == (a$publish[["freq"]] == a$publish[[freqDecNames[1]]]))) 
    warning("problem")
  
  if (output == "all") 
    return(a)
  
  if (output == "both") 
    return(a[c("publish", "inner")])
  
  if (output == "publish") 
    return(a$publish)
  
  if (output == "inner") 
    return(a$inner)
  
  ma <- Match(a$inner[dimVarPub], a$publish[dimVarPub])
  
  a$publish$isPublish <- TRUE
  a$publish$isInner <- FALSE
  a$publish$isInner[ma[!is.na(ma)]] <- TRUE
  
  if (!anyNA(ma)) {
    return(a$publish)
  }
  
  a$inner <- a$inner[is.na(ma), c(dimVarPub, innerFreqName, freqDecNames), drop = FALSE]
  names(a$inner)[names(a$inner) == innerFreqName] <- "freq"
  
  a$inner$isPublish <- FALSE
  a$inner$isInner <- TRUE
  
  a$inner$primary <- NA
  a$inner$suppressed <- NA
  
  rbind(a$publish, a$inner)
  
}