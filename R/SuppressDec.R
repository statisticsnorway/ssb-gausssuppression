
# Code in this file is copied from package RegSDC and modified to do sparse algorithm within IpsoExtra   


SuppressDec <- function(x, z = NULL, y = NULL, suppressed = NULL, digits = 9, nRep = 1, yDeduct = NULL, resScale = NULL, rmse = NULL, sparseLimit) {
  origY <- !is.null(y)
  if (!is.null(z)) 
    z <- EnsureMatrix(z, NCOL(x))
  if (!is.null(y)) 
    y <- EnsureMatrix(y, NROW(x))
  
  
  if (!is.null(suppressed)) {
    suppr <- suppressed
  } else {
    
    if (is.null(z)) {
      suppr <- rep(FALSE, NCOL(x))
    } else {
      suppr <- is.na(z[, 1])
    }
  }
  
  if (is.null(y)) 
    if (!is.null(suppressed)) 
      if (!any(is.na(z))) 
        if (any(suppr)) {
          a <- ReduceX(x = x, z = z, digits = digits)
          y <- a$y
          origY <- !any(!a$yKnown)
        }
  
  nonSuppr <- which(!suppr)
  if (is.null(y)) {
    if (is.null(z)) {
      stop("z needed in input when y is NULL")
    }
  } else {
    z <- NULL
  }
  
  a <- ReduceX(x = x[, nonSuppr, drop = FALSE], z = z[nonSuppr, , drop = FALSE], y = y, digits = digits)
  
  
  if (is.null(y)) 
    origY <- !any(!a$yKnown)
  
  if (!origY) {
    if (is.null(rmse)) {
      stop("Without original y values, rmse must be supplied.")
    } else {
      if (!is.null(resScale)) {
        warning("Without original y values, rmse is used instead of resScale.")
      }
    }
  }
  
  
  if (any(!a$yKnown)){ 
    if(is.null(yDeduct)){
      rw <- RoundWhole(IpsoExtra(y = a$y[which(!a$yKnown), , drop = FALSE], x = a$x, nRep = nRep, 
                                 ensureIntercept = FALSE, rmse = rmse, resScale = resScale, sparseLimit = sparseLimit), digits = digits)
    } else {
      yDeduct <- EnsureMatrix(yDeduct)[which(!a$yKnown), , drop = FALSE]
      rw <- RoundWhole(IpsoExtra(y = a$y[which(!a$yKnown), , drop = FALSE] - yDeduct, x = a$x, nRep = nRep, 
                                 ensureIntercept = FALSE, rmse = rmse, resScale = resScale, sparseLimit = sparseLimit), digits = digits)
      if (nRep != 1)
        yDeduct <- ColRepMatrix(yDeduct, nRep)
      rw <- rw + yDeduct
    }
  }
  
  if (nRep != 1) 
    a$y <- ColRepMatrix(a$y, nRep)
  if (any(!a$yKnown)) 
    a$y[which(!a$yKnown), ] <- rw
  a$y
}



#' @importFrom RegSDC GenQR ReduceX Z2Yhat EnsureIntercept EnsureMatrix
#' @importFrom SSBtools SeqInc DummyDuplicated GaussIndependent
#' @importFrom stats rnorm runif
#' @importFrom Matrix which solve crossprod
NULL



ColRepMatrix <- function(x, nRep) {
  if (nRep == 1) 
    return(x)
  if (nRep == 0) 
    return(x[, integer(0), drop = FALSE])
  cn <- colnames(x)
  rn <- rownames(x)
  x <- matrix(x, nrow(x), ncol(x) * nRep)
  if (!is.null(cn)) 
    colnames(x) <- rep_len(cn, ncol(x))
  if (!is.null(rn)) 
    rownames(x) <- rn
  x
}




IpsoExtra <- function(y, x = NULL, ensureIntercept = TRUE, returnParts = FALSE, nRep = 1, resScale = NULL, digits = 9, rmse = NULL, sparseLimit, printInc = TRUE) {
  
  y <- EnsureMatrix(y)
  
  sparse <- (nrow(y) > sparseLimit) & (class(x)[1] != "matrix") 
  
  if(sparse){
    if(nrow(y) != nrow(x))
      stop("nrow(y) != nrow(x)")
    if (ensureIntercept)
      stop("ensureIntercept not implemented when sparse")
    dd <- DummyDuplicated(x, rnd = TRUE)
    if (any(dd)) {
      if (printInc) {
        cat("-")
        flush.console()
      }
      x <- x[ , !dd, drop = FALSE]
    }
    x <- x[ , GaussIndependent(x, printInc = printInc)[[2]], drop = FALSE]
    if (printInc) {
      cat("\n")
    }
    NROW_xQ <- nrow(x)
    NCOL_xQ <- ncol(x)
    crossprod_x <- crossprod(x) 
  } else {
    x <- EnsureMatrix(x, nrow(y))
    if (ensureIntercept) 
      x <- EnsureIntercept(x)
    xQ <- GenQR(x, findR = FALSE)
    NROW_xQ <- NROW(xQ)
    NCOL_xQ <- NCOL(xQ)
  }
  
  if (NROW_xQ == NCOL_xQ) {
    if (!is.null(resScale) | !is.null(rmse)) 
      warning("resScale/rmse ignored when Q from X is square.")
    if (nRep != 1) 
      y <- ColRepMatrix(y, nRep)
    if (returnParts) 
      return(list(yHat = y, yRes = 0 * y)) else return(y)
  }
  
  if(sparse){
    yHat <- as.matrix(x %*% solve(crossprod_x, crossprod(x, y)))
  } else {
    yHat <- xQ %*% (t(xQ) %*% y)
  }
  
  n <- NROW(y)
  ncoly <- NCOL(y)
  
  eQRR <- NULL
  if (!is.null(digits) & !is.null(resScale)) {
    if (!is.null(rmse)) 
      if (max(abs(round(y - yHat, digits = digits))) == 0) {
        if (ncoly > 1){
          warning("rmse with identical residual vectors used instead of resScal since perfect fit.")
        } else {
          warning("rmse used instead of resScal since perfect fit.")
        }
        resScale <- NULL
        eQRR <- matrix(1, 1, ncoly)  # Changed below
        m <- 1L
      }
  }
  
  if (is.null(eQRR)) {
    eQRR <- GenQR(y - yHat, makeunique = TRUE)$R
    m <- NROW(eQRR)
  }
  
  
  if (!is.null(rmse)) 
    if (ncoly > 1){ 
      rmseVar <- match(TRUE,!is.na(rmse))
      rmse <- rmse[rmseVar] 
      resScale <- rmse * sqrt((n - NCOL_xQ)/sum(eQRR[, rmseVar]^2)) 
    }
  
  
  if (!is.null(resScale)) {
    eQRR <- resScale * eQRR
  } else {
    if (!is.null(rmse)) 
      eQRR[] <- sqrt(n - NCOL_xQ) * rmse
  }
  
  if (nRep != 1) {
    yHat <- ColRepMatrix(yHat, nRep)
    yRes <- 0 * yHat
  }
  for (i in seq_len(nRep)) {
    yNew <- matrix(rnorm(n * m), n, m)
    if(sparse){
      eSim <- yNew - as.matrix(x %*% solve(crossprod_x, crossprod(x, yNew)))
    } else {
      eSim <- yNew - xQ %*% (t(xQ) %*% yNew)
    }
    eSimQ <- GenQR(eSim, findR = FALSE, makeunique = TRUE)
    if (nRep == 1) 
      yRes <- eSimQ %*% eQRR 
    else {
      yRes[, SeqInc(1 + ncoly * (i - 1), ncoly * i)] <- eSimQ %*% eQRR
    }
  }
  if(!is.null(rownames(y))){
    rownames(yHat) <- rownames(y)
    rownames(yRes) <- rownames(y)
  }
  if (returnParts) 
    return(list(yHat = yHat, yRes = yRes))
  yHat + yRes
}