#' Incremental Time Suppression
#' 
#' Suppression function useful for sequential calculations over successive time periods. 
#'
#' @inheritParams AdditionalSuppression
#' @param timeVar The time period variable  
#' @param formula  formula A formula defining tables within the time periods. 
#'     Therefore, the variable `timeVar`  should not be included.
#' @param subTotals Whether all cumulative totals over time should be included. 
#' @param finalTotal When `FALSE`, the `timeVar` total is named according to time period categories. 
#' @param totalPriority  When `FALSE`, the `timeVar`  totals are not prioritized 
#'       for publication. In other words, these totals are preferred for secondary suppression.
#'
#' @return A data.frame 
#' @keywords internal
#' @export
#' 
#' @note This function has been made internal since it is new and future non-backward compatible changes may occur.
#' 
#' @seealso \code{\link{AdditionalSuppression}}
#'
#' @examples
#' # Generating a dataset spanning four quarters 
#' d2s <- SSBtoolsData("d2s")
#' d <- rbind(d2s, d2s, d2s, d2s)
#' set.seed(10)
#' d$freq[25:96] <- round(d$freq[25:96] + 9 * rnorm(72))
#' d$freq[d$freq < 0] <- 0
#' d$quarter <- rep(c("Q1", "Q2", "Q3", "Q4"), each = 24)
#' 
#' # Redefine the function so that several parameters are preset. 
#' # Also, a simpler function name.   
#' STS <- function(data, ...) {
#'   IncrementalTimeSuppression(data, 
#'           fun = SuppressSmallCounts, 
#'           timeVar = "quarter", 
#'           formula = ~main_income * size, 
#'           maxN = 15, freqVar = "freq", ...)}
#'           
#'           
#' # Default settings without suppressedData as input
#' a1 <- STS(d[1:24, ])   
#' a2 <- STS(d[1:48, ])
#' a3 <- STS(d[1:72, ])
#' a4 <- STS(d, finalTotal = TRUE)
#' 
#' # The quarters and named totals
#' unique(a1$quarter)
#' unique(a2$quarter)
#' unique(a3$quarter)
#' unique(a4$quarter)
#' 
#' # Default settings with suppressedData as input
#' b2 <- STS(d[1:48, ], suppressedData = a1)
#' b3 <- STS(d[1:72, ], suppressedData = b2)
#' b4 <- STS(d, finalTotal = TRUE, suppressedData = b3)
#' 
#' # Without totalPriority, suppression will be the same as before. 
#' # suppressedData makes no difference.
#' # However, if, for example, there is a changed version of the suppression 
#' # algorithm, it may be important to use suppressedData 
#' identical(a2$suppressed, b2$suppressed)
#' identical(a3$suppressed, b3$suppressed)
#' identical(a4$suppressed, b4$suppressed) # totalPriority here, since finalTotal 
#' 
#' # With totalPriority and  all the subtotals 
#' # Note: subtotals are not prioritized
#' c2 <- STS(d[1:48, ], subTotals = TRUE, totalPriority = TRUE)
#' c3 <- STS(d[1:72, ], subTotals = TRUE, totalPriority = TRUE)
#' c4 <- STS(d, subTotals = TRUE, finalTotal = TRUE)
#' unique(c2$quarter)
#' unique(c3$quarter)
#' unique(c4$quarter)
#' 
#' # With such a method, we can see that is important to take into account 
#' # previously published results.
#' # Here this is not done and we see differences.
#' a2[a2$suppressed | c2$suppressed, ]
#' c2[a2$suppressed | c2$suppressed, ]
#' c3[SSBtools::Match( c2[a2$suppressed | c2$suppressed, 1:4], c3[1:4]), ]
#' c4[SSBtools::Match( c2[a2$suppressed | c2$suppressed, 1:4], c4[1:4]), ]
#' 
#' 
#' # Here we take into account previously published results.
#' d2 <- STS(d[1:48, ], subTotals = TRUE, totalPriority = TRUE, suppressedData = a1)
#' d3 <- STS(d[1:72, ], subTotals = TRUE, totalPriority = TRUE, suppressedData = d2)
#' d4 <- STS(d, subTotals = TRUE, finalTotal = TRUE, suppressedData = d3)
#' 
#' SSBtools::SortRows(d2[d2$suppressed, ])
#' SSBtools::SortRows(d3[d3$suppressed, ])
#' 
#' # With such a method, some annual totals must be suppressed
#' SSBtools::SortRows(d4[d4$suppressed, ]) 
#' 
#' # If necessary, several suppressed data sets can be taken into account
#' e4 <- STS(d, finalTotal = TRUE, suppressedData = list(a1, a2))
#' 
IncrementalTimeSuppression <- function(data, fun, timeVar, formula, 
                                       suppressedData = NULL, 
                                       subTotals = FALSE, 
                                       finalTotal = FALSE, 
                                       totalPriority = !isFALSE(finalTotal), ...) {
  times <- sort_num_nchar(unique(data[[timeVar]]))
  
  
  if (subTotals) {
    ncolInput <- ncol(data)
    data <- incremental_time(data, timevar = timeVar)
    subNames <- names(data)[SeqInc(ncolInput + 1, ncol(data))]
    timeVarS <- paste("(", paste(c(timeVar, subNames), collapse = " + "), ")")
    Hidden <- function(crossTable, ...) {
      grepl("N_oT__", crossTable[[timeVar]])
    }
  } else {
    subNames <- integer(0)
    timeVarS <- timeVar
  }
  
  if (length(times) == 1) {
    out <- fun(data, formula = formula, ...)
    out[[timeVar]] <- times
    return(out)
  } else {
    
    GetCandidates = function(..., candidates = GetDefault(fun, "candidates")){
      candidates
    }
    Candidates <- GetCandidates(...)  
    
    if (totalPriority) {
      notPriority <- subNames
    } else {
      notPriority <- c("Total", subNames)
    }
    
    CandidatesTime <- function(..., crossTable, timeV = timeVar, notP = notPriority) {
      candidates <- Candidates(..., crossTable = crossTable)
      timeTotal <- which(crossTable[[timeV]] %in% notP)
      ct <- candidates %in% timeTotal
      c(candidates[!ct], candidates[ct])
    }
    
    
    formula_as_character <- rev(as.character(formula))[1]
    formula_with_time <- as.formula(paste("~", timeVarS, "*(", formula_as_character, ")"))
    if (subTotals) {
      out <- AdditionalSuppressionHere(data, 
                                   fun = fun, 
                                   formula = formula_with_time, 
                                   candidatesTime = CandidatesTime, 
                                   suppressedData = suppressedData, 
                                   hidden = Hidden, ...)
    } else {
      out <- AdditionalSuppressionHere(data, 
                                   fun = fun, 
                                   formula = formula_with_time, 
                                   candidatesTime = CandidatesTime, 
                                   suppressedData = suppressedData, ...)
    }
    time_names <- incremental_time_names(data, timeVar)
    if (is.logical(finalTotal)) {
      if (!finalTotal) {
        out[[timeVar]][out[[timeVar]] == "Total"] <- rev(time_names)[1]
      }
    } else {
      out[[timeVar]][out[[timeVar]] == "Total"] <- finalTotal
    }
  }
  out <- out[!grepl("N_oT__", out[[timeVar]]), , drop = FALSE]
  rownames(out) <- NULL
  out
}

AdditionalSuppressionHere <- function(..., candidates, candidatesTime){
  AdditionalSuppression(..., candidates = candidatesTime)
}


#  Example: incremental_time(d,"quarter")
incremental_time <- function(data, timevar, allnames = FALSE, first = FALSE, last = FALSE, sep = "_") {
  times <- sort_num_nchar(unique(data[[timevar]]))
  if (!last) {
    times <- times[seq_len(length(times) - 1)]
  }
  if (!length(times)) {
    return(data)
  }
  
  tmatrix <- matrix(FALSE, nrow(data), length(times))
  
  tnames <- incremental_time_names(data, timevar = timevar, all = allnames, sep = sep)[seq_len(length(times))]
  
  colnames(tmatrix) <- names(tnames)
  
  for (i in seq_along(times)) {
    if (i == 1) {
      tmatrix[, i] <- data[[timevar]] == times[i]
    } else {
      tmatrix[, i] <- tmatrix[, i - 1] | data[[timevar]] == times[i]
    }
  }
  tframe <- as.data.frame(tmatrix)
  for (i in seq_along(times)) {
    tframe[[i]] <- paste0(c("N_oT__", "")[1L + as.integer(tframe[[i]])], tnames[i])
  }
  if (!first) {
    tframe <- tframe[-1]
  }
  cbind(data, tframe)
}


# Example: incremental_time_names(d,"quarter")
incremental_time_names <- function(data, timevar, all = FALSE, sep = "_") {
  times <- sort_num_nchar(unique(data[[timevar]]))
  time_names <- times
  for (i in seq_along(times)) {
    if (i == 1) {
      time_names[i] <- paste(timevar, times[i], sep = sep)
    } else {
      if (all) {
        time_names[i] <- paste(time_names[i - 1], times[i], sep = sep)
      } else {
        time_names[i] <- paste(time_names[1], times[i], sep = sep)
      }
    }
  }
  names(time_names) = time_names
  time_names[1] <- times[1]
  time_names
}


# Same as in i vignettes/GaussKable 
sort_num_nchar <- function(x) {
  numx <- suppressWarnings(as.numeric(x))
  
  intx <- suppressWarnings(as.integer(x))
  intx[intx != numx] <- NA
  nonnegint <- !is.na(intx)
  nonnegint[nonnegint][numx[nonnegint] < 0] <- FALSE
  
  signx <- sign(numx)
  signx[is.na(signx)] <- 2
  signx[signx == 0] <- 1
  
  numnchar <- pmax(1, ceiling(log10(abs(numx)) + 1e-12))
  numnchar[is.na(numnchar)] <- 0
  numnchar[nonnegint] <- pmax(nchar(x[nonnegint]), numnchar[nonnegint])
  
  numx[is.na(numx)] <- 0
  
  df <- data.frame(signx, numnchar, numx, x)
  ord <- SortRows(df, index.return = TRUE)
  
  x[ord]
}






