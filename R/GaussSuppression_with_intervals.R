

GaussSuppression_with_intervals <- function(x, candidates, primary, forced, hidden, 
                                            singleton, singletonMethod, 
                                            xExtraPrimary, 
                                            whenEmptyUnsuppressed = message, 
                                            rangeLimits = NULL,
                                            lpPackage = NULL, 
                                            ..., 
                                            cell_grouping = NULL, 
                                            table_id = NULL,
                                            printInc = TRUE,
                                            printXdim = FALSE, 
                                            x_interval = x,
                                            z = rep(0, ncol(x_interval)),
                                            forced_interval = forced, 
                                            cell_grouping_interval = cell_grouping) {
  
  primary <- as_not_logical(primary)
  forced <- as_not_logical(forced)
  hidden <- as_not_logical(hidden)
  
  
  secondary <- GaussSuppression(x = x, 
                                ...,
                                candidates = candidates, 
                                primary = primary, 
                                forced = forced, 
                                hidden = hidden, 
                                singleton = singleton, 
                                singletonMethod = singletonMethod, 
                                whenEmptyUnsuppressed = whenEmptyUnsuppressed, 
                                cell_grouping = cell_grouping,
                                table_id = table_id,
                                auto_anySumNOTprimary = FALSE,
                                auto_subSumAny = FALSE,
                                printInc = printInc,
                                printXdim = printXdim)
  
  unsafe <- -secondary[secondary < 0]
  secondary <- secondary[secondary > 0]
  
  
  if(!is.null(lpPackage)){
    
    if (!is.null(rangeLimits)) {
      interval_suppressed <- interval_suppression(x = x_interval, 
                                                  candidates = candidates, 
                                                  primary = primary, 
                                                  secondary = secondary,
                                                  forced = forced_interval, 
                                                  hidden = hidden, 
                                                  singleton = singleton, 
                                                  singletonMethod = singletonMethod, 
                                                  whenEmptyUnsuppressed = whenEmptyUnsuppressed, 
                                                  cell_grouping = cell_grouping_interval,
                                                  ...,
                                                  xExtraPrimary = NULL,
                                                  lpPackage = lpPackage,
                                                  rangeLimits = rangeLimits,
                                                  z = z,
                                                  printInc = printInc,
                                                  printXdim = printXdim,
                                                  auto_anySumNOTprimary = FALSE,
                                                  auto_subSumAny = FALSE)
      secondary <-  interval_suppressed[[1]]
      gauss_intervals <- interval_suppressed[[2]]
    } else {
      gauss_intervals <- ComputeIntervals_(
        x = x_interval,
        z = z,
        primary = primary,
        secondary = secondary,
        hidden = hidden, 
        forced = forced_interval,
        minVal = NULL,
        allInt = FALSE,
        sparseConstraints = TRUE,
        lpPackage = lpPackage,
        gaussI = TRUE,
        cell_grouping = cell_grouping_interval
      ) 
      gauss_intervals <- as.data.frame(gauss_intervals)
    }
    
  } else {
    gauss_intervals <- NULL
  }
  
  
  secondary <- c(secondary, -unsafe)
  
  
  return(list(secondary = secondary, 
              gauss_intervals = gauss_intervals))
  
  
}