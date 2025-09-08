
interval_suppression <- function(x, 
                                 candidates,
                                 primary,
                                 secondary, 
                                 forced,
                                 hidden,
                                 singleton,
                                 singletonMethod,
                                 ...,
                                 xExtraPrimary,
                                 whenEmptyUnsuppressed,
                                 cell_grouping,
                                 lpPackage,
                                 intervalLimits,
                                 z,
                                 printInc,
                                 minVal = NULL,
                                 allInt = FALSE,
                                 gaussIFix = FALSE){
  
  if (length(forced)) {
    stop("forced when interval iteration not implemented")
  }
  
  #intervalLimits <- RangeLimitsDefault(..., primary = primary, num = num, freq = freq, freqVar = freqVar)
    
  intervalLimits <- split_by_intervalVar(intervalLimits)
  
  if (length(intervalLimits) != 1) {
    stop("Only single intervalVar implemented")
  }
  
  intervalLimits <- intervalLimits[[1]]
  
  m <- ncol(x)
  
  # Make sure primary is logical
  primary_input <- primary
  primary <- rep(FALSE, m)
  primary[primary_input] <- TRUE 
    
  suppressed <- rep(FALSE, m)
  suppressed[primary] <- TRUE
  suppressed[secondary] <- TRUE
  suppressed[hidden] <- TRUE     # in interval computation, hidden similar to  secondary
  
  suppressed_integer <- rep(0L, m)
  suppressed_integer[primary] <- 1L
  suppressed_integer[secondary] <- 2L
  
  
  gauss_intervals <- ComputeIntervals(
    x = x,
    z = z,
    primary = primary,
    suppressed = suppressed,
    minVal = minVal,
    allInt = allInt,
    lpPackage = lpPackage,
    cell_grouping = cell_grouping
  )
  
  
  risky <- FindRisky(intervalLimits, 
                     lo = gauss_intervals[, 1], 
                     up = gauss_intervals[, 2])
  
  risky[!primary] <- FALSE
  
  
  colnames(gauss_intervals) <- paste(colnames(gauss_intervals), "1", sep = "_")
  num <- as.data.frame(gauss_intervals)
  
  newPrimary <- FixRiskyIntervals(
    x = x,
    z = z,
    primary = risky, 
    suppressed = suppressed,
    candidates = candidates, 
    minVal = minVal,
    allInt = allInt,
    gaussI = gaussIFix,
    lpPackage = lpPackage,
    intervalLimits =  intervalLimits[risky, , drop = FALSE],
    cell_grouping = cell_grouping
  )
  
  primary2 <- primary
  primary2[newPrimary] <- TRUE
  
  suppressed_integer[newPrimary] <- 3L
  suppressed1 <- suppressed
  suppressed2 <- suppressed1
  suppressed2[primary2] <- TRUE
  suppressed <- suppressed2
  
  if (any(cell_grouping>0)) {
    ncol_old <- ncol(x)
    x_ <- cbind(x, x0diff(x, repeated_as_integer(cell_grouping)))
    forced_ <- c(forced, SeqInc(ncol_old + 1, ncol(x_)))
  } else {
    x_ <- x
    forced_ <- forced  
  }
  

  secondary <- GaussSuppression(x = x_, candidates = candidates, primary = suppressed, forced = forced_, hidden = hidden, singleton = NULL, singletonMethod = "none", printInc = printInc, whenEmptyUnsuppressed = NULL, xExtraPrimary = xExtraPrimary, 
                                ...)
  
  suppressed_integer[secondary] <- 4L
  
  
  suppressed[secondary] <- TRUE
  
  
  gauss_intervals <- ComputeIntervals(
    x = x,
    z = z,
    primary = primary,
    suppressed = suppressed,
    minVal = minVal,
    allInt = allInt,
    lpPackage = lpPackage,
    cell_grouping = cell_grouping
  )
  
  
  risky <- FindRisky(intervalLimits, 
                     lo = gauss_intervals[, 1], 
                     up = gauss_intervals[, 2])
  
  risky[!primary] <- FALSE
  
  

  # After introducing FindRisky() there will never be a warning.
  # But that's ok since infinity as an interval limit is actually correct sometimes.
  if (any(is.na(risky))) {
    warning(paste("Missing values in final risk calculation"))
    risky[is.na(risky)] <- FALSE
  }
  
  if(any(risky)){
    warning(paste("Still", sum(risky) ,"risky (Algorithm may iterate in the future)."))
  }
  
  num <- cbind(num, as.data.frame(gauss_intervals))
  
  num <- cbind(num, suppressed_integer = suppressed_integer)
  
  secondary <- which(suppressed & !primary)   # Suppressed is re-calculated in TailGaussSuppressionFromData
  
  list(secondary, num)
}






# Flower since I used that as the term when I asked ChatGPT to create the function
split_by_intervalVar <- function(df) {  ##split_by_flower <- function(df) {
  # allowed prefixes and regex for column names
  pref <- c("rlim", "lomax", "upmin")
  rx <- "^(rlim|lomax|upmin)_(.+)$"
  
  nms <- names(df)
  m <- regexec(rx, nms)
  parts <- regmatches(nms, m)
  
  # collect matches: column index, prefix, and flower
  matches <- do.call(rbind, lapply(seq_along(parts), function(i) {
    p <- parts[[i]]
    if (length(p) == 3) {
      data.frame(col = i, prefix = p[2], flower = p[3], 
                 stringsAsFactors = FALSE)
    }
  }))
  
  if (is.null(matches)) {
    stop("No columns found matching rlim_|lomax_|upmin_.")
  }
  
  # build one sub-data.frame per flower
  flowers <- unique(matches$flower)
  out <- setNames(lapply(flowers, function(fl) {
    s <- matches[matches$flower == fl, ]
    subdf <- df[, s$col, drop = FALSE]
    
    # rename columns to just the prefix (rlim/lomax/upmin)
    names(subdf) <- s$prefix
    
    # reorder columns consistently (rlim, lomax, upmin when present)
    ord <- order(match(names(subdf), pref))
    subdf[, ord, drop = FALSE]
  }), flowers)
  
  out
}


# df <- data.frame(
#   rlim_rose = 1:3,
#   lomax_lily = 4:6,
#   upmin_rose = 7:9,
#   lomax_rose = 10:12,
#   upmin_tulip = 13:15,
#   check.names = FALSE
# )
# 
# split_by_intervalVar(df)


# Related function also written by ChatGPT
# Collapse duplicate rlim_/lomax_/upmin_ columns by rowwise max/min
# NA is ignored unless all values in a row are NA -> then NA
dedupe_range_limits <- function(df) {
  rx <- "^(rlim|lomax|upmin)_"
  nms <- names(df)
  tgt_idx <- which(grepl(rx, nms))
  if (length(tgt_idx) == 0L) return(df)
  
  uniq <- unique(nms[tgt_idx])
  to_drop <- integer(0)
  
  for (nm in uniq) {
    idx <- which(nms == nm)
    if (length(idx) <= 1L) next
    
    prefix <- sub("_.*$", "", nm)
    cols <- lapply(idx, function(i) df[[i]])  # numeric/logical only
    
    # aggregator: lomax -> min, rlim/upmin -> max
    agg_fun <- if (prefix == "lomax") pmin else pmax
    combined <- do.call(agg_fun, c(cols, list(na.rm = TRUE)))
    
    # rows where all duplicates are NA should be NA
    all_na <- Reduce("&", lapply(cols, is.na))
    combined[all_na] <- NA_real_
    
    # keep the first, drop the rest
    df[[idx[1]]] <- combined
    to_drop <- c(to_drop, idx[-1])
  }
  
  if (length(to_drop)) df <- df[, -to_drop, drop = FALSE]
  df
}

# df <- data.frame(
#   rlim_rose = c(1, NA, 3, NA),
#   rlim_rose = c(2,  5, NA, NA),   # rowwise max
#   lomax_lily = c(10, 8, NA, NA),
#   lomax_lily = c( 9, 9, 12, NA),  # rowwise min
#   upmin_tulip = c(NA, 4, 6, NA),
#   upmin_tulip = c( 3, 2, 7, NA),  # rowwise max
#   check.names = FALSE
# )
# dedupe_range_limits(df)
# # rlim_rose   : 2, 5, 3, NA
# # lomax_lily  : 9, 8, 12, NA
# # upmin_tulip : 3, 4, 7, NA












