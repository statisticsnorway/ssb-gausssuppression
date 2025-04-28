BackTrackingGauss <- function(x, table_memberships, ..., 
                              candidates = 1:ncol(x), primary = NULL, forced = NULL, hidden = NULL, 
                              singletonMethod = "anySum", xExtraPrimary = NULL, whenPrimaryForced = warning, 
                              sequential = TRUE, iterBackTracking = Inf){
  
  if (nrow(table_memberships) != ncol(x)) {
    stop("nrow(table_memberships) != ncol(x)")
  }
  if(!is.null(xExtraPrimary)){
    stop("xExtraPrimary must be NULL in BackTrackingGauss")
  }
  if (singletonMethod != "none") {
    stop("For now singletonMethod must be none in BackTrackingGauss")
  }
  
  if (is.logical(primary)) 
    primary <- which(primary) 
  else 
    primary <- unique(primary)
  
  
  if (!length(primary)) 
    return(integer(0))
  
  if (is.logical(candidates)) 
    candidates <- which(candidates) 
  else 
    candidates <- unique(candidates)
  
  if (is.logical(hidden)) 
    hidden <- which(hidden) 
  else 
    hidden <- unique(hidden)
  
  if (is.logical(forced)) 
    forced <- which(forced) 
  else forced <- unique(forced)
  
  if (length(hidden)) 
    candidates <- candidates[!(candidates %in% hidden)]
  
  if (!is.null(whenPrimaryForced)) {
    if (any(primary %in% forced)) {
      whenPrimaryForced("Primary suppression of forced cells ignored")
    }
  }
  
  
  table_x <- vector("list", ncol(table_memberships))
  orig_col <- vector("list", ncol(table_memberships))

  
  for (i in seq_along(table_x)) {
    ti <- table_memberships[, i]
    dd <- SSBtools::DummyDuplicated(x[, ti, drop = FALSE], idx = FALSE, rows = TRUE, rnd = TRUE)
    table_x[[i]] <- x[!dd, ti, drop = FALSE]
    orig_col_i <- seq_len(ncol(x))[ti]
    colsi <- colSums(abs(table_x[[i]])) != 0
    table_x[[i]] <- table_x[[i]][, colsi, drop = FALSE]
    orig_col_i <- orig_col_i[colsi]
    
    orig_col[[i]] <- orig_col_i
  }
  
  fix_by_table_memberships <- function(indices, orig_col) {
    if (!length(indices)) {
      return(indices)
    }
    ma <- match(orig_col, indices)
    indices_new <- which(!is.na(ma))
    indices_new <- indices_new[order(ma[!is.na(ma)])]
    indices_new
  }
  
  fix_by_orig_col <- function(orig_col, indices) {
    fix_by_table_memberships(indices, orig_col)
  }
  
  primary_input <- primary
  
  candidates <- lapply(orig_col, fix_by_orig_col, candidates)
  primary <- lapply(orig_col, fix_by_orig_col, primary)
  forced <- lapply(orig_col, fix_by_orig_col, forced)
  hidden <- lapply(orig_col, fix_by_orig_col, hidden)
  secondary  <- lapply(orig_col, fix_by_orig_col, integer(0)) 
  
  
  suppressed_col <- vector("list", ncol(table_memberships))
  
  
  find_suppressed_col <- function(orig_col, primary, secondary){
    orig_col[c(primary, secondary)]
  }
  
  for(i in seq_along(suppressed_col)){
    suppressed_col[[i]] <- find_suppressed_col(orig_col[[i]], primary[[i]], secondary[[i]])
  }
  
  back_track <- function(i) {
    suppressed_i <- suppressed_col[[i]] 
    suppressed_not_i <- unique(unlist(suppressed_col[-i]))
    suppressed_extra = orig_col[[i]][orig_col[[i]] %in% suppressed_not_i]
    suppressed_extra = suppressed_extra[!(suppressed_extra %in% suppressed_i)]
    
    if(length(suppressed_extra)){
      return(fix_by_orig_col(orig_col[[i]], c(suppressed_i, suppressed_extra)))
    }
    return(NULL)
  }
  
  
  rerun <- TRUE
  
  iter <- 0
  while (rerun) {
    rerun <- FALSE
    iter <- iter + 1
    i_secondary <- integer(0)
    for (i in seq_along(table_x)) {
      back_track_i <- back_track(i)
      
      if (!is.null(back_track_i)) {
        primary[[i]] <- back_track_i
      }
      if (!is.null(back_track_i) | iter == 1) {
        secondary[[i]] <- GaussSuppression(x = table_x[[i]], table_memberships = NULL, ..., 
                                           candidates = candidates[[i]], primary = primary[[i]], 
                                           forced = forced[[i]], hidden = hidden[[i]], 
                                           singletonMethod = singletonMethod, xExtraPrimary = xExtraPrimary)
        if (sequential) {
          suppressed_col[[i]] <- find_suppressed_col(orig_col[[i]], primary[[i]], secondary[[i]])
        } else {
          i_secondary <- c(i_secondary, i)
        }
        if (length(secondary[[i]])) {
          rerun <- TRUE
        }
      }
    }
    if (!sequential) {
      for (i in i_secondary) {
        suppressed_col[[i]] <- find_suppressed_col(orig_col[[i]], primary[[i]], secondary[[i]])
      }
    }
    if (iter == iterBackTracking) {
      if (rerun) {
        if (length(unlist(lapply(as.list(seq_along(table_x)), back_track)))) {
          warning("iterBackTracking reached. Inconsistent suppression across common cells within the algorithm.")
        }
        rerun <- FALSE
      }
    }
  }
  
  suppressed <- sort(unique(unlist(suppressed_col)))
  
  suppressed[!(suppressed %in% primary)]
  
}