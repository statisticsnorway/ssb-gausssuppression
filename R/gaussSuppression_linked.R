# SSBtools::GaussSuppression() generalized to take parameters 
# for multiple tables and duplicate id (cell_grouping) as list input.  
# This is combined before SSBtools::GaussSuppression() is run.
#
#  Code moved from an SSBtools-branch. 
#  Decided to move the code out of SSBtools::GaussSuppression 
#  Below is deleted documentation 
#
# @param cell_grouping Numeric vector indicating suppression group membership.
#        Cells with the same non-zero value belong to the same suppression group,
#        meaning they will be suppressed or non-suppressed together.
#        A value of 0 indicates that the cell is not a member of any suppression group.
#        When used together with `table_memberships`, `cell_grouping` must be set to `TRUE` or `FALSE`;  
#        see `table_memberships` for details. 
# @param table_memberships A `data.frame` with `ncol(x)` rows and one logical column per linked table.  
#      This causes the `x` matrix to be rewritten into a block-diagonal structure, with duplicate rows  
#      and empty columns removed.  
#         When `table_memberships` is specified, `cell_grouping` must also be set to either `TRUE` or `FALSE`.  
#      If `cell_grouping = FALSE`, suppression is handled independently within each table,  
#      as a consequence of the block-diagonal structure,  
#      providing local protection per table.  
#      If `cell_grouping = TRUE`, common cells are protected consistently across tables,  
#      using the same method as when `cell_grouping` is used directly as input.  
#         The final secondary indices relate to the columns in the original input `x`.  
#      A cell is marked as secondary if any of the corresponding common cells are suppressed.  
#         Inconsistent suppression across common cells results in a warning (`cell_grouping = TRUE`)  
#      or a message (`cell_grouping = FALSE`).
gaussSuppression_linked <- function(x, candidates, primary, forced, hidden, 
                                    singleton, singletonMethod, 
                                    xExtraPrimary, 
                                    whenEmptyUnsuppressed = message, 
                                    z = rep(0, ncol(x)),
                                    rangeLimits = data.frame(a = rep(0, ncol(x))),
                                    lpPackage = NULL, 
                                    ..., 
                                    dup_id = NULL,
                                    table_memberships = NULL,
                                    cell_grouping = TRUE,
                                    super_consistent = FALSE, 
                                    linkedIntervals = "local-bdiag", 
                                    iterBackTracking = 0L, 
                                    sequential = TRUE,
                                    printInc = TRUE,
                                    printXdim = FALSE) {
  
  local <- identical(iterBackTracking, "local")
  if (local) {
    iterBackTracking <- 1L
    sequential <- FALSE
  }
  
  #if (!identical(unique(unlist(singletonMethod)), "none")) {
  #  stop("For now singletonMethod must be none in gaussSuppression_linked")
  #}
  if (any(!sapply(xExtraPrimary, is.null))) {
    stop("For now xExtraPrimary must be NULL in gaussSuppression_linked")
  }
  primary <- as_not_logical(primary)
  forced <- as_not_logical(forced)
  hidden <- as_not_logical(hidden)
  
  primary_original_input <- primary
  
  use_cell_grouping <- cell_grouping
  cell_grouping <- NULL
  
  fix_by_table_memberships <- function(indices, orig_col) {
    if (!length(indices)) {
      return(indices)
    }
    ma <- match(orig_col, indices)
    indices_new <- which(!is.na(ma))
    indices_new <- indices_new[order(ma[!is.na(ma)])]
    indices_new
  }
  
  if (!is.null(table_memberships)) {
    if (nrow(table_memberships) != ncol(x)) {
      stop("nrow(table_memberships) != ncol(x)")
    }
    table_x <- vector("list", ncol(table_memberships))
    singleton_recoded <- recode_singleton(singleton, singletonMethod, x)
    singletonMethod <- singleton_recoded$singletonMethod
    singleton <- vector("list", ncol(table_memberships))
    table_x_cnames <- character(0)
    if (iterBackTracking) {
      orig_col <- vector("list", ncol(table_memberships))
    } else {
      orig_col <- integer(0)
      table_id <- integer(0) 
    }
    for (i in seq_along(table_x)) {
      ti <- table_memberships[, i]
      # dd <- DummyDuplicated(x[, ti, drop = FALSE], idx = FALSE, rows = TRUE, rnd = TRUE)
      # table_x[[i]] <- x[!dd, ti, drop = FALSE]
      # replace two code lines above to include singleton
      rrdr <- removeDuplicatedRows(x[, ti, drop = FALSE], 
                                   singleton_recoded$singleton)
      table_x[[i]] <- rrdr$x
      singleton[[i]] <- rrdr$singleton
      orig_col_i <- seq_len(ncol(x))[ti]
      table_id_i <- rep(i, sum(ti))
      colsi <- colSums(abs(table_x[[i]])) != 0
      orig_col_i <- orig_col_i[colsi]
      table_x[[i]] <- table_x[[i]][, colsi, drop = FALSE]
      if (iterBackTracking) {
        orig_col[[i]] <- orig_col_i
      } else {
        table_x_cnames_i <- paste(i, seq_len(ncol(x))[ti], sep = "_")
        table_x_cnames_i <- table_x_cnames_i[colsi]
        table_id_i <- table_id_i[colsi]
        table_x_cnames <- c(table_x_cnames, table_x_cnames_i)
        orig_col <- c(orig_col, orig_col_i)
        table_id <- c(table_id, table_id_i) 
      }
    }
    
    if(!iterBackTracking){
      
      if (use_cell_grouping) {
        cell_grouping <- orig_col
      }
      if ("global" %in% linkedIntervals) {
        x_g <- x
        z_g <- z
        primary_g <- primary
        hidden_g <- hidden
        forced_g <- forced
      }
      z <- z[orig_col]
      rangeLimits <- rangeLimits[orig_col, , drop = FALSE]
      x <- Matrix::bdiag(table_x)
      colnames(x) <- table_x_cnames 
      rm(table_x)
      singleton <- rbind_singleton(singleton)
      
      candidates <- fix_by_table_memberships(candidates, orig_col)
      primary <- fix_by_table_memberships(primary, orig_col)
      forced <- fix_by_table_memberships(forced, orig_col)
      hidden <- fix_by_table_memberships(hidden, orig_col)
      
      ##    candidates <- candidates[order(table_id[candidates])]     ###################################### Maybe choose such order by a parameter 
      
      if (printXdim) {
        cat("{table_memberships}<", nrow(x), "*", ncol(x), ">", sep = "")
        flush.console()
      }  
    } else  {
      singletonMethod <- rep(list(singleton_recoded$singletonMethod), ncol(table_memberships))
    } 
    
  } 
  
  if(iterBackTracking) {
    fix_by_orig_col <- function(orig_col, indices) {
      fix_by_table_memberships(indices, orig_col)
    }
    if (is.null(table_memberships)) {
      orig_col <- dup_id
      # The code was originally written with table_memberships as input. 
      # Thus, the variable name orig_col is used. 
      # However, the same functionality can be used with dup_id as input.
    } else {
      candidates <- lapply(orig_col, fix_by_orig_col, candidates)
      primary <- lapply(orig_col, fix_by_orig_col, primary)
      forced <- lapply(orig_col, fix_by_orig_col, forced)
      hidden <- lapply(orig_col, fix_by_orig_col, hidden)
      x <- table_x
    }
    find_suppressed_col <- function(orig_col, primary, secondary){
      unique(orig_col[c(primary, secondary[secondary>0])])   # secondary>0 since negative means unsafe
    }
    secondary  <- replicate(length(primary), integer(0), simplify = FALSE)
    suppressed_col <- vector("list", length(x))
    primary_input <- primary
    if (!local) {
      for(i in seq_along(suppressed_col)) {
        suppressed_col[[i]] <- find_suppressed_col(orig_col[[i]], primary[[i]], secondary[[i]])
      }
    }
    back_track <- function(i) {
      suppressed_i <- suppressed_col[[i]] 
      suppressed_not_i <- unique(unlist(suppressed_col[-i]))
      suppressed_extra = orig_col[[i]][orig_col[[i]] %in% suppressed_not_i]
      suppressed_extra = suppressed_extra[!(suppressed_extra %in% suppressed_i)]
      return(list(
        new_primary = fix_by_orig_col(orig_col[[i]], c(suppressed_i, suppressed_extra)),
        any_extra = length(suppressed_extra)>0))
    }
    
    
    rerun <- TRUE
    
    if (printInc) {
      if (local) {
        cat_linkedGauss("local")
      } else {
        cat_linkedGauss("back-tracking")
      }
    }
    
    iter <- 0
    while (rerun) {
      rerun <- FALSE
      iter <- iter + 1
      if (!local & printInc) {
        cat("\n   =====   back-tracking iteration", iter, "=====\n")
      }
      i_secondary <- integer(0)
      for (i in seq_along(x)) {
        if (! local) {
          back_track_i <- back_track(i)
          primary[[i]] <- back_track_i$new_primary
          any_extra <- back_track_i$any_extra
        } else {
          any_extra <- FALSE
        }
        if (any_extra | iter == 1) {
          secondary[[i]] <- GaussSuppression(x = x[[i]], ..., 
                                             candidates = candidates[[i]], primary = primary[[i]], 
                                             forced = forced[[i]], hidden = hidden[[i]],
                                             singleton = singleton[[i]],
                                             singletonMethod = singletonMethod[[i]], 
                                             auto_anySumNOTprimary = FALSE, 
                                             printInc = printInc, printXdim = printXdim)
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
      if (local & is.null(table_memberships)) {
        return(secondary)
      }
      if (!sequential) {
        if (local) {
          not_suppressed_col <- suppressed_col
        }
        for (i in i_secondary) {
          suppressed_col[[i]] <- find_suppressed_col(orig_col[[i]], primary[[i]], secondary[[i]])
          if (local) {
            not_suppressed_col[[i]] <- unique(orig_col[[i]][!(orig_col[[i]] %in% suppressed_col[[i]])])
          } else {
            not_suppressed_col[[i]] <- integer(0)
          }
        }
      }
      if (iter == iterBackTracking) {
        if (rerun) {
          if(!local)
            if (any(unlist(lapply(as.list(seq_along(x)), function(i) back_track(i)$any_extra)))) {
              warning("iterBackTracking reached. Inconsistent suppression across common cells within the algorithm.")
            }
          rerun <- FALSE
        }
      }
    }
    
    
    if (is.null(table_memberships)) {
      for(i in seq_along(primary)){
        primary[[i]][!(primary[[i]] %in% primary_input[[i]])]
        unsafe <- -secondary[[i]][secondary[[i]] < 0]
        unsafe <- unsafe[unsafe %in% primary_input[[i]]]
        primary[[i]] <- c(primary[[i]], -unsafe)
      }
      return(primary)
    }
    
    suppressed <- sort(unique(unlist(suppressed_col)))
    if(local){
      not_suppressed <- sort(unique(unlist(not_suppressed_col)))
      if(any(not_suppressed %in% suppressed)){
        message("Inconsistent suppression across common cells within the algorithm.")
      }
    }
    
    suppressed <- suppressed[!(suppressed %in% primary_original_input)]
    
    unsafe_col <- vector("list", length(x))
    # reuse method to capture unsafe 
    for(i in seq_along(unsafe_col)){
      unsafe_col[[i]] <- find_suppressed_col(orig_col[[i]], integer(0), -secondary[[i]])
    }
    unsafe <- sort(unique(unlist(unsafe_col)))
    unsafe <- unsafe[unsafe %in% primary_original_input]
    
    return(c(suppressed, -unsafe))
  }
  
  
    
  n <- length(x)
  
  
  
  if (is.null(table_memberships)) {
    table_id <- NULL
    cumsum_0_ncol_x <- c(0L, cumsum(sapply(x, ncol)))
    candidates_ <- candidates
    for (i in SeqInc(2, n)) {
      candidates_[[i]] <- candidates_[[i]] + cumsum_0_ncol_x[[i]]
      primary[[i]] <- primary[[i]] + cumsum_0_ncol_x[[i]]
      forced[[i]] <- forced[[i]] + cumsum_0_ncol_x[[i]]
      hidden[[i]] <- hidden[[i]] + cumsum_0_ncol_x[[i]]
    }
    candidates_ <- unlist(candidates_)
    primary <- unlist(primary)
    forced <- unlist(forced)
    hidden <- unlist(hidden)
    
    for (i in seq_len(n)) {
      singleton_recoded <- recode_singleton(singleton[[i]], singletonMethod[[i]], x[[i]])
      singleton[[i]] <- singleton_recoded$singleton
      singletonMethod[[i]] <- singleton_recoded$singletonMethod
    }
    if (length(unique(singletonMethod)) != 1) {
      stop("singletonMethod must be unique")
    }
    singletonMethod <- singletonMethod[[1]]
    singleton <- rbind_singleton(singleton)

    x <- Matrix::bdiag(x)
    z <- unlist(z)
    rangeLimits <- SSBtools::RbindAll(rangeLimits)
    
    if (!is.null(dup_id)) {
      fcgac <- fix_cell_grouping_and_candidates(dup_id, candidates, cumsum_0_ncol_x)
      candidates <- fcgac$candidates
      cell_grouping <- fcgac$cell_grouping
    } else {
      candidates <- candidates_
      cell_grouping <- NULL
    }
  }
 
  if (super_consistent) {
    ncol_old <- ncol(x)
    x_ <- cbind(x, x0diff(x, repeated_as_integer(cell_grouping)))
    forced_ <- c(forced, SeqInc(ncol_old + 1, ncol(x_)))
    if (get0("super_consistent_cell_grouping", ifnotfound = FALSE)) {
      cell_grouping_ <- c(cell_grouping, rep(0L, ncol(x_) - ncol_old))
      message('"super_consistent" with "cell_grouping"')
    } else {
      cell_grouping_ <- NULL
    }
    table_id_ <- NULL
  } else {
    x_ <- x
    forced_ <- forced
    cell_grouping_ = cell_grouping
    table_id_ = table_id
  }
  
  if (printInc) {
    if (is.null(cell_grouping)) {
      cat_linkedGauss("local-bdiag")
    } else {
      if (super_consistent) {
        cat_linkedGauss("super-consistent")
      } else {
        cat_linkedGauss("consistent")
      }
    }
  }
  
  secondary <- GaussSuppression(x = x_, 
                                ...,
                                candidates = candidates, 
                                primary = primary, 
                                forced = forced_, 
                                hidden = hidden, 
                                singleton = singleton, 
                                singletonMethod = singletonMethod, 
                                whenEmptyUnsuppressed = whenEmptyUnsuppressed, 
                                cell_grouping = cell_grouping_,
                                table_id = table_id_,
                                auto_anySumNOTprimary = FALSE,
                                auto_subSumAny = FALSE,
                                printInc = printInc,
                                printXdim = printXdim)
  
  unsafe <- -secondary[secondary < 0]
  secondary <- secondary[secondary > 0]
  


  if(!is.null(lpPackage)){
    
    if(sum(rangeLimits, na.rm = TRUE)){
      interval_suppressed <- interval_suppression(x = x, 
                                  candidates = candidates, 
                                  primary = primary, 
                                  secondary = secondary,
                                  forced = forced, 
                                  hidden = hidden, 
                                  singleton = singleton, 
                                  singletonMethod = singletonMethod, 
                                  whenEmptyUnsuppressed = whenEmptyUnsuppressed, 
                                  cell_grouping = {if (linkedIntervals[1] == "local-bdiag") NULL else cell_grouping},
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
        x = x,
        z = z,
        primary = primary,
        secondary = secondary,
        hidden = hidden, 
        forced = forced,
        minVal = NULL,
        allInt = FALSE,
        sparseConstraints = TRUE,
        lpPackage = lpPackage,
        gaussI = TRUE,
        cell_grouping = {if (linkedIntervals[1] == "local-bdiag") NULL else cell_grouping}
      ) 
      gauss_intervals <- as.data.frame(gauss_intervals)
    }
    
  } else {
    gauss_intervals <- NULL
  }
  
  
  if(!is.null(lpPackage) & length(linkedIntervals) > 1){
    short_name_linked = c(`local-bdiag` = "lb", `super-consistent` = "sc", global = "global")
    for(i in SeqInc(2, length(linkedIntervals))){
      if(linkedIntervals[i] %in% c("local-bdiag", "super-consistent")){
        gauss_intervals_extra = as.data.frame(
          ComputeIntervals_(
            x = x,
            z = z,
            primary = primary,
            secondary = secondary,
            hidden = hidden, 
            forced = forced,
            minVal = NULL,
            allInt = FALSE,
            sparseConstraints = TRUE,
            lpPackage = lpPackage,
            gaussI = TRUE,
            cell_grouping = {if (linkedIntervals[i] == "local-bdiag") NULL else cell_grouping}
          )
        )
      }
      if(linkedIntervals[i] %in% c("global")){
        gauss_intervals_extra = as.data.frame(
          ComputeIntervals_(
            x = x_g,
            z = z_g,
            primary = primary_g,
            secondary =  unique(orig_col[secondary]),
            hidden = hidden_g, 
            forced = forced_g,
            minVal = NULL,
            allInt = FALSE,
            sparseConstraints = TRUE,
            lpPackage = lpPackage,
            gaussI = TRUE,
            cell_grouping = NULL 
          )
        )
        gauss_intervals_extra = gauss_intervals_extra[orig_col, , drop = FALSE]  # Possible to move code to avoid this
      }
      
      names(gauss_intervals_extra) = paste(names(gauss_intervals_extra), short_name_linked[linkedIntervals[i]], sep = "_")
      gauss_intervals = cbind(gauss_intervals, gauss_intervals_extra)
    }
  }
  
  
  
  if (is.null(table_memberships)) {
    secondary <- as_list_from_not_logical(secondary, cumsum_0_ncol_x)
    unsafe <- as_list_from_not_logical(unsafe, cumsum_0_ncol_x)
    for(i in seq_along(secondary)){
      secondary[[i]] <- c(secondary[[i]], -unsafe[[i]])
    }
    if (!is.null(gauss_intervals)) {
      return(list(secondary = secondary, 
                  gauss_intervals = as_data_frame_list(gauss_intervals, cumsum_0_ncol_x)))
    } else {
      return(secondary)
    }
  }
  
  not_secondary <- rep(TRUE, length(orig_col))
  not_secondary[secondary] <- FALSE
  secondary_out <- unique(orig_col[secondary])
  not_secondary_out <- unique(orig_col[not_secondary])
  if (use_cell_grouping) {
    message_fun <- warning
  } else {
    message_fun <- message
  }
  if (length(unique(orig_col)) != length(secondary_out) + length(not_secondary_out)) {
    message_fun("Inconsistent suppression across common cells within the algorithm")
  }
  secondary <- c(secondary_out, -unique(orig_col[unsafe]))
  if (!is.null(gauss_intervals)) {
      # same interval calculated several times, to be fixed 
    gauss_intervals <- gauss_intervals_orig_col(gauss_intervals, orig_col)
    return(list(secondary, gauss_intervals))
  }
  secondary
}


as_data_frame_list <- function(x, cumsum_0_ncol_x) {
  n <- length(cumsum_0_ncol_x) - 1
  x_list <- vector("list", n)
  for (i in seq_len(n)) {
    r <- x > cumsum_0_ncol_x[i] & x <= cumsum_0_ncol_x[i + 1]
    x_list[[i]] <- x[SSBtools::SeqInc(cumsum_0_ncol_x[i] + 1, cumsum_0_ncol_x[i + 1]), , drop = FALSE]
  }
  x_list
}

as_list_from_not_logical <- function(x, cumsum_0_ncol_x) {
  n <- length(cumsum_0_ncol_x) - 1
  x_list <- vector("list", n)
  j <- 0L
  for (i in seq_len(n)) {
    r <- x > cumsum_0_ncol_x[i] & x <= cumsum_0_ncol_x[i + 1]
    x_list[[i]] <- x[r] - cumsum_0_ncol_x[i]
  }
  x_list
}


as_not_logical <- function(obj) {
  if (is.list(obj)) {
    return(lapply(obj, as_not_logical))
  }
  if (is.null(obj)) {
    return(integer(0))
  }
  if (is.logical(obj)) {
    obj <- which(obj)
  } else {
    obj <- unique(obj)
  }
  obj
}

# final_fix set to FALSE, since this is part of general functionality
# within SSBtools:::GaussSuppression. Don't need to run twice.
fix_cell_grouping_and_candidates <- function(dup_id, candidates, cumsum_0_ncol_x, final_fix = FALSE) {
  n <- length(dup_id)
  dup_id_un <- unlist(dup_id)
  dup_dup <- unique(dup_id_un[duplicated(dup_id_un)])
  integer_codes <- vector("list", n)
  for (i in seq_len(n)) {
    dup_id[[i]][!(dup_id[[i]] %in% dup_dup)] <- 0L  # non-duplicated to 0L
    integer_codes[[i]] <- dup_id[[i]][candidates[[i]]]
    non0 <- integer_codes[[i]] != 0L
    integer_codes[[i]][non0][rev_duplicated(integer_codes[[i]][non0])] <- 0L  # within-duplicated (with rev) to 0L since common_cell_grouping algorithm ... ok since fixed later   
  }
  ccg <- common_cell_grouping(integer_codes)
  can <- rep(0L, length(ccg$table))
  for (i in seq_len(n)) {
    table_i <- ccg$table == i
    can[table_i] <- candidates[[i]][ccg$ind[table_i]] + cumsum_0_ncol_x[[i]]
  }
  out <- list(candidates = can, cell_grouping = unlist(dup_id))
  if (final_fix) {
    out$candidates <- order_candidates_by_cell_grouping(out$candidates, out$cell_grouping)
  }
  out
}



# copy of SSBtools:::order_candidates_by_cell_grouping
# included here as code documentation
order_candidates_by_cell_grouping <- function(candidates, cell_grouping) {
  candidates_order <- seq_along(candidates)
  cell_grouping_candidates <- cell_grouping[candidates]
  pos_cell_grouping_candidates <- cell_grouping_candidates > 0
  candidates_order_g0 <- candidates_order[pos_cell_grouping_candidates]
  cell_grouping_g0 <- cell_grouping_candidates[pos_cell_grouping_candidates]
  ma <- match(cell_grouping_g0, cell_grouping_g0)
  candidates_order_g0 <- candidates_order_g0[ma]
  candidates_order[pos_cell_grouping_candidates] <- candidates_order_g0
  candidates[order(candidates_order)]
}


# Function to create an order (candidates) for all table cells 
# based on cell grouping and orders from separate tables.
common_cell_grouping <- function(integer_codes) {
  integer_non0 <- lapply(integer_codes, function(x) x[x != 0])
  if (any(sapply(integer_non0, anyDuplicated))) {
    stop("duplicated non-zero integer codes found")
  }
  out_n <- sum(sapply(integer_codes, length))
  out_codes <- vector("integer", out_n)
  out_ind <- vector("integer", out_n)
  out_table <- vector("integer", out_n)
  integer_ind <- integer_codes
  for (i in seq_along(integer_codes)) {
    integer_ind[[i]] <- seq_len(length(integer_codes[[i]]))
  }
  j <- 0L
  nex <- find_next(integer_non0)
  while (nex != 0) {
    for (i in seq_along(integer_codes)) {
      w <- which(integer_non0[[i]] == nex)
      if (length(w)) {
        if (w != 1) {
          integer_non0[[i]] <- c(nex, integer_non0[[i]][-w])
          m <- which(integer_codes[[i]] == nex)
          m0 <- min(which(integer_codes[[i]] > 0))
          if (m > m0) {
            new_order <- c(SeqInc(1, m0 - 1), m, SeqInc(m0, m - 1))
          } else {
            stop("Error in common_cell_grouping()")
          }
          integer_codes[[i]][seq_len(m)] <- integer_codes[[i]][new_order]
          integer_ind[[i]][seq_len(m)] <- integer_ind[[i]][new_order]
        }
        m <- which(integer_codes[[i]] == nex)
        out_codes[j + seq_len(m)] <- integer_codes[[i]][seq_len(m)]
        out_ind[j + seq_len(m)] <- integer_ind[[i]][seq_len(m)]
        out_table[j + seq_len(m)] <- i
        j <- j + m
        integer_codes[[i]] <- integer_codes[[i]][-seq_len(m)]
        integer_non0[[i]] <- integer_non0[[i]][-1]
        integer_ind[[i]] <- integer_ind[[i]][-seq_len(m)]
      }
    }
    nex <- find_next(integer_non0)
  }
  for (i in seq_along(integer_codes)) {
    m <- length(integer_codes[[i]])
    out_codes[j + seq_len(m)] <- integer_codes[[i]]
    out_ind[j + seq_len(m)] <- integer_ind[[i]]
    out_table[j + seq_len(m)] <- i
    j <- j + m
  }
  return(list(table = out_table, ind = out_ind, codes = out_codes))
}


# Function used by common_cell_grouping()
find_next <- function(integer_non0) {
  pos_length <- sapply(integer_non0, length) > 0
  if (any(!pos_length)) {
    integer_non0 <- integer_non0[pos_length]
  }
  if (!length(integer_non0)) {
    return(0L)
  }
  a <- vector("integer", length(integer_non0))
  for (i in seq_along(integer_non0)) {
    a[i] <- integer_non0[[i]][1]
  }
  a <- unique(a)
  if (length(a) == 1) {
    return(a)
  }
  for (i in seq_along(integer_non0)) {
    integer_non0[[i]] <- integer_non0[[i]][-1]
  }
  aa <- a
  for (i in seq_along(integer_non0)) {
    aa <- aa[!(aa %in% integer_non0[[i]])]
  }
  if (length(aa)) {
    return(aa[1])
  }
  b <- rep(0L, length(a))
  for (j in seq_along(a)) {
    for (i in seq_along(integer_non0)) {
      b[j] <- max(b[j], which(integer_non0[[i]] == a[j]))
    }
  }
  a[which.min(b)]
}


rev_duplicated <- function(x) {
  rev(duplicated(rev(x)))
}





recode_singleton <- function(singleton, singletonMethod, x){
  # START code copy from SSBtools::GaussSuppression
  if(is.null(singleton)){
    singleton <- rep(FALSE, nrow(x))
  }
  if (is.list(singleton)){
    if(!identical(as.vector(sort(names(singleton))), c("freq", "num"))){
      stop('names of singleton when list must be "freq" and "num"')
    }
    if(!identical(as.vector(sort(names(singleton))), c("freq", "num"))){
      stop('names of singletonMethod when several must be "freq" and "num"')
    }
    singleton_num <- singleton[["num"]]
    singleton <- as.logical(singleton[["freq"]])
    singletonMethod_num <- singletonMethod[["num"]] 
    singletonMethod <- singletonMethod[["freq"]]
  } else {
    if (is.logical(singleton)) {
      if(length(singleton) == 1L){
        singleton <- rep(singleton, nrow(x))
      }
    }
    if(is.integer(singleton)){
      singleton_num <- singleton
      singleton <- as.logical(singleton)
    } else {
      singleton_num <- singleton
    }
    if (!is.logical(singleton)) {
      stop("singleton must be logical or integer")
    }
    if (singletonMethod %in% c("sub2Sum") | !is.null(NumSingleton(singletonMethod))) {
      singletonMethod_num <- singletonMethod
      singletonMethod <- "none"
    } else {
      singletonMethod_num <- "none"
    }
  }
  # END code copy from SSBtools::GaussSuppression
  list(singleton = list(freq = singleton, num = singleton_num), 
       singletonMethod = list(freq = singletonMethod, num = singletonMethod_num))
} 


rbind_singleton <- function(singleton) {
  singleton_freq <- unlist(lapply(singleton, function(x) x$freq))
  singleton_num <- lapply(singleton, function(x) x$num)
  is_logical <- unique(sapply(singleton_num, is.logical))
  if (length(is_logical) > 1) {
    stop("unique singleton-num class needed")
  }
  if (!is_logical) {
    cum_max <- cumsum(sapply(singleton_num, max))
    for (i in SeqInc(2, length(singleton))) {
      s <- singleton_num[[i]]
      s[s > 0] <- s[s > 0] + cum_max[i - 1]
      singleton_num[[i]] <- s
    }
  }
  singleton_num <- unlist(singleton_num)
  list(freq = singleton_freq, num = singleton_num)
}


removeDuplicatedRows <- function(x, singleton) {
  
  
  row_filter <- rowSums(x) > 0
  x <- x[row_filter, , drop = FALSE]
  singleton_num <- singleton[["num"]][row_filter]
  singleton <- singleton[["freq"]][row_filter]
  
  
  # START code copy from SSBtools::GaussSuppression
  
  #  Duplicated non-singleton rows are removed.
  row_filter <- rep(TRUE, nrow(x))
  if (any(singleton)) {
    row_filter[singleton] <- FALSE
  }
  if (any(singleton_num)) {
    row_filter[as.logical(singleton_num)] <- FALSE
  }
  if (any(row_filter)) {
    row_filter[row_filter] <- DummyDuplicated(x[row_filter, , drop = FALSE], idx = FALSE, rows = TRUE, rnd = TRUE)
    if (any(!row_filter)) {
      if (any(singleton)) {
        singleton <- singleton[!row_filter]
      }
      if (any(singleton_num)) {
        singleton_num <- singleton_num[!row_filter]
      }
      x <- x[!row_filter, , drop = FALSE]
    }
  }
  
  #  Duplicated singleton (for frequency tables) rows are removed.
  if (any(singleton)) {
    row_filter <- singleton
    row_filter[row_filter] <- DummyDuplicated(x[row_filter, , drop = FALSE], idx = FALSE, rows = TRUE, rnd = TRUE)
    if (any(row_filter)) {
      x <- x[!row_filter, , drop = FALSE]
      singleton <- singleton[!row_filter]
      if (any(singleton_num))
        singleton_num <- singleton_num[!row_filter]
    }
  }
  
  
  #  Some duplicated singleton (for magnitude tables) rows are removed.
  if (any(singleton_num)) {
    row_filter <- as.logical(singleton_num)
    dd_idx <- DummyDuplicated(x[row_filter, , drop = FALSE], idx = TRUE, rows = TRUE, rnd = TRUE)
    
    
    # First remove duplicates seen from both singleton integers and rows of x
    # After this, the remaining problem is the same, whether singleton_num is logical or integer.
    if (!is.logical(singleton_num)) {
      duplicated2 <- duplicated(cbind(dd_idx, singleton_num[row_filter]))
      row_filter[row_filter] <- duplicated2
      if (any(row_filter)) {
        x <- x[!row_filter, , drop = FALSE]
        singleton_num <- singleton_num[!row_filter]
        if (any(singleton))
          singleton <- singleton[!row_filter]
        dd_idx <- dd_idx[!duplicated2]
      }
      row_filter <- as.logical(singleton_num)
    }
    
    # A group of replicated rows with more than three contributors is not 
    # related to singleton disclosures protected by any of the methods. 
    # Singleton marking can be removed, 
    # and duplicates can also be eliminated. 
    # Note that removing duplicates while retaining singleton marking will 
    # result in incorrect calculations of the number of unique contributors.
    table_dd_idx <- table_all_integers(dd_idx, max(dd_idx))
    least3 <- dd_idx %in% which(table_dd_idx > 2)
    if (any(least3)) {
      row_filter[row_filter] <- least3
      dd_idx <- dd_idx[least3]
      
      duplicated4 <- duplicated(dd_idx)
      
      singleton_num[row_filter] <- FALSE  # i.e. set 0 when not logical
      row_filter[row_filter] <- duplicated4
      x <- x[!row_filter, , drop = FALSE]
      singleton_num <- singleton_num[!row_filter]
      if (any(singleton))
        singleton <- singleton[!row_filter]
    }
  }
  
  # Checks for errors in the code above
  if (any(singleton)) 
    if (length(singleton) != nrow(x)) 
      stop("removeDuplicatedRows failed")
  if (any(singleton_num)) 
    if (length(singleton_num) != nrow(x)) 
      stop("removeDuplicatedRows failed")
  
  # END code copy from SSBtools::GaussSuppression
  
  
  # since cases with !any(singleton) ignored above
  if (!any(singleton)) 
    singleton <- rep(FALSE, nrow(x))  
  if (!any(singleton_num)) 
    singleton_num <- rep(FALSE, nrow(x))
  
  list(x = x,
       singleton = list(freq = singleton, num = singleton_num))
}


cat_linkedGauss <- function(linkedGauss = "consistent") {
  cat('\n====== Linked GaussSuppression by "', linkedGauss, '" algorithm:\n\n', sep = "")
}





x0diff <- function(x, cell_grouping){
  uc = unique(cell_grouping)
  uc = uc[uc!=0]
  
  ma = match(uc, cell_grouping)
  xuc = x[ , ma,drop = FALSE]
  m = x[, integer(0),drop=FALSE]
  
  cell_grouping[ma[!is.na(ma)]] <- 0L
  
  while(any(cell_grouping != 0)){
    ma = match(uc, cell_grouping)
    m = cbind(m, xuc[ , !is.na(ma),drop = FALSE] - x[ , ma[!is.na(ma)],drop = FALSE])
    cell_grouping[ma[!is.na(ma)]] <- 0L 
  }
  m[ , !DummyDuplicated(m, rnd = TRUE)]
}


# Copy of SSBtools:::repeated_as_integer
# Replaces non-zero elements occurring at least twice 
# with unique integer group codes; all others become 0.
repeated_as_integer <- function(a) {
  a_non0 <- a[a != 0]
  a_dup <- a %in% unique(a_non0[duplicated(a_non0)])
  b <- rep(0L, length(a))
  b[a_dup] <- as.integer(factor(a[a_dup]))
  b
}



# Function to make sure maximum of lower bounds 
#                   and minimum of upper bounds 
gauss_intervals_orig_col = function(gauss_intervals, orig_col){
  ma <- match(seq_len(max(orig_col)), orig_col)
  gauss_intervals_out <- gauss_intervals[ma, ]
  for(i in which(substr(names(gauss_intervals),1,2) == "lo")){
    gauss_intervals_out[[i]] = order_matched(gauss_intervals[[i]], orig_col, TRUE)
  }
  for(i in which(substr(names(gauss_intervals),1,2) == "up")){
    gauss_intervals_out[[i]] = order_matched(gauss_intervals[[i]], orig_col, FALSE)
  }
  gauss_intervals_out
}

order_matched <- function(x, orig_col, decreasing = FALSE) {
  ord <- order(x, decreasing = decreasing)
  x <- x[ord]
  orig_col <- orig_col[ord]
  ma <- match(seq_len(max(orig_col)), orig_col) 
  x[ma]
}



ComputeIntervals_ <- function(..., x, primary, secondary, hidden, forced) {
  suppressed <- rep(FALSE, ncol(x))
  suppressed[primary] <- TRUE
  suppressed[secondary] <- TRUE
  suppressed[hidden] <- TRUE  # in interval computation, hidden similar to secondary
  suppressed[forced] <- FALSE
  ComputeIntervals(..., x = x, primary = primary, suppressed = suppressed)
}

  
short_name_linked = c(`local-bdiag` = "lb", `super-consistent` = "sc", global = "global")
  
  

