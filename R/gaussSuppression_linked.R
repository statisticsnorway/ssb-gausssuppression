# SSBtools::GaussSuppression() generalized to take parameters 
# for multiple tables and duplicate id (cell_grouping) as list input.  
# This is combined before SSBtools::GaussSuppression() is run.
gaussSuppression_linked <- function(x, candidates, primary, forced, hidden, 
                                    singleton, singletonMethod, 
                                    xExtraPrimary, 
                                    whenEmptyUnsuppressed, 
                                    ..., 
                                    dup_id = NULL) {
  if (!identical(unique(unlist(singletonMethod)), "none")) {
    stop("For now singletonMethod must be none in gaussSuppression_linked")
  }
  if (any(!sapply(xExtraPrimary, is.null))) {
    stop("For now xExtraPrimary must be NULL in gaussSuppression_linked")
  }
  n <- length(x)
  primary <- as_not_logical(primary)
  forced <- as_not_logical(forced)
  hidden <- as_not_logical(hidden)
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
  x <- Matrix::bdiag(x)
  if (!is.null(dup_id)) {
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
    candidates <- can
    cell_grouping <- unlist(dup_id)
  } else {
    candidates <- candidates_
    cell_grouping <- NULL
  }
  secondary <- GaussSuppression(x = x, 
                                candidates = candidates, 
                                primary = primary, 
                                forced = forced, 
                                hidden = hidden, 
                                singleton = NULL, 
                                singletonMethod = "none", 
                                printInc = TRUE, 
                                whenEmptyUnsuppressed = whenEmptyUnsuppressed, 
                                xExtraPrimary = NULL, 
                                unsafeAsNegative = TRUE, 
                                cell_grouping = cell_grouping)
  
  as_list_from_not_logical(secondary, cumsum_0_ncol_x)
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