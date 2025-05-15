 

#' Consistent Suppression of Linked Tables 
#'
#' @inheritParams AdditionalSuppression
#' @param data The `data` argument to `fun`. When NULL `data` must be included in  `withinArg`.
#' @param ... Arguments to `fun` that are not kept constant.
#' @param withinArg A list of named lists. Arguments to `fun` that are not kept constant.
#' @param linkedGauss  See \link{parameter_linkedGauss}. 
#' @param recordAware  See \link{parameter_linkedGauss}.
#' @param iterBackTracking See \link{parameter_linkedGauss}.
#' @param whenEmptyUnsuppressed Parameter to \code{\link[SSBtools]{GaussSuppression}}.
#'
#' @return List of data frames
#' @export
#'
#' @examples
#'  
#' #### Similar to parameter_linkedGauss example
#' # Trick "sector4 - sector4" and "geo - geo" to ensure same names in output 
#' output <- LinkedSuppression(data = SSBtoolsData("magnitude1"),
#'                  fun = SuppressDominantCells, 
#'                  withinArg = list(list(formula = ~(geo + eu) * sector2 + sector4 - sector4), 
#'                                   list(formula = ~eu:sector4 - 1 + geo - geo), 
#'                                   list(formula = ~geo + eu + sector4 - 1)), 
#'                  dominanceVar  = "value", 
#'                  pPercent = 10, 
#'                  contributorVar = "company",
#'                  singletonMethod = "none", 
#'                  linkedGauss = "consistent") 
#' 
#' ####  Similar to LazyLinkedTables example:
#' z1 <- SSBtoolsData("z1")
#' z2 <- SSBtoolsData("z2")
#' z2b <- z2[3:5]  # As in ChainedSuppression example 
#' names(z2b)[1] <- "region" 
#' # As 'f' and 'e' in ChainedSuppression example. 
#' # 'A' 'annet'/'arbeid' suppressed in b[[1]], since suppressed in b[[3]].
#' b <- LinkedSuppression(fun = SuppressSmallCounts,
#'      linkedGauss = "back-tracking",  
#'      recordAware = FALSE,
#'      withinArg = list(
#'        list(data = z1, dimVar = 1:2, freqVar = 3, maxN = 5), 
#'        list(data = z2b, dimVar = 1:2, freqVar = 3, maxN = 5), 
#'        list(data = z2, dimVar = 1:4, freqVar = 5, maxN = 1)))
LinkedSuppression <- function(fun, 
                              data = NULL, 
                              ..., 
                              withinArg = NULL, 
                              linkedGauss,
                              recordAware = TRUE,
                              iterBackTracking = Inf,
                              whenEmptyUnsuppressed = NULL) {
  SSBtools::CheckInput(linkedGauss, type = "character", alt = c("local", "consistent", "back-tracking", "local-old", "local-bdiag"), okNULL = FALSE)
  
  if (is.null(withinArg)) {
    return(fun(...))
  }
  if (!is.null(withinArg)) {
    if (!is.list(withinArg)) {
      stop("withinArg must be a list when non-NULL")
    }
  }
  parentFrame <- parent.frame()
  sysCall <- as.list(sys.call())[-1]
  
  removeArgs <- c("fun", "linkedGauss", "withinArg")
  if (is.null(data)) {
    removeArgs <- c(removeArgs, "data")
  }
  
  sysCall <- c(sysCall["fun"], sysCall[!(names(sysCall) %in% removeArgs)])
  
  if (recordAware) {
    if (is.null(data)) {
      stop("data as constant parameter needed when recordAware is TRUE")
    }
    data <- cbind(data, rnd_7(nrow(data)))
    sysCall[["data"]] <- data
    sysCall$r_rnd <- paste0("r_rnd_", 1:7)
  }
  
  env_list <- vector("list", length(withinArg))
  
  for (i in seq_along(withinArg)) {
    if (is.null(names(withinArg[[i]]))) {
      if (!is.data.frame(withinArg[[i]])) {
        stop("non-named element of withinArg must be a data frame")
      }
    } else {
      env_list[[i]] <- eval(as.call(c(sysCall, withinArg[[i]], output = "pre_gauss_env")), envir = parentFrame)
    }
  }
  
  n <- length(withinArg)
  
  primary_list <- lapply(env_list, function(x) x$primary)
  secondary_list <- rep(list(integer(0)), n)
  totCode_list <- vector("list", n)
  
  
  
  crossTable_list <- lapply(env_list, function(x) x$crossTable)
  
  suppressedData <- lapply(env_list, function(x) x$crossTable)
  
  for (i in seq_along(suppressedData)) {
    suppressedData[[i]]$suppressed <- primary_list[[i]]
    totCode_list[[i]] <- FindTotCode2(env_list[[i]]$x, crossTable = env_list[[i]]$crossTable)
  }
  
  
  r_rnd_7_list <- lapply(env_list, function(x) x$r_rnd)
  
  
  if (recordAware) {
    dup_id <- duplicated_id_rnd(r_rnd_7_list = r_rnd_7_list, code0 = TRUE, totCode_list = totCode_list, crossTable_list = crossTable_list)
  } else {
    dup_id <- duplicated_id_code(totCode_list = totCode_list, crossTable_list = crossTable_list)
  }
  
  
  if (linkedGauss %in% c("back-tracking", "back-tracking-old")) {
    maxJ <- iterBackTracking * n
  } else {
    maxJ <- n
  }
  
  j <- 0L
  nSuppressedIsPrimary <- 0L
  
  if (linkedGauss %in% c("consistent", "local-bdiag")) {
    if(linkedGauss == "local-bdiag") {
      dup_id <- NULL
    }
    secondary <- gaussSuppression_linked(x = list_element(env_list, "x"), 
                                         candidates = list_element(env_list, "candidates"), 
                                         primary = list_element(env_list, "primary"), 
                                         forced = list_element(env_list, "forced"), 
                                         hidden = list_element(env_list, "hidden"), 
                                         singleton = list_element(env_list, "singleton"), 
                                         singletonMethod = list_element(env_list, "singletonMethod"), 
                                         xExtraPrimary = list_element(env_list, "xExtraPrimary"),
                                         printInc = list_element(env_list, "printInc"), 
                                         whenEmptyUnsuppressed = whenEmptyUnsuppressed, 
                                         unsafeAsNegative = TRUE,
                                         dup_id = dup_id)
    for (i in seq_len(n)) {
      env_list[[i]]$secondary <- secondary[[i]]
    }
    for (i in seq_along(suppressedData)) {
      environment(TailGaussSuppressionFromData) <- env_list[[i]]
      suppressedData[[i]] <- TailGaussSuppressionFromData()
    }
    return(suppressedData)
    
  
  }
  
  

  if (linkedGauss %in% c("back-tracking", "local")) {
    if (!is.logical(primary_list[[1]])) {
      stop("Primary must be logical in current implementation")
    }
    primary_list_updated <- primary_list
    if (linkedGauss == "back-tracking") {
      for (i in seq_along(primary_list)) {
        primary_list_updated <- update_primary_list(primary_list_updated, i, primary_list[[i]], dup_id)
      }
    }
    while (j < maxJ) {
      j <- j + 1L
      i <- 1L + ((j - 1L)%%n)
      cat(i, "\n")
      
      secondary_list[[i]] <- GaussSuppression(x = env_list[[i]]$x, candidates = env_list[[i]]$candidates, primary = primary_list_updated[[i]], forced = env_list[[i]]$forced, hidden = env_list[[i]]$hidden, singleton = env_list[[i]]$singleton,
                                              singletonMethod = env_list[[i]]$singletonMethod, printInc = env_list[[i]]$printInc, whenEmptyUnsuppressed = whenEmptyUnsuppressed, xExtraPrimary = env_list[[i]]$xExtraPrimary, unsafeAsNegative = TRUE)  # dot-dot-dot not include for now 
      
      if (linkedGauss == "back-tracking") {
        primary_list_updated <- update_primary_list(primary_list_updated, i, secondary_list[[i]], dup_id)
      }
      
      if (length(secondary_list[[i]]) == 0) {
        nSuppressedIsPrimary <- nSuppressedIsPrimary + 1L
      } else {
        nSuppressedIsPrimary <- 0L
      }
      if (nSuppressedIsPrimary == n) {
        break
      }
      
    }
    if (linkedGauss == "back-tracking" & nSuppressedIsPrimary != n) {
      warning("Iteration limit exceeded")
    }
    
    for (i in seq_len(n)) {
      if (linkedGauss == "back-tracking") {
        secondary <- primary_list_updated[[i]] & !primary_list[[i]]
        env_list[[i]]$secondary <- which(secondary)
      } else {
        env_list[[i]]$secondary <- secondary_list[[i]] 
      }
    }
    
    for (i in seq_along(suppressedData)) {
      environment(TailGaussSuppressionFromData) <- env_list[[i]]
      suppressedData[[i]] <- TailGaussSuppressionFromData()
    }
    
    return(suppressedData)
    
  }
  
  
  while (j < maxJ) {
    j <- j + 1L
    i <- 1L + ((j - 1L)%%n)
    cat(i, "\n")
    
    if (linkedGauss == "back-tracking-old") {
      suppressedData[[i]]$suppressed[PrimaryFromSuppressedData(x = env_list[[i]]$x, 
                                                               crossTable = env_list[[i]]$crossTable, 
                                                               suppressedData = suppressedData, 
                                                               totCode = totCode_list[[i]])] <- TRUE 
    }
    
    secondary_list[[i]] <- GaussSuppression(x = env_list[[i]]$x, candidates = env_list[[i]]$candidates, primary = suppressedData[[i]]$suppressed, forced = env_list[[i]]$forced, hidden = env_list[[i]]$hidden,
                                            singleton = env_list[[i]]$singleton, singletonMethod = env_list[[i]]$singletonMethod, printInc = env_list[[i]]$printInc, whenEmptyUnsuppressed = whenEmptyUnsuppressed, xExtraPrimary = env_list[[i]]$xExtraPrimary,
                                            unsafeAsNegative = TRUE)   # dot-dot-dot not include for now 
    suppressedData[[i]]$suppressed[secondary_list[[i]]] <- TRUE
    
    if (length(secondary_list[[i]]) == 0) {
      nSuppressedIsPrimary <- nSuppressedIsPrimary + 1L
    } else {
      nSuppressedIsPrimary <- 0L
    }
    if (nSuppressedIsPrimary == n) {
      break
    }
    
  }
  
  if (linkedGauss == "back-tracking-old" & nSuppressedIsPrimary != n) {
    warning("Iteration limit exceeded")
  }
  
  for (i in seq_along(suppressedData)) {
    secondary <- suppressedData[[i]]$suppressed & !primary_list[[i]]
    env_list[[i]]$secondary <- which(secondary)
  }
  
  for (i in seq_along(suppressedData)) {
    environment(TailGaussSuppressionFromData) <- env_list[[i]]
    suppressedData[[i]] <- TailGaussSuppressionFromData()
  }
  
  suppressedData
}


list_element <- function(list_of_lists, element) {
  lapply(list_of_lists, function(x) x[[element]])
}




# Copy of SSBtools:::Sample_Symmetric_integer.maxa 
Sample_Symmetric_integer.max <- function(size, replace = FALSE, n = .Machine$integer.max) {
  a <- sample.int(n = n, size = size, replace = replace)
  s <- sample(c(-1L, 1L), size = size, replace = TRUE)
  as.numeric(s * a)
}

rnd_7 <- function(n, seed = 123) {
  if (!is.null(seed)) {
    if (!exists(".Random.seed"))
      if (runif(1) < 0)
        stop("Now seed exists")
    exitSeed <- .Random.seed
    on.exit(.Random.seed <<- exitSeed)
    set.seed(seed)
  }
  u <- Sample_Symmetric_integer.max(size = n * 7)
  matrix(u, ncol = 7, dimnames = list(NULL, paste0("r_rnd_", 1:7)))
}


# id is a row number
duplicated_id_rnd <- function(r_rnd_7_list, code0 = FALSE, totCode_list = NULL, crossTable_list = NULL, list_again = TRUE) {
  r_rnd_7_all <- SSBtools::RbindAll(r_rnd_7_list)
  nRep <- 7
  nClaim <- 3
  n <- nrow(r_rnd_7_all)
  ma <- matrix(0L, n, nRep)
  for (i in 1:nRep) {
    ma[, i] <- match(r_rnd_7_all[, i], r_rnd_7_all[, i])
  }
  maMax <- apply(ma, 1, max)  # RowMax
  
  min_n_maMax <- min(rowSums(matrix(maMax, n, nRep) == ma))
  
  if (min_n_maMax < nClaim) {
    stop("Duplicated by random method did not work")
  }
  
  if (code0) {
    r0 <- rowSums(abs(r_rnd_7_all)) == 0
    if (any(r0)) {
      maMax[r0] <- 0L
      id_code <- duplicated_id_code(totCode_list, crossTable_list, list_again = FALSE)
      if (any(maMax %in% id_code[r0])) {
        stop("Combined duplicated method did not work")
      }
      maMax[r0] <- id_code[r0]
    }
  }
  if (list_again) {
    return(as_list_again(maMax, r_rnd_7_list))
  }
  maMax
}

# id is a row number
duplicated_id_code <- function(totCode_list, crossTable_list, list_again = TRUE) {
  
  totCode <- combine_code_list(totCode_list)
  
  crossTable_all <- SSBtools::RbindAll(crossTable_list)
  
  for (nam in names(crossTable_all)) {
    crossTable_all[[nam]][crossTable_all[[nam]] %in% c(totCode[[nam]], NA)] <- "ToTA_L_iD"
  }
  ma <- Match(crossTable_all, crossTable_all)
  if (list_again) {
    return(as_list_again(ma, crossTable_list))
  }
  ma
}


combine_code_list <- function(x) {
  a <- x[[1]]
  for (i in SeqInc(2, length(x))) {
    b <- x[[i]]
    for (nam in names(b)) {
      a[[nam]] <- unique(c(a[[nam]], b[[nam]]))
    }
  }
  a
}

as_list_again <- function(x, reference_list) {
  x_list <- vector("list", length(reference_list))
  j <- 0L
  for (i in seq_along(x_list)) {
    ni <- nrow(reference_list[[i]])
    x_list[[i]] <- x[j + seq_len(ni)]
    j <- j + ni
  }
  x_list
}


update_primary_list <- function(primary_list, list_nr, new_primary, dup_id) {
  new_id <- unique(dup_id[[list_nr]][new_primary])
  for (i in seq_along(primary_list)) {
    new_rows <- dup_id[[i]] %in% new_id
    primary_list[[i]][new_rows] <- TRUE
  }
  primary_list
}



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