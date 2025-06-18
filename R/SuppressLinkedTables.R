 

#' Consistent Suppression of Linked Tables 
#' 
#' Provides alternatives to global protection for linked tables through 
#' methods that may reduce the computational burden. 
#' 
#' The reason for introducing the new method `"consistent"`, which has not yet been extensively tested in practice, 
#' is to provide something that works better than `"back-tracking"`, while still offering equally strong protection.
#'
#' Note that for singleton methods of the *elimination* type (see [SSBtools::NumSingleton()]), `"back-tracking"` may lead to 
#' the creation of a large number of redundant secondary cells. This is because, during the method's iterations, 
#' all secondary cells are eventually treated as primary. As a result, protection is applied to prevent a singleton 
#' contributor from inferring a secondary cell that was only included to protect that same contributor.
#' 
#' The combination of intervals with the various linked table strategies is not yet implemented, 
#' so the 'lpPackage' parameter is currently ignored.
#'
#' @inheritParams AdditionalSuppression
#' @param data The `data` argument to `fun`. When NULL `data` must be included in  `withinArg`.
#' @param ... Arguments to `fun` that are kept constant.
#' @param withinArg A list of named lists. Arguments to `fun` that are not kept constant.
#' @param linkedGauss Specifies the strategy for protecting linked tables. Possible values are:
#'
#' - `"consistent"` (default): All linked tables are protected by a single call to `GaussSuppression()`. 
#'    The algorithm internally constructs a block diagonal model matrix and handles common cells 
#'    consistently across tables.
#' 
#' - `"local"`: Each table is protected independently by a separate call to `GaussSuppression()`.
#'
#' - `"back-tracking"`: Iterative approach where each table is protected via `GaussSuppression()`, 
#'    and primary suppressions are adjusted based on secondary suppressions from other tables across 
#'    iterations.
#'
#' - `"local-bdiag"`: Produces the same result as `"local"`, but uses a single call to 
#'   `GaussSuppression()` with a block diagonal matrix. It does not apply the linked-table methodology.
#' 
#' @param recordAware If `TRUE` (default), the suppression procedure will ensure consistency 
#'                    across cells that aggregate the same underlying records, 
#'                    even when their variable combinations differ.
#'                    When `TRUE`, `data` cannot be included in  `withinArg`.
#' @param iterBackTracking Maximum number of back-tracking iterations.
#' @param whenEmptyUnsuppressed Parameter to \code{\link[SSBtools]{GaussSuppression}}.
#' @param lpPackage Currently ignored. If specified, a warning will be issued.
#'
#' @return List of data frames
#' @importFrom SSBtools NumSingleton
#' @export
#'
#' @examples
#' 
#' ### The first example can be performed in three ways
#' ### Alternatives are possible since only the formula parameter varies between the linked tables
#'  
#' a <- SuppressLinkedTables(data = SSBtoolsData("magnitude1"), # With trick "sector4 - sector4" and 
#'                  fun = SuppressDominantCells,        # "geo - geo" to ensure same names in output
#'                  withinArg = list(list(formula = ~(geo + eu) * sector2 + sector4 - sector4), 
#'                                   list(formula = ~eu:sector4 - 1 + geo - geo), 
#'                                   list(formula = ~geo + eu + sector4 - 1)), 
#'                  dominanceVar  = "value", 
#'                  pPercent = 10, 
#'                  contributorVar = "company",
#'                  linkedGauss = "consistent")
#' print(a)  
#' 
#' # Alternatively, SuppressDominantCells() can be run directly using the linkedGauss parameter  
#' a1 <- SuppressDominantCells(SSBtoolsData("magnitude1"), 
#'                formula = list(table_1 = ~(geo + eu) * sector2, 
#'                               table_2 = ~eu:sector4 - 1,
#'                               table_3 = ~(geo + eu) + sector4 - 1), 
#'                dominanceVar = "value", 
#'                pPercent = 10, 
#'                contributorVar = "company", 
#'                linkedGauss = "consistent")
#' print(a1)
#' 
#' # In fact, tables_by_formulas() is also a possibility
#' a2 <- tables_by_formulas(SSBtoolsData("magnitude1"),
#'                table_fun = SuppressDominantCells, 
#'                table_formulas = list(table_1 = ~region * sector2, 
#'                                     table_2 = ~region1:sector4 - 1, 
#'                                     table_3 = ~region + sector4 - 1), 
#'                substitute_vars = list(region = c("geo", "eu"), region1 = "eu"), 
#'                collapse_vars = list(sector = c("sector2", "sector4")), 
#'                dominanceVar  = "value", 
#'                pPercent = 10, 
#'                contributorVar = "company",
#'                linkedGauss = "consistent") 
#' print(a2)                 
#'                
#'                
#'                
#'                
#' ####  The second example cannot be handled using the alternative methods.
#' ####  This is similar to the (old) LazyLinkedTables() example.
#' 
#' z1 <- SSBtoolsData("z1")
#' z2 <- SSBtoolsData("z2")
#' z2b <- z2[3:5]  # As in ChainedSuppression example 
#' names(z2b)[1] <- "region" 
#' # As 'f' and 'e' in ChainedSuppression example. 
#' # 'A' 'annet'/'arbeid' suppressed in b[[1]], since suppressed in b[[3]].
#' b <- SuppressLinkedTables(fun = SuppressSmallCounts,
#'               linkedGauss = "consistent",  
#'               recordAware = FALSE,
#'               withinArg = list(
#'                 list(data = z1, dimVar = 1:2, freqVar = 3, maxN = 5), 
#'                 list(data = z2b, dimVar = 1:2, freqVar = 3, maxN = 5), 
#'                 list(data = z2, dimVar = 1:4, freqVar = 5, maxN = 1)))
#' print(b)        
#'        
SuppressLinkedTables <- function(data = NULL,
                              fun,
                              ..., 
                              withinArg = NULL, 
                              linkedGauss = "consistent",
                              recordAware = TRUE,
                              iterBackTracking = Inf,
                              whenEmptyUnsuppressed = NULL,
                              lpPackage = NULL) {
  
  if (!is.null(lpPackage)) {
    warning("The 'lpPackage' parameter is currently ignored by SuppressLinkedTables().")
  }
  
  SSBtools::CheckInput(linkedGauss, type = "character", 
    alt = c("local", "consistent", "back-tracking", "local-bdiag"), 
    okNULL = FALSE)
  
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

  if (!("fun" %in% names(sysCall))) {
    stop("Argument 'fun' must be specified by name.")
  }
    
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
  
  
  if (linkedGauss == "back-tracking") {
    maxJ <- iterBackTracking * n
  } else {
    maxJ <- n
  }
  
  j <- 0L
  nSuppressedIsPrimary <- 0L

  
  if(linkedGauss == "local-bdiag") {
    dup_id <- NULL
  }
  if(linkedGauss == "back-tracking") {
    iterBackTracking = Inf
  } else {
    iterBackTracking = 0L
  }
  if(linkedGauss ==  "local") {
    iterBackTracking = "local"
  }
  
  secondary <- gaussSuppression_linked_fix_dots(
                                       x_ = list_element(env_list, "x"), 
                                       candidates_ = list_element(env_list, "candidates"), 
                                       primary_ = list_element(env_list, "primary"), 
                                       forced_ = list_element(env_list, "forced"), 
                                       hidden_ = list_element(env_list, "hidden"), 
                                       singleton_ = list_element(env_list, "singleton"), 
                                       singletonMethod_ = list_element(env_list, "singletonMethod"), 
                                       xExtraPrimary_ = list_element(env_list, "xExtraPrimary"),
                                       whenEmptyUnsuppressed = whenEmptyUnsuppressed, 
                                       unsafeAsNegative = TRUE,
                                       dup_id = dup_id,
                                       iterBackTracking = iterBackTracking, 
                                       ...)
  for (i in seq_len(n)) {
    env_list[[i]]$secondary <- secondary[[i]]
  }
  for (i in seq_along(suppressedData)) {
    environment(TailGaussSuppressionFromData) <- env_list[[i]]
    suppressedData[[i]] <- TailGaussSuppressionFromData()
  }
  return(suppressedData)
  
}

# To avoid formal argument "candidates" matched by multiple actual arguments
gaussSuppression_linked_fix_dots <- function(x_,  
                        candidates_, 
                        primary_, 
                        forced_, 
                        hidden_, 
                        singleton_, 
                        singletonMethod_, 
                        xExtraPrimary_,
                        x,
                        candidates, 
                        primary, 
                        forced, 
                        hidden, 
                        singleton, 
                        singletonMethod, 
                        xExtraPrimary,
                        ...){
  gaussSuppression_linked(x = x_, 
                          candidates = candidates_, 
                          primary = primary_, 
                          forced = forced_, 
                          hidden = hidden_, 
                          singleton = singleton_, 
                          singletonMethod = singletonMethod_,  
                          xExtraPrimary = xExtraPrimary_,
                          ...)
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



