 

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
#' Note that the frequency singleton methods `"subSpace"`, `"anySum0"`, and `"anySumNOTprimary"` are currently not implemented 
#' and will result in an error. 
#' As a result, the `singletonZeros` parameter in the `SuppressDominantCells()` function cannot be set to `TRUE`, 
#' and the `SuppressKDisclosure()` function is not available for use.
#' Also note that automatic forcing of `"anySumNOTprimary"` is disabled. 
#' That is, [SSBtools::GaussSuppression()] is called with `auto_anySumNOTprimary = FALSE`. 
#' See the parameter documentation for an explanation of why `FALSE` is required.
#' 
#' @note Note on differences between `SuppressLinkedTables()` and alternative approaches.  
#' By *alternatives*, we refer to using the `linkedGauss` parameter via `GaussSuppressionFromData()`, its wrappers, or through `tables_by_formulas()`, as shown in the examples below.
#'
#' - Alternatives can be used when only the `formula` parameter varies between the linked tables.
#' - `SuppressLinkedTables()` creates several smaller model matrices, which may be combined into a single block-diagonal matrix. A large overall matrix is never created.
#' - With the alternatives, a large overall matrix is created first. Smaller matrices are then derived from it. If the size of the full matrix is a bottleneck, `SuppressLinkedTables()` is the better choice.
#' - The `"global"` method is available with the alternatives, but not with `SuppressLinkedTables()`.
#' - The `collapseAware` parameter is supported by the alternatives, but not by `SuppressLinkedTables()`. This option may improve coordination across tables. See [GaussSuppressionFromData()]. 
#' - Due to differences in candidate ordering, the two methods may not always produce identical results. With the alternatives, candidate order is constructed globally across all cells (as with the global method).  
#'   In contrast, `SuppressLinkedTables()` uses a locally determined candidate order within each table.  The ordering across tables 
#'   is coordinated to ensure the method works, but it is not based on a strictly defined global order.  
#'   This may lead to some differences.
#' - With the alternatives, `linkedIntervals` may also contain `"global"`. 
#'   See the documentaion of the `linkedIntervals` parameter above and in [GaussSuppressionFromData()].
#' 
#'
#' @inheritParams AdditionalSuppression
#' @param data The `data` argument to `fun`. When NULL `data` must be included in  `withinArg`.
#' @param ... Arguments to `fun` that are kept constant.
#' @param withinArg A list of named lists. Arguments to `fun` that are not kept constant.
#'                  If `withinArg` is named, the names will be used as names in the output list.
#' @param linkedGauss Specifies the strategy for protecting linked tables. 
#'        The `"super-consistent"`, `"consistent"`, and `"local-bdiag"` methods 
#'        protect all linked tables together in a single call to `GaussSuppression()` 
#'        using an internally constructed block-diagonal model matrix.
#'
#' - `"super-consistent"` (default): Shares the key property of `"consistent"` that
#'   common cells are suppressed equally across tables, but also exploits the fact
#'   that these cells have identical values in all tables. The coordination is
#'   therefore stronger. If intervals are calculated using such coordination,
#'   common cells will have identical interval bounds in each table.
#'
#' - `"consistent"`: Common cells are suppressed equally across tables.
#'
#' - `"local"`: Each table is protected independently by a separate call to
#'   `GaussSuppression()`.
#'
#' - `"back-tracking"`: Iterative approach where each table is protected via
#'   `GaussSuppression()`, and primary suppressions are adjusted based on
#'   secondary suppressions from other tables across iterations.
#'
#' - `"local-bdiag"`: Produces the same result as `"local"`, but uses a single call
#'   to `GaussSuppression()`. It does not apply the linked-table methodology.
#'   
#'        
#' @param linkedIntervals This parameter controls how interval calculations, 
#'         triggered by the `lpPackage` parameter, are performed.
#'  
#' -  **Default:** `"local-bdiag"` if `linkedGauss` is set to `"local-bdiag"`, 
#'    and `"super-consistent"` in all other cases.      
#'         
#'  - Possible values of **`linkedIntervals`** are `"local-bdiag"` and `"super-consistent"`.
#'    
#'  - Interval calculations can be performed when **`linkedGauss`** is `"super-consistent"`, `"consistent"`, or `"local-bdiag"`.
#'         
#'  - When `linkedGauss` is `"local-bdiag"`, `"local-bdiag"` is the only allowed value in `linkedIntervals`  
#'    (except that, with the alternative approaches, `"global"` may appear as a later element; 
#'    `"super-consistent"` is never allowed).
#'         
#'  - It is possible to request multiple types of intervals by supplying `linkedIntervals` as a vector.  
#'    Only the first value affects the additional suppression defined by `rangePercent` and/or `rangeMin`.
#'         
#'  - With the alternative approaches (see the note below), `"global"` may also appear in `linkedIntervals`, 
#'    provided it is not the first element. 
#'  
#' @param lpPackage See [GaussSuppressionFromData()].    
#' 
#' @param recordAware If `TRUE` (default), the suppression procedure will ensure consistency 
#'                    across cells that aggregate the same underlying records, 
#'                    even when their variable combinations differ.
#'                    When `TRUE`, `data` cannot be included in  `withinArg`.
#' @param iterBackTracking Maximum number of back-tracking iterations.
#' @param whenEmptyUnsuppressed Parameter to \code{\link[SSBtools]{GaussSuppression}}.
#'   This is about a helpful message  
#'   *"Cells with empty input will never be secondary suppressed. Extend input data with zeros?"*  
#'   Here, the default is set to \code{NULL} (no message), since preprocessing of the model matrix  
#'   may invalidate the assumptions behind this message.
#'
#' @return A list of data frames, or, if `withinArg` is `NULL`, the ordinary output from `fun`.
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
#'     
#'     
#' ##################################       
#' ####  Examples with intervals     
#' ################################## 
#'   
#' lpPackage <- "highs" 
#' if (requireNamespace(lpPackage, quietly = TRUE)) {
#' 
#'   # Common cells occur because the default for recordAware is TRUE
#'   out1 <- SuppressLinkedTables(data = SSBtoolsData("magnitude1"), 
#'                fun = SuppressDominantCells, 
#'                withinArg = list(table_1 = list(dimVar = c("geo", "sector2")), 
#'                                 table_2 = list(dimVar = c("eu", "sector4"))), 
#'                dominanceVar = "value", k = 90, contributorVar = "company", 
#'                lpPackage = lpPackage, rangeMin = 50)
#'   print(out1)
#'                       
#'  
#'  # In the algorithm, common cells occur because recordAware is TRUE, 
#'  # although this is not reflected in the output variables table_1 and table_2
#'  out2 <- tables_by_formulas(data = SSBtoolsData("magnitude1"), 
#'                table_fun = SuppressDominantCells, 
#'                table_formulas = list(table_1 = ~geo * sector2, 
#'                                      table_2 = ~eu * sector4), 
#'                substitute_vars = list(region = c("geo", "eu"), 
#'                                       sector = c("sector2", "sector4")), 
#'                dominanceVar = "value", k = 90, contributorVar = "company", 
#'                linkedGauss = "super-consistent", 
#'                lpPackage = lpPackage, rangeMin = 50, 
#'                linkedIntervals = c("super-consistent", "local-bdiag", "global"))
#'  print(out2)
#'                        
#' } else {
#'   message(paste0("Examples skipped because the '", lpPackage, "' package is not installed."))
#' }  
SuppressLinkedTables <- function(data = NULL,
                              fun,
                              ..., 
                              withinArg = NULL, 
                              linkedGauss = "super-consistent",
                              linkedIntervals = ifelse(linkedGauss == "local-bdiag", "local-bdiag", "super-consistent"),
                              lpPackage = NULL,
                              recordAware = TRUE,
                              iterBackTracking = Inf,
                              whenEmptyUnsuppressed = NULL) {
  
  SSBtools::CheckInput(linkedGauss, type = "character", 
    alt = c("local", "consistent", "back-tracking", "local-bdiag", "super-consistent"), 
    okNULL = FALSE)
  
  if (!is.null(lpPackage)) {
    check_parameter_linkedIntervals(linkedGauss, linkedIntervals)
  }
  
  if (is.null(withinArg)) {
    return(fun(data = data, ...))
  }
  if (!is.null(withinArg)) {
    if (!is.list(withinArg)) {
      stop("withinArg must be a list when non-NULL")
    }
  }
  parentFrame <- parent.frame()
  sysCall <- as.list(match.call())[-1] 
  # sys.call() is similar to match.call, but does not expand the argument name 
  # match.call needed here for unnamed data when fun = GaussSuppressionFromData

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
      if (i == 2) { # To prevent multiple identical unused-dots warnings 
        if (!("action_unused_dots" %in% unlist(lapply(withinArg, names)))) {
          sysCall[["action_unused_dots"]] <- "none"
        }
      }
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
  
  super_consistent <- linkedGauss == "super-consistent" 
  
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
                                       z = list_element(env_list, "z"),
                                       intervalLimits = list_element(env_list, "intervalLimits"),
                                       unsafeAsNegative = TRUE,
                                       dup_id = dup_id,
                                       super_consistent = super_consistent, 
                                       iterBackTracking = iterBackTracking, 
                                       linkedIntervals = linkedIntervals,
                                       lpPackage = lpPackage, 
                                       ...)
  
  
  if (identical(names(secondary), c("secondary", "gauss_intervals"))) {
    for (i in seq_len(n)) {
      env_list[[i]]$secondary <- secondary$secondary[[i]]
      if (!is.null(env_list[[i]]$num)) {
        env_list[[i]]$num <- cbind(env_list[[i]]$num, secondary$gauss_intervals[[i]])
      }
    }
  } else {
    for (i in seq_len(n)) {
      env_list[[i]]$secondary <- secondary[[i]]
    }
  }
  for (i in seq_along(suppressedData)) {
    environment(TailGaussSuppressionFromData) <- env_list[[i]]
    suppressedData[[i]] <- TailGaussSuppressionFromData()
  }
  names(suppressedData) <- names(withinArg)
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



check_parameter_linkedIntervals <- function(linkedGauss, linkedIntervals, ok_global = FALSE) {
  if (anyDuplicated(linkedIntervals)) {
    stop("Duplicates in linkedIntervals")
  }
  for (i in seq_along(linkedIntervals)) {
    check_pl(linkedGauss, linkedIntervals[i], ifelse(i == 1, FALSE, ok_global))
  }
}

check_pl <- function(linkedGauss, linkedIntervals, ok_global) {
  if (!(linkedGauss %in% c("super-consistent", "consistent", "local-bdiag"))) {
    if (ok_global) {
      stop('When intervals, linkedGauss must be "super-consistent", "consistent", "local-bdiag" or "global"')  
    }
    stop('When intervals, linkedGauss must be "super-consistent", "consistent" or "local-bdiag"')
  }
  if (linkedGauss == "local-bdiag") {
    if (linkedIntervals != "local-bdiag") {
      if (ok_global) {
        if (linkedIntervals != "global") {
          stop('When linkedGauss is "local-bdiag", linkedIntervals must be "local-bdiag" or "global"') 
        }
      } else {
        stop('When linkedGauss is "local-bdiag", linkedIntervals must be "local-bdiag"') 
      }
    }
  }
  if (linkedIntervals == "global") {
    if (!ok_global) {
      stop('linkedIntervals cannot be "global"')
    }
  } else {
    if (!(linkedIntervals %in% c("super-consistent", "local-bdiag"))) {
      if (ok_global) {
        stop('linkedIntervals must be "local-bdiag", "super-consistent" or "global"') 
      } else {
        stop('linkedIntervals must be "local-bdiag" or "super-consistent"') 
      }
    }
  }
}


