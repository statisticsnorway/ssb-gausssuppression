#' K-disclosure suppression
#'
#' Frequency table suppression for targeted attribute disclosure protection.
#'
#' The argument `targeting` may also be a function that returns such a list.
#' This works similarly to supplied functions in `GaussSuppressionFromData()`.
#' Note, however, that the function operates on possibly extended versions of
#' `freq`, `x`, and `crossTable` that reflect the use of `mc_hierarchies`, when
#' applicable.
#' 
#' The parameters `identifying` and `sensitive` are included here as explicit
#' arguments, but they are in fact parameters of [default_targeting()].
#' In addition, the `default_targeting()` parameters `targeting_include` and
#' `targeting_exclude` may also be used (see examples).
#'
#' @param data a data.frame representing the data set
#' @param coalition numeric vector of length one, representing possible size of an
#' attacking coalition. This parameter corresponds to the parameter k in the
#' definition of k-disclosure.
#' @param dimVar The main dimensional variables and additional aggregating
#' variables. This parameter can be  useful when hierarchies and formula are
#' unspecified.
#' @param formula A model formula
#' @param hierarchies List of hierarchies, which can be converted by
#' \code{\link[SSBtools]{AutoHierarchies}}. Thus, the variables can also be coded by
#' `"rowFactor"` or `""`, which correspond to using the categories in the data.
#' @param freqVar name of the frequency variable in `data`
#' @param mc_hierarchies a hierarchy representing meaningful combinations to be
#' protected. Default value is `NULL`.
#' @param upper_bound  Numeric value specifying the maximum cell frequency for which
#' disclosure of belonging to the cell may be regarded as
#' unacceptable. When freq > upper_bound, disclosure of
#' belonging to the cell is regarded as acceptable regardless
#' of the specification of the `sensitive` parameter.
#' Default is Inf. Note that this parameter may also be useful
#' for reducing computational burden.
#' @param ... parameters passed to children functions
#' @inheritParams GaussSuppressionFromData
#' @inheritParams default_targeting
#'
#' @param targeting The mechanism underlying the interpretation of
#' `identifying` and `sensitive`. See Details in [KDisclosurePrimary()].
#' 
#' @param print_frames Logical or character. If TRUE, additional data frames are
#' printed to the console. When `mc_hierarchies` is used, this includes a data
#' frame with hidden results. In addition, a data frame containing the primary
#' suppressed difference cells is printed. If set to `"primary_cells"`, only the
#' primary suppressed difference cells are printed. The default is FALSE.

#'
#' @return A data.frame containing the publishable data set, with a boolean
#' variable `$suppressed` representing cell suppressions.
#' @export
#'
#' @author Daniel P. Lupp and Øyvind Langsrud
#' 
#' @references
#' Langsrud (2026).
#' \doi{10.1007/978-3-032-37883-5_8}.
#'
#' Lupp and Langsrud (2023).
#' Disclosure-Driven Suppression in Frequency Tables.
#' Archived unpublished manuscript.
#' \url{https://research.lupp.no/K_disclosure.pdf}.
#'
#' Lupp and Langsrud (2021).
#' Suppression of directly-disclosive cells in frequency tables.
#' Joint UNECE/Eurostat Expert Meeting on Statistical Data Confidentiality,
#' Poznań, Poland.
#' \url{https://unece.org/sites/default/files/2021-12/SDC2021_Day2_Lupp_AD.pdf}.
#'
#' @examples
#' # data
#' mun_a <- SSBtools::SSBtoolsData("mun_accidents")
#' 
#' # Function to print output in wide format, marking suppressed values with `*`
#' show_out <- function(out) {  
#'   out$freq = sprintf("%s%s", out$freq, c(" ","*")[1+out$suppressed])
#'   a <- reshape(out[1:3], idvar = "mun", timevar = "inj", direction = "wide", )
#'   names(a) <- sub("^freq\\.", "", names(a))
#'   print(a)}
#'
#' # hierarchies as DimLists
#' mun <- data.frame(levels = c("@@", rep("@@@@", 6)),
#' codes = c("Total", paste("k", 1:6, sep = "")))
#' inj <- data.frame(levels = c("@@", "@@@@" ,"@@@@", "@@@@", "@@@@"),
#' codes = c("Total", "serious", "light", "none", "unknown"))
#' dimlists <- list(mun = mun, inj = inj)
#'
#' inj2 <- data.frame(levels = c("@@", "@@@@", "@@@@@@" ,"@@@@@@", "@@@@", "@@@@"),
#' codes = c("Total", "injured", "serious", "light", "none", "unknown"))
#' inj3 <- data.frame(levels = c("@@", "@@@@", "@@@@" ,"@@@@", "@@@@"),
#' codes = c( "shadowtotal", "serious", "light", "none", "unknown"))
#' mc_dimlist <- list(inj = inj2)
#' mc_nomargs <- list(inj = inj3)
#'
#' #' # Example with formula, no meaningful combination
#' out <- SuppressKDisclosure(mun_a, coalition = 1, freqVar = "freq", 
#'                            formula = ~mun*inj, print_frames = TRUE)
#' show_out(out)
#'
#' # Example with hierarchy and meaningful combination
#' out2 <- SuppressKDisclosure(mun_a, coalition = 1, freqVar = "freq",
#'                        hierarchies = dimlists, mc_hierarchies = mc_dimlist,
#'                        print_frames = TRUE)
#' show_out(out2)
#'
#' #' # Example of table without mariginals, and mc_hierarchies to protect
#' out3 <- SuppressKDisclosure(mun_a, coalition = 1, freqVar = "freq",
#'                        formula = ~mun:inj, mc_hierarchies = mc_nomargs,
#'                        print_frames = TRUE)
#' show_out(out3)
#' 
#' 
#' ### Examples with identifying and sensitive ###
#'                 
#' out_d <- SuppressKDisclosure(mun_a, coalition = 1, freqVar = "freq",
#'                              formula = ~mun*inj, sensitive= "inj",
#'                              print_frames = TRUE)
#' show_out(out_d)                                                    
#'                 
#' 
#' out_d1 <- SuppressKDisclosure(mun_a, coalition = 1, freqVar = "freq",
#'                               formula = ~mun*inj, mc_hierarchies = mc_dimlist,
#'                               sensitive = list(mun =  "k3", inj = "injured"),
#'                               print_frames = TRUE)
#' show_out(out_d1)                             
#' 
#' out_d2 <- SuppressKDisclosure(mun_a, coalition = 1, freqVar = "freq",
#'                               formula = ~mun*inj, 
#'                               sensitive = list(inj = "serious", mun = "k3"),
#'                               print_frames = TRUE)
#' show_out(out_d2)                         
#'
#' out_i1 <- SuppressKDisclosure(mun_a, coalition = 1, freqVar = "freq",
#'                               formula = ~mun*inj, identifying = "mun",
#'                               print_frames = TRUE)
#' show_out(out_i1)                            
#'  
#' out_i2 <- SuppressKDisclosure(mun_a, coalition = 1, freqVar = "freq",
#'                               formula = ~mun*inj, identifying = "inj",
#'                               print_frames = TRUE)
#' show_out(out_i2)
#' 
#'
#' mun_b <- SSBtools::SSBtoolsData("mun_accidents")
#' mun_b$freq <- c(0,5,3,4,1,0,
#'                 0,0,2,0,0,6,
#'                 4,1,0,4,0,0,
#'                 0,0,0,0,0,0)
#'
#' # With cells forced to be published, yielding unsafe table
#' out_unsafe <- SuppressKDisclosure(mun_b, coalition = 1, freqVar = "freq",
#'                                  formula = ~mun*inj, sensitive = "inj", 
#'                                  forced = c(12,14,15), output = "all",
#'                                  print_frames = TRUE)
#' show_out(out_unsafe$publish)
#' 
#' # colnames in $unsafe give an indication as to which cells/differences are unsafe
#' colnames(out_unsafe$unsafe)
#'                                
#'                                
#'                                
#'  ### Advanced examples using `targeting_exclude` and `targeting_include`                             
#'                                
#' # Create a wrapper function to avoid repeating common arguments                                
#' fun <- function(..., coalition = 7) {
#'    SuppressKDisclosure(SSBtoolsData("d3"), 
#'        formula = ~(region + county)*main_income + region*months + county*main_income*months, 
#'        freqVar = "freq", coalition = coalition , print_frames = "primary_cells", 
#'        mc_hierarchies = list(main_income = c("special = assistance + other", 
#'                                              "ordinary = pensions + wages")),
#'        ...)}
#'        
#' # Without any sensitive or identifying specifications       
#' a1 <- fun()
#' 
#' # Treat the `main_income` variable as sensitive
#' a2 <- fun(sensitive = "main_income")
#' 
#' # In addition, treat `region` as identifying
#' a3 <- fun(sensitive = "main_income", identifying = "region")
#' 
#' # Only the categories "assistance" and "wages" are considered sensitive
#' # Also use "special" and "ordinary" as identifying categories (instead of "Total")
#' a4 <- fun(sensitive = list(main_income = c("assistance", "wages")), 
#'           identifying = list(region = "*", main_income = c("special", "ordinary")))  
#'           
#' # As above, but additionally exclude regions i and j via the sensitive specification          
#' a5 <- fun(sensitive = list(main_income = c("assistance", "wages")), 
#'           identifying = list(region = "*", main_income = c("special", "ordinary")), 
#'           targeting_exclude = list(list(sensitive = list(region = c("i", "j")))))
#' 
#' # Same exclusion as above, but specified via identifying instead of sensitive
#' # Here `main_income` must also be specified, since the default for identifying is "Total" 
#' a6 <- fun(sensitive = list(main_income = c("assistance", "wages")), 
#'           identifying = list(region = "*", main_income = c("special", "ordinary")), 
#'           targeting_exclude = list(list(identifying = list(region = c("i", "j"), 
#'                                         main_income = "*"))))
#'                                         
#' # The results are identical                                          
#' identical(a5,a6)
#' 
#' 
#' # Add relations so that additional difference cells may be suppressed 
#' a7 <- fun(sensitive = list(main_income = c("assistance", "wages")), 
#'           identifying = list(region = "*", main_income = c("special", "ordinary")), 
#'           targeting_exclude = list(list(identifying = list(region = c("i", "j"), 
#'                                         main_income = "*"))), 
#'           targeting_include = list(
#'             list(identifying = list(region = c("14", "U", "V", "X"), 
#'                                     main_income = c("special", "ordinary"), 
#'                                     months = c("m10m12", "Total")), 
#'                  sensitive = list(region = c("m01m05"), 
#'                                   main_income = c("pensions", "assistance")))))
#'             
#' # As above, but use a data.frame for precise specification of relations
#' # Therefore, "V ordinary–pensions" is no longer included                                     
#' a8 <- fun(sensitive = list(main_income = c("assistance", "wages")), 
#'           identifying = list(region = "*", main_income = c("special", "ordinary")), 
#'           targeting_exclude = list(list(identifying = list(region = c("i", "j"), 
#'                                         main_income = "*"))), 
#'           targeting_include = list(
#'             list(identifying = data.frame(region = c("14", "U", "V", "X"), 
#'                                           main_income = c("special", "ordinary"), 
#'                                           months = c("m10m12", "Total")), 
#'                  sensitive = list(region = c("m01m05"), 
#'                                   main_income = c("pensions", "assistance")))))    
#'    
#' # Specify the same relations as above, but in a different way
#' # Using multiple list elements                                    
#' a9 <- fun(sensitive = list(main_income = c("assistance", "wages")), 
#'           identifying = list(region = "*", main_income = c("special", "ordinary")), 
#'           targeting_exclude = list(list(identifying = list(region = c("i", "j"), 
#'                                         main_income = "*"))), 
#'           targeting_include = list(
#'             list(identifying = list(region = "14", 
#'                                     main_income = "special", 
#'                                     months = "m10m12"), 
#'                  sensitive = list(region = "14", 
#'                                   main_income = "assistance", 
#'                                   months = "m10m12")), 
#'             list(identifying = list(region = c("U", "X"), 
#'                                     main_income = "ordinary", 
#'                                     months = "Total"), 
#'                  sensitive = list(region = c("U", "X"), 
#'                                   main_income = "pensions", 
#'                                   months = "Total"))))                                                                   
#'
#' # The results are identical 
#' identical(a8,a9)
#'
SuppressKDisclosure <- function(data,
                                coalition = 0,
                                mc_hierarchies = NULL,
                                upper_bound = Inf,
                                dimVar = NULL,
                                formula = NULL,
                                hierarchies = NULL,
                                freqVar = NULL,
                                targeting = default_targeting,
                                identifying = NULL, 
                                sensitive = NULL,
                                print_frames = FALSE,
                                ...,
                                spec = PackageSpecs("kDisclosureSpec")) {
  additional_params <- list(...)
  if (length(additional_params)) {
    if ("singletonMethod" %in% names(additional_params) &
        "none" %in% additional_params[["singletonMethod"]])
      warning(
        "SuppressKDisclosure should use a singleton method for protecting the zero singleton problem. The output might not be safe, consider rerunning with a singleton method (default)."
      )
  }
  GaussSuppressionFromData(
    data,
    hierarchies = hierarchies,
    formula = formula,
    dimVar = dimVar,
    freqVar = freqVar,
    coalition = coalition,
    mc_hierarchies = mc_hierarchies,
    upper_bound = upper_bound,
    spec = spec,
    targeting = targeting,
    identifying = identifying,
    sensitive = sensitive,
    print_frames = print_frames,
    ...
  )
}

#' Construct primary suppressed difference matrix
#'
#' Function for constructing model matrix columns representing primary suppressed
#' difference cells
#' 
#' @details
#' The `targeting` specification is a named list that may contain the following
#' optional elements. References to `crossTable` below refer to a data frame
#' that may be extended after applying `mc_hierarchies`.
#'
#' \describe{
#'   \item{`identifying`}{A data frame containing selected rows from
#'   `crossTable`. Membership in the cells represented by these rows is regarded
#'   as information that an intruder may already know. If omitted, it defaults
#'   to `crossTable`.}
#'
#'   \item{`sensitive`}{A data frame containing selected rows from
#'   `crossTable`. If an intruder can infer membership in the cells represented
#'   by these rows, this is considered an unacceptable disclosure, subject to
#'   any further specification provided by `is_sensitive`. If omitted, it
#'   defaults to `crossTable`.}
#'
#'   \item{`is_sensitive`}{A data frame with the same structure as
#'   `sensitive`, but with logical variables. It indicates which codes in
#'   `sensitive` are regarded as sensitive. When specified, disclosure is
#'   assessed by which codes within a revealed cell are disclosed. If omitted,
#'   it is equivalent to a data frame where all elements are `TRUE`.}
#'   
#'   \item{`exclude_relations`}{A specification defining identifying–sensitive
#'   relations that are ignored. This may be given either as a sparse logical
#'   matrix (or a dummy matrix with values 0/1), or as a list of lists.
#'   In the matrix form, rows correspond to rows in `sensitive` (or `crossTable`
#'   if `sensitive` is not specified), and columns correspond to rows in
#'   `identifying` (or `crossTable` if `identifying` is not specified).
#'   In the list form, each list element specifies a set of relations by
#'   selecting rows from `identifying` and/or `sensitive` defined above. Each
#'   element may contain the components `identifying` and `sensitive`; omitted
#'   components default to all rows of the corresponding element. The full list
#'   jointly defines the relations to be excluded.}
#'
#'   \item{`include_relations`}{As for `exclude_relations`, but defining the
#'   identifying–sensitive relations that are considered. Only the relations
#'   specified are included; all others are ignored.}

#'   
#' }
#'
#' @inheritParams SuppressKDisclosure
#' @param crossTable crossTable generated by parent function
#' @param x ModelMatrix generated by parent function
#' 
#' @param targeting NULL, a list, or a function that returns a list specifying
#' attribute disclosure scenarios. See Details.
#' Default is [default_targeting].
#'
#' @return dgCMatrix corresponding to primary suppressed cells
#' @export
#'
#' @author Daniel P. Lupp and Øyvind Langsrud
KDisclosurePrimary <- function(data,
                               x,
                               crossTable,
                               freqVar,
                               mc_hierarchies = NULL,
                               coalition = 1,
                               upper_bound = Inf,
                               targeting = default_targeting,
                               print_frames = FALSE,
                               ...) {
  
  
  
  mc_obj <- X_from_mc(
    data = data,
    x = x,
    crossTable = crossTable,
    mc_hierarchies = mc_hierarchies,
    freqVar = freqVar,
    coalition = coalition,
    upper_bound = upper_bound,
    returnNewCrossTable = TRUE,
    ...
  )
  
  orig_nrow_crossTable <- nrow(crossTable)
  
  x <- cbind(x, mc_obj$x)
  crossTable <- rbind(crossTable, mc_obj$crossTable)
  
  freq <- as.vector(crossprod(x, data[[freqVar]]))
  
  only_print_primary_cells <- FALSE
  if (identical(print_frames, "primary_cells")) {
    print_frames <- TRUE
    only_print_primary_cells <- TRUE
  } 
  
  if (print_frames & !only_print_primary_cells & !is.null(mc_obj)) {
    r <- SSBtools::SeqInc(orig_nrow_crossTable + 1, nrow(crossTable))
    hidden_cells <- cbind(crossTable[r, ,drop = FALSE], freq = freq[r])
    rownames(hidden_cells) <- NULL
    cat("\n----- hidden cells from mc_hierarchies -----\n")
    print(hidden_cells)
  }
  
  
  if(is.function(targeting)) {
    targeting <- targeting(..., freq = freq, x = x, crossTable = crossTable)
  }
  
  identifying <- targeting$identifying
  sensitive  <- targeting$sensitive
  is_sensitive <- targeting$is_sensitive
  exclude_relations <- targeting$exclude_relations
  include_relations <- targeting$include_relations
  
  if (is.list(exclude_relations)) {
    targeting_exclude <- exclude_relations
    exclude_relations <- NULL
  } else {
    targeting_exclude <- NULL
  }
  if (is.list(include_relations)) {
    targeting_include <- include_relations
    include_relations <- NULL
  } else {
    targeting_include <- NULL
  }
    
  use_is_sensitive <- !is.null(is_sensitive)
  
  if (use_is_sensitive | !is.null(identifying) | !is.null(sensitive) | 
      !is.null(exclude_relations) | !is.null(include_relations)) {
    if (is.null(identifying)) {
      identifying <- crossTable
    }
    if (is.null(sensitive)) {
      sensitive <- crossTable
    }
  }
    
  if (use_is_sensitive) {
    validate_is_sensitive(is_sensitive, sensitive)
    
    if (isTRUE(all(is_sensitive))) {
      is_sensitive <- NULL
      use_is_sensitive <- FALSE
    }
  }
  
  
  if (!is.null(identifying)) {   # from above !is.null(sensitive) when !is.null(identifying)
    
    # Match identifying
    ma <- SSBtools::Match(identifying, crossTable)
    identifying <- identifying[!is.na(ma), , drop = FALSE]
    exclude_relations <- exclude_relations[, !is.na(ma) ,drop = FALSE]
    include_relations <- include_relations[, !is.na(ma) ,drop = FALSE]
    y <- x[, ma[!is.na(ma)], drop = FALSE]
    if (!use_is_sensitive) {
      sel <- !SSBtools::DummyDuplicated(y, rnd = TRUE)
      y <- y[, sel, drop = FALSE]
      identifying <- identifying[sel, , drop = FALSE]
      exclude_relations <- exclude_relations[, sel,drop = FALSE]
      include_relations <- include_relations[, sel,drop = FALSE]
    }
    
    # Match sensitive
    ma <- SSBtools::Match(sensitive, crossTable)
    sensitive <- sensitive[!is.na(ma), , drop = FALSE]
    exclude_relations <- exclude_relations[!is.na(ma), ,drop = FALSE]
    include_relations <- include_relations[!is.na(ma), ,drop = FALSE]
    if (use_is_sensitive) {
      is_sensitive <- is_sensitive[!is.na(ma), , drop = FALSE]
    }
    x <- x[, ma[!is.na(ma)], drop = FALSE]
    if (!use_is_sensitive) {
      sel <- !SSBtools::DummyDuplicated(x, rnd = TRUE)
      x <- x[, sel, drop = FALSE]
      sensitive <- sensitive[sel, , drop = FALSE]
      exclude_relations <- exclude_relations[sel, ,drop = FALSE]
      include_relations <- include_relations[sel, ,drop = FALSE]
    }
    
  } else {
    sel <- !SSBtools::DummyDuplicated(x, rnd = TRUE)
    x <- x[, sel, drop = FALSE]
    y <- x
    crossTable <- crossTable[sel, , drop = FALSE]
    exclude_relations <- exclude_relations[sel, sel, drop = FALSE]
    include_relations <- include_relations[sel, sel, drop = FALSE]
  }
  
  if (use_is_sensitive) {  # Extra check after modifications  
    validate_is_sensitive(is_sensitive, sensitive)
  }
  
  FindDifferenceCells(
    x = x,
    y = y,
    freq_x = as.vector(crossprod(x, data[[freqVar]])),
    freq_y = as.vector(crossprod(y, data[[freqVar]])),
    coalition = coalition,
    upper_bound = upper_bound,
    crossTable = crossTable,
    identifying = identifying,
    sensitive = sensitive,
    is_sensitive = is_sensitive,
    exclude_relations  = exclude_relations,
    include_relations = include_relations,
    targeting_exclude = targeting_exclude, 
    targeting_include = targeting_include,
    orig_nrow_crossTable = orig_nrow_crossTable,
    print_frames = print_frames
  )
}



FindDifferenceCells <- function(x,
                                y = x,
                                freq_x,
                                freq_y = freq_x,
                                coalition,
                                upper_bound = Inf,
                                crossTable,
                                identifying,
                                sensitive,
                                is_sensitive,
                                exclude_relations,
                                include_relations,
                                targeting_exclude,
                                targeting_include,
                                orig_nrow_crossTable,
                                print_frames = FALSE
                                ) {
  
  use_is_sensitive <- !is.null(is_sensitive)
  
  if (is.null(identifying)) {
    identifying <- crossTable
  }
  if (is.null(sensitive)) {
    sensitive <- crossTable
  }
  
  sel_x <- freq_x <= upper_bound & freq_x > 0  
  sel_y <- freq_y <= (upper_bound + coalition)  & freq_y > 0 
  if (!any(sel_x) | !any(sel_y)) {
    return(rep(FALSE, orig_nrow_crossTable))
  }
  freq_x <- freq_x[sel_x]
  freq_y <- freq_y[sel_y]
  if (use_is_sensitive) {
    is_sensitive <- is_sensitive[sel_x, , drop = FALSE]
  }
  sensitive <- sensitive[sel_x, , drop = FALSE]
  identifying <- identifying[sel_y, , drop = FALSE]
  x <- x[, sel_x, drop = FALSE]
  y <- y[, sel_y, drop = FALSE]
  exclude_relations <- exclude_relations[sel_x, sel_y, drop = FALSE]
  include_relations <- include_relations[sel_x, sel_y, drop = FALSE]
  
  xty <- crossprod(x, y)
  
  if (!is.null(exclude_relations)) {
    xty <- xty - xty * exclude_relations # This way to preserve matrix sparsity 
  }
  if (!is.null(include_relations)) {
    xty <- xty * include_relations
  }
  
  xty <- As_TsparseMatrix(xty, do_drop0 = TRUE) # do_drop0 = TRUE is default in As_TsparseMatrix  
  colSums_y_xty_j_1 <- colSums(y)[xty@j + 1]
  # finds children in x and parents in y
  r <- colSums(x)[xty@i + 1] == xty@x & 
    colSums_y_xty_j_1     != xty@x 
  
  if (!any(r)) {
    return(rep(FALSE, orig_nrow_crossTable))
  }
  
  child <- xty@i[r] + 1L
  parent <- xty@j[r] + 1L
  
  freq_diff <- freq_y[parent] - freq_x[child]
  
  disclosures <- freq_diff <= coalition
  
  if (!any(disclosures)) {
    return(rep(FALSE, orig_nrow_crossTable))
  }
  
  freq_diff <- freq_diff[disclosures]
  parent <- parent[disclosures]
  child <- child[disclosures]
  
  identifying <- identifying[parent, , drop = FALSE]
  sensitive <- sensitive[child, , drop = FALSE]
  
  if (use_is_sensitive) {
    is_sensitive <- as.matrix(is_sensitive)
    is_sensitive <- is_sensitive[child, , drop = FALSE]
    
    same_codes <- identifying == sensitive
    same_codes[!is_sensitive] <- TRUE
    same_row <- rowSums(!same_codes) == 0
    
    parent <- parent[!same_row]
    child <- child[!same_row]
    
    identifying <- identifying[!same_row, , drop = FALSE]
    sensitive <- sensitive[!same_row, , drop = FALSE]
    freq_diff <- freq_diff[!same_row]
  }
  
  if (!is.null(targeting_exclude) | !is.null(targeting_include)) {
    
    include <- rep(TRUE, length(parent))
    
    if (!is.null(targeting_include)) {
      include <- rep(FALSE, length(parent))
      for (i in seq_along(targeting_include)) {
        sel_i <- identifying_sensitive_selection(
          sel_identifying = targeting_include[[i]]$identifying, 
          sel_sensitive = targeting_include[[i]]$sensitive, 
          identifying = identifying, 
          sensitive = sensitive)
        include <- include | sel_i
      }
    } else {
      include <- rep(TRUE, length(parent))
    }
    
    if (!is.null(targeting_exclude)) {
      for (i in seq_along(targeting_exclude)) {
        sel_i <- identifying_sensitive_selection(
          sel_identifying = targeting_exclude[[i]]$identifying, 
          sel_sensitive = targeting_exclude[[i]]$sensitive, 
          identifying = identifying, 
          sensitive = sensitive)
        include <- include & !sel_i
      }
    }
    
    parent <- parent[include]
    child <- child[include]
    identifying <- identifying[include, , drop = FALSE]
    sensitive <- sensitive[include, , drop = FALSE]
    freq_diff <- freq_diff[include]
  }
  
  

  
  diff_matrix <- drop0(y[, parent, drop = FALSE] - 
                       x[, child, drop = FALSE])
  
  diff_cells <- difference_cells(identifying, sensitive)
  colnames(diff_matrix) <- apply(diff_cells , 1 , paste , collapse = ":" )
  
  if (print_frames) {
    cat("\n---- primary suppressed difference cells ---\n")
    diff_cells$diff <- freq_diff
    print(diff_cells)
    cat("\n")
  }
  
  diff_matrix[, !SSBtools::DummyDuplicated(diff_matrix, rnd = TRUE), drop = FALSE]
  
}


identifying_sensitive_selection <- function(sel_identifying, sel_sensitive, 
                                            identifying, sensitive) {
  if (!is.null(sel_identifying)) {
    ma_identifying <- !is.na(SSBtools::Match(identifying, sel_identifying))
  }
  if (!is.null(sel_sensitive)) {
    ma_sensitive <- !is.na(SSBtools::Match(sensitive, sel_sensitive))
  }
  if (is.null(sel_identifying)) {
    return(ma_sensitive)
  }
  if (is.null(sel_sensitive)) {
    return(ma_identifying)
  }
  ma_identifying & ma_sensitive
}




# Written by ChatGPT
validate_is_sensitive <- function(is_sensitive, sensitive) {
  if (!is.data.frame(is_sensitive)) {
    stop("`is_sensitive` must be a data frame.", call. = FALSE)
  }
  
  if (!identical(dim(is_sensitive), dim(sensitive))) {
    stop(
      "`is_sensitive` must have the same dimensions as `sensitive`.",
      call. = FALSE
    )
  }
  
  if (!identical(names(is_sensitive), names(sensitive))) {
    stop(
      "`is_sensitive` must have the same variable names as `sensitive`.",
      call. = FALSE
    )
  }
  
  if (!all(vapply(is_sensitive, is.logical, logical(1)))) {
    stop(
      "All variables in `is_sensitive` must be logical.",
      call. = FALSE
    )
  }
  
  # no return value needed
}



difference_cells <- function(identifying, sensitive) {
  r <- identifying != sensitive
  identifying[r] <- paste(identifying[r], sensitive[r], sep = "-")
  rownames(identifying) <- NULL
  identifying
}




#' Default `targeting` function for SuppressKDisclosure()
#'
#' Generates a `targeting` specification for use with
#' [SuppressKDisclosure()]. The function is actually used internally by
#' [KDisclosurePrimary()].
#' 
#' @details
#' The parameters `identifying` and `sensitive` are used to select table cells
#' (including hidden cells constructed via `mc_hierarchies`). All such cells are
#' represented by rows in `crossTable`, which may be extended due to
#' `mc_hierarchies`. Thus, rows in `crossTable` are selected as identifying or
#' sensitive.
#'
#' In addition, `sensitive` specifies which codes within the selected rows are
#' regarded as sensitive.
#'
#' The logic differs slightly for unspecified variables:
#' For `identifying`, unspecified variables are set to total codes.
#' For `sensitive`, all rows in `crossTable` matching the specified variables
#' are selected.
#'
#' The parameters `identifying` and `sensitive` are used to construct the
#' `targeting` specification for `KDisclosurePrimary()`, resulting in the
#' elements `identifying`, `sensitive`, and `is_sensitive`.
#'
#' When `targeting_include` and/or `targeting_exclude` are specified,
#' additional elements `include_relations` and `exclude_relations` are created.
#'
#'
#' @param crossTable A `crossTable`, possibly extended after applying
#' `mc_hierarchies`.
#' @param x The model matrix, `x`, possibly extended after applying 
#' `mc_hierarchies`.
#' 
#' @param identifying Specification of information that an intruder may already
#' know. The specification is subject to the same requirements as `sensitive`
#' below. If not all variables are included, total codes for the missing
#' variables are derived automatically. This requires that the overall total
#' is included as an output row.
#'
#' @param sensitive Specification of information considered unacceptable to
#' disclose. It may be given as a character vector of variable names, a named
#' list with variable names as names and specified codes as values, or a data
#' frame specifying variable combinations. The wildcard characters `*` and `?`,
#' as well as the exclusion operator `!`, may be used, since
#' [SSBtools::WildcardGlobbing()] is applied.
#' 
#' @param targeting_include A list of two-element lists with components
#' `identifying` and `sensitive`. Each element defines identifying–sensitive
#' relations using the same specification rules as the parameters
#' `identifying` and `sensitive`. All specifications together, including the
#' main `identifying` and `sensitive` parameters, define the relations that are
#' examined for suppression.
#'
#' @param targeting_exclude A list specified in the same way as
#' `targeting_include`. The relations defined here are ignored when examining
#' suppression.
#' 
#' @param ... Unused parameters.
#'
#' @returns
#' A named `targeting` list. See [SuppressKDisclosure()].
#'
#' @export
#' 
#' @examples
#' 
#' mm <- SSBtools::ModelMatrix(SSBtoolsData("example1"), 
#'      formula = ~age * eu + geo, crossTable = TRUE)
#' crossTable <- mm$crossTable
#' x <- mm$modelMatrix      
#' 
#' default_targeting(crossTable, x)  # just NULL 
#' 
#' # geo identifying and age sensitive (age sensitive variable)
#' a2 <- default_targeting(crossTable, x, 
#'                         identifying = "geo", 
#'                         sensitive = "age")
#' a1 <- default_targeting(crossTable, x, 
#'                         identifying = list(age = "Total", geo = "*"), 
#'                         sensitive = list(age = "*")) 
#' identical(a1, a2)
#' a1                         
#'                   
#'                   
#' # Not ok to disclose 'EU' and 'Portugal'
#' # But ok to disclose 'Spain' with 'EU' known
#' # and also ok to disclose 'Spain' in other table cells without 'EU' as marginal  
#' default_targeting(crossTable, x, 
#'                   sensitive = list(geo = c("Portugal", "EU")))
#'                   
#' # As above but now also ok to disclose 'Portugal' from 'EU' known,
#' # since protection only considers 'age' identifying.                   
#' default_targeting(crossTable, x, 
#'                   identifying = "age",
#'                   sensitive = list(geo = c("Portugal", "EU")))                 
#' 
default_targeting <- function(crossTable, x, 
                              identifying = NULL, sensitive = NULL, 
                              targeting_include = NULL,
                              targeting_exclude = NULL,
                              ...) {
  
  if (!is.null(targeting_include) | !is.null(targeting_exclude)) {
    if (!is.null(identifying) | !is.null(sensitive)) {
      targeting_include <- c(list(list(identifying = identifying, sensitive = sensitive)), targeting_include)
    }
    d <- include_via_list(crossTable = crossTable, x = x, 
                          via_list = targeting_include, ...)
    identifying <- d$identifying
    sensitive <- d$sensitive
    
    if (!is.null(targeting_exclude)) {
      d$exclude_relations <- exclude_via_list(crossTable = crossTable, x = x, 
                                              identifying = identifying, 
                                              sensitive = sensitive, 
                                              via_list = targeting_exclude, ...)
    } 
    
    return(d)
  }
  
  if (is.null(identifying) & is.null(sensitive)) {
    return(NULL)
  }
  
  output <- NULL
  
  check_targeting_lists(crossTable, identifying, sensitive)
  
  tot_code <- NULL
  
  if (!is.null(identifying)) {
    if (is.character(identifying)) {
      identifying <- setNames(rep(list("*"),length(identifying)), identifying)
    }
    missing_identifying_names <- setdiff(names(crossTable), names(identifying))
    if(length(missing_identifying_names)) {
      tot_code <- FindTotCode2(x, crossTable)
      missing_identifying <- tot_code[missing_identifying_names]
      missing_tot_code <- sapply(missing_identifying, length) == 0
      if(any(missing_tot_code)) {
        stop(paste0("Total code not found automatically: ",
                   paste(names(missing_identifying)[missing_tot_code], collapse = ", "),
                   ". Specify in identifying list."))
      }
      if (is.data.frame(identifying)) {
        if (!is.list(missing_identifying) ||
            any(lengths(missing_identifying) != 1)) {
          stop("For data.frame input, additional variable(s) must be specified since the total code is not unique.")
        }
        identifying <- cbind(
          identifying,
          as.data.frame(missing_identifying, stringsAsFactors = FALSE)
        )
      } else {
        identifying <- c(identifying, missing_identifying)
      }
    }
    
    
    if (is.data.frame(identifying)) {
      identifying_rows <- SSBtools::WildcardGlobbing(crossTable, identifying)
    } else {
      identifying_rows <- rep(TRUE, nrow(crossTable))
      for (i in seq_along(identifying)) {
        name_i <- names(identifying)[i]
        identifying_rows <- identifying_rows & SSBtools::WildcardGlobbing(crossTable[name_i], as.data.frame(identifying[i]))
      }
    }
    output$identifying <- crossTable[identifying_rows, , drop = FALSE]
    rownames(output$identifying) <- NULL
  } 
  
  if (!is.null(sensitive)) {
    if (is.character(sensitive)) {
      sensitive <- setNames(rep(list("*"),length(sensitive)), sensitive)
    }
    
    is_sensitive <- as.data.frame(matrix(FALSE, nrow(crossTable), ncol(crossTable)))
    names(is_sensitive) <- names(crossTable)
    
    if (is.data.frame(sensitive)) {
      sensitive_rows <- SSBtools::WildcardGlobbing(crossTable, sensitive)
    }
    for (i in seq_along(sensitive)) {
      name_i <- names(sensitive)[i]
      if (is.data.frame(sensitive)) {
        is_sensitive[[name_i]] <- sensitive_rows
      } else {
        is_sensitive[[name_i]] <- SSBtools::WildcardGlobbing(crossTable[name_i], as.data.frame(sensitive[i])) 
      }
    }
    
    if (is.null(tot_code)) {
      tot_code <- FindTotCode2(x, crossTable)
    }
    
    any_sensitive <- rowSums(is_sensitive) != 0
    output$sensitive <- crossTable[any_sensitive, , drop = FALSE]
    output$is_sensitive <- is_sensitive[any_sensitive, , drop = FALSE]
    
    
    # Remove tot-rows if possible (not important)
    if (is.null(tot_code)) {
      tot_code <- FindTotCode2(x, crossTable)
    }
    if(!any(sapply(tot_code[names(sensitive)], length) == 0)){
      dis_tot <- matrix(FALSE, nrow(output$sensitive), length(sensitive))
      for (i in seq_along(sensitive)) {
        name_i <- names(sensitive)[i]
        dis_tot[,i] <- output$sensitive[[name_i]] %in% tot_code[i]
      }
      ok_rows <- rowSums(!dis_tot) != 0
      output$sensitive <- output$sensitive[ok_rows, , drop = FALSE]
      output$is_sensitive <- output$is_sensitive[ok_rows, , drop = FALSE]
    }
    rownames(output$sensitive) <- NULL
    rownames(output$is_sensitive) <- NULL
  }
  
  output
}


include_via_list <- function(crossTable, x, via_list, ...) {
  
  identifying <- NULL
  sensitive <- NULL
  is_sensitive <- NULL
  
  for (i in seq_along(via_list)) {
    d <- default_targeting(crossTable = crossTable, 
                           x = x, 
                           identifying = via_list[[i]]$identifying, 
                           sensitive = via_list[[i]]$sensitive, ...)
    
    if (!is.null(d$identifying)) {
      ma <- SSBtools::Match(d$identifying, identifying)
      identifying <- rbind(identifying, d$identifying[is.na(ma), , drop = FALSE])
      via_list[[i]]$identifying <- d$identifying
    }
    
    if (!is.null(d$sensitive)) {
      ma <- SSBtools::Match(d$sensitive, sensitive)
      if (!is.null(d$is_sensitive)) {
        if (is.null(is_sensitive) & !is.null(sensitive)) {
          is_sensitive <- as.data.frame(matrix(TRUE, nrow(sensitive), ncol(sensitive)))
          names(is_sensitive) <- names(sensitive)
        } else {
          if (any(!is.na(ma))) {
            is_sensitive[ma[!is.na(ma)], ] <- 
              is_sensitive[ma[!is.na(ma)], , drop = FALSE] | 
              d$is_sensitive[!is.na(ma), , drop = FALSE]
          }
        }
        is_sensitive <- rbind(is_sensitive, d$is_sensitive[is.na(ma), , drop = FALSE])
      }
      sensitive <- rbind(sensitive, d$sensitive[is.na(ma), , drop = FALSE])
    }
    via_list[[i]]$sensitive <- d$sensitive
  }
  
  list(identifying = identifying, 
       sensitive = sensitive, 
       is_sensitive = is_sensitive, 
       include_relations = via_list)
  
}

exclude_via_list <- function(crossTable, x, identifying, sensitive, via_list, ...) {
  for (i in seq_along(via_list)) {
    d <- default_targeting(crossTable = crossTable, 
                           x = x, 
                           identifying = via_list[[i]]$identifying, 
                           sensitive = via_list[[i]]$sensitive, ...)
    if (!is.null(d$identifying)) {
      ma <- SSBtools::Match(d$identifying, identifying)
      via_list[[i]]$identifying <- d$identifying[!is.na(ma), , drop = FALSE]
    }
    if (!is.null(d$sensitive)) {
      ma <- SSBtools::Match(d$sensitive, sensitive)
      via_list[[i]]$sensitive <- d$sensitive[!is.na(ma), , drop = FALSE]
    }
  }
  via_list
}


# Written by ChatGPT
check_targeting_lists <- function(crossTable, identifying = NULL, sensitive = NULL) {
  
  ct_names <- names(crossTable)
  
  check_spec <- function(spec, argname) {
    if (is.null(spec)) {
      return(invisible(TRUE))
    }
    
    # Extract "names" to validate: either list names or character values
    if (is.character(spec)) {
      spec_names <- spec
      if (length(spec_names) == 0) {
        stop("`", argname, "` must contain at least one element.", call. = FALSE)
      }
      if (any(spec_names == "")) {
        stop("`", argname, "` must not contain empty strings.", call. = FALSE)
      }
    } else if (is.list(spec)) {
      spec_names <- names(spec)
      if (is.null(spec_names) || length(spec_names) == 0) {
        stop("`", argname, "` must be a named list.", call. = FALSE)
      }
      if (any(spec_names == "")) {
        stop("`", argname, "` must be a named list with no empty names.", call. = FALSE)
      }
    } else {
      stop("`", argname, "` must be NULL, a named list, or a character vector.", call. = FALSE)
    }
    
    # Validate against crossTable names
    if (!all(spec_names %in% ct_names)) {
      stop(
        "`", argname, "` contains names not found in `crossTable`: ",
        paste(setdiff(spec_names, ct_names), collapse = ", "),
        call. = FALSE
      )
    }
    
    invisible(TRUE)
  }
  
  check_spec(identifying, "identifying")
  check_spec(sensitive,  "sensitive")
  
  invisible(TRUE)
}



