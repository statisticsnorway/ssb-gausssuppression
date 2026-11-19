suppress_double <- function(data,
                            formula,
                            suppress_fun = SuppressDominantCells,
                            double_vars = NULL,
                            double_priority = 0,
                            use_diff_groups = FALSE, 
                            remove_diff_vars = TRUE,
                            ...,
                            candidates = GetDefault(suppress_fun, "candidates"),
                            output = "publish") {
  
  if(hasArg("hierarchies")) {
    warning("The 'hierarchies' parameter is ignored in 'suppress_double()'.")
  }
  
  
  ncol_input_data <- ncol(data)
  rename_extra <- NULL
  ignore_vars <- list()
  added_names <- character(0)
  double_vars_input <- double_vars
  
  if (!is.null(double_vars)) {
    if (double_priority %in% c(-1, -2)) {
      for (i in seq_along(double_vars)) {
        double_vars[[i]] <- double_vars[[i]][-double_priority]
      }
    } else {
      ignore_vars <- list(ignore_vars = character(0), ignore_diff = character(0))
      for (i in seq_along(double_vars)) {
        if (double_priority >= 1) {
          ignore_vars[["ignore_vars"]] <- 
            c(ignore_vars[["ignore_vars"]], rev(double_vars[[i]])[double_priority])
        }
      }
      if (!isFALSE(use_diff_groups)) {
        double_vars_input <- double_vars 
        for (i in seq_along(double_vars)) {
          names_data <- names(data)
          data <- my_data_diff_groups(data, 
                                      input_vars = double_vars[[i]], 
                                      diff_extra = identical(use_diff_groups, "extra"), 
                                      diff_name = names(double_vars)[i])
          if (double_priority >= 0) {
            ignore_vars[["ignore_diff"]] <- c(ignore_vars[["ignore_diff"]], comment(data))
          }
          double_vars[[i]] <- c(double_vars[[i]], comment(data))
        }
        comment(data) <- NULL
      }
      idx <- duplicated_grouping(data[SSBtools::SeqInc(ncol_input_data+1, ncol(data))],
                                 idx = TRUE)
      
      added_names <- names(data)[(SSBtools::SeqInc(ncol_input_data+1, ncol(data)))]
      name_map <- setNames(added_names[idx], added_names)
      duplicated_vars <- names(data)[(SSBtools::SeqInc(ncol_input_data+1, ncol(data)))[duplicated(idx)]]
      data <- data[!(names(data) %in% duplicated_vars)]
      added_names <- unique(map_names(added_names, name_map))
      ignore_vars[["ignore_diff"]] <- unique(map_names(ignore_vars[["ignore_diff"]], name_map))
      for (i in seq_along(double_vars)) {
        double_vars[[i]] <- unique(map_names(double_vars[[i]], name_map))
      }
    }
    
    formula_no_diff <- substitute_double_vars(formula, double_vars_input,
                                              double_priority)
    
    formula <- substitute_double_vars(formula, double_vars,
                                      double_priority)
    
  }
   
  
  if (output == "double") {
    output_ <- "publish"
    if (double_priority < 0) {
      output <- "publish"
    }
  } else {
    output_ <- output
  }
  
  
  out <- suppress_fun(data = data,
                      formula = unlist(formula),
                      hierarchies = NULL,
                      ...,
                      candidates = candidates_ignore_vars,
                      ignore_vars = ignore_vars,
                      candidates_input = candidates,
                      output = output_)
  
  
  if (!output %in% c("publish", "double")) {
    return(out)
  }
  
  if (remove_diff_vars & (!is.null(double_vars)) & (!isFALSE(use_diff_groups))) {
    out <- FormulaSelection(out, combine_formulas(unlist(formula_no_diff)))
  }
  
  startRow <- attr(out, "startRow", exact = TRUE)
  totCode <- attr(out, "totCode", exact = TRUE)  
  
  if (remove_diff_vars & (!is.null(double_vars)) & (!isFALSE(use_diff_groups))) {
    out <- out[!(names(out) %in% added_names)]
    totCode <- totCode[!(names(totCode) %in% added_names)]
  }
  
  attr(out, "totCode") <- totCode
  attr(out, "startRow") <-  startRow 
  
  
  if (output == "double") {
    return(list(a = FormulaSelection(out, combine_formulas(unlist(formula_no_diff[1]))),
                b = FormulaSelection(out, combine_formulas(unlist(formula_no_diff[2])))))
  }
  
  out
}








my_data_diff_groups <- function(data, diff_name, ...) {
  ncol_data <- ncol(data)
  d <- SSBtools::data_diff_groups(data, ..., 
                                  output_vars = c(diff_1_2 = paste0(diff_name, "_diff_1_2"), 
                                                  diff_2_1 = paste0(diff_name, "_diff_2_1")))
  d_new <- d[-seq_len(ncol_data)]
  new_names <- names(d_new)[colSums(!is.na(d_new)) > 0]
  d_new <- d[new_names]
  new_names <- new_names[!duplicated_grouping(d_new)]
  comment(d) <- new_names
  d
}



candidates_ignore_vars <- function(ignore_vars, candidates_input,..., x, crossTable) {
  candidates <- candidates_input(..., x = x, crossTable = crossTable)
  empty <- colSums(x) == 0
  for (j in seq_along(ignore_vars)) {
    to_ignore <- rep(FALSE, length(candidates))
    for (i in seq_along(ignore_vars[[j]])) {
      to_ignore[(crossTable[[ignore_vars[[j]][i]]])[candidates] != "Total"] <- TRUE
      # empty not ignored to avoid warning 
      #         "Cells with empty input will never be secondary suppressed...."
      to_ignore[empty[candidates]] <- FALSE
    }
    candidates <- c(candidates[!to_ignore], candidates[to_ignore])
  }
  candidates
}



duplicated_grouping <- function(df, idx = FALSE) {
  z <- lapply(df, function(x) {
    ma <- match(x, unique(x))
    ma[is.na(x)] <- 0L
    ma
  })
  if (idx) {
    return(match(z, z))
  }
  duplicated(z)
}



map_names <- function(vars, name_map) {
  i <- match(vars, names(name_map))
  vars[!is.na(i)] <- name_map[i[!is.na(i)]]
  vars
}


substitute_double_vars <- function(formula, double_vars, double_priority) {
  formula_is_list <- is.list(formula)
  if (!formula_is_list) {
    formula <- list(formula)
  }
  
  if (formula_is_list && is.null(names(formula))) {
    names(formula) <- as.character(seq_along(formula))
  }
  
  if (double_priority >= 0) {
    double_vars_a <- lapply(double_vars, \(x) x[-2])
    double_vars_b <- lapply(double_vars, \(x) x[-1])
    
    formula_a <- lapply(formula, SSBtools::substitute_formula_vars,
                        replacements = double_vars_a)
    formula_b <- lapply(formula, SSBtools::substitute_formula_vars,
                        replacements = double_vars_b)
    
    if (formula_is_list) {
      names(formula_a) <- paste0("a_", names(formula))
      names(formula_b) <- paste0("b_", names(formula))
    } else {
      names(formula_a) <- "a"
      names(formula_b) <- "b"
    }
    
    return(list(formula_a, formula_b))
  }
  
  formula <- lapply(formula, SSBtools::substitute_formula_vars,
                    replacements = double_vars)
  
  formula
}





