#' Cell Suppression with Double Coding and Difference Cells
#'
#' A wrapper for cell suppression when data contain two alternative
#' classifications of the same variable. The function supports simultaneous
#' protection of tables based on both classifications, with optional
#' prioritization and special handling of difference cells.
#'
#' The function was developed with Eurostat's requirements for double
#' reporting during the revision of the Statistical Classification of
#' Economic Activities (NACE) in mind.
#'
#' @param data The input data to be processed by `suppress_fun`.
#' @param formula A formula or a named list of formulas, as in
#'   [GaussSuppressionFromData()], but using the variable names defined
#'   in `double_vars` instead of the original classification variables.
#' @param suppress_fun The suppression function to be used.
#'   Defaults to [SuppressDominantCells()].
#' @param double_vars A named list specifying pairs of variables representing
#'   alternative classifications. For example,
#'   `list(nace_2 = c("nace_2_A", "nace_2_B"),
#'         nace_3 = c("nace_3_A", "nace_3_B"))`.
#'   The list names (`nace_2` and `nace_3`) are used in `formula`,
#'   while the corresponding variables must exist in `data`.
#' @param double_priority Controls prioritization between the two
#'   classifications and allows special processing modes.
#'   Possible values are:
#'   * `0`: Only difference cells are given lower priority.
#'   * `1`: The first classification is prioritized.
#'   * `2`: The second classification is prioritized.
#'   * `-1`: Process only the first classification.
#'   * `-2`: Process only the second classification.
#'   * `-9`: Process without prioritization, primarily for testing.
#' @param use_diff_groups Controls whether difference cells are
#'   generated and included in the suppression process.
#'   Possible values are:
#'   * `FALSE`: Do not generate difference cells.
#'   * `TRUE`: Generate basic difference cells.
#'   * `"extra"`: Also generate additional difference cells when found.
#'
#'   Difference cells are generated using [SSBtools::data_diff_groups()].
#'   The value `"extra"` corresponds to `diff_extra = TRUE` in that function.
#' @param remove_diff_vars Logical. If `TRUE` (the default), generated
#'   difference-group variables are removed from the ordinary data frame
#'   output. If `FALSE`, these variables are retained.
#'   Some difference-group variable names may appear unintuitive because
#'   different names for variables representing identical groupings are
#'   avoided.
#' @param ... Additional arguments passed to `suppress_fun`.
#' @param candidates As the `candidates` parameter in
#'   [GaussSuppressionFromData()]. Included as a formal argument because
#'   candidate ordering is modified according to `double_priority`.
#' @param output As the `output` parameter in [GaussSuppressionFromData()].
#'   In addition, `"double"` can be specified to return a list containing
#'   separate outputs for the two classifications, along with difference cells 
#'   when `remove_diff_vars = FALSE`
#'
#' @returns The suppression result as specified by `output`.
#'
#' @export
#' 
#' @examples
#' 
#' # First, generate example data to be used below 
#' a <- SSBtoolsData("magnitude1")
#' k <- c("50.1", "50.2", "70.1", "70.2")
#' b <- data.frame(nace_3_A = c(k, k), nace_3_B = c(k, k))
#' b[5:8, 2] <- b[c(6, 5, 8, 5), 2]
#' b$nace_2_A <- substr(b[[1]], 1, 2)
#' b$nace_2_B <- substr(b[[2]], 1, 2)
#' set.seed(559)
#' d <- cbind(a, b[sample.int(8, 40, replace = TRUE, prob = 8:1), ])
#' d$id <- paste0("id_", 1:40)
#' rownames(d) <- NULL
#' 
#' 
#' # In this first example, no primary suppressed cells are visible in the output.
#' # Only difference cells are primary suppressed, but these are not shown
#' # in the default output (remove_diff_vars = TRUE).
#' suppress_double(d, numVar = "value", formula = ~nace_2 * eu + nace_3,
#'                 contributorVar = "id", pPercent = 5,
#'                 double_vars = list(nace_2 = c("nace_2_A", "nace_2_B"),
#'                                    nace_3 = c("nace_3_A", "nace_3_B")),
#'                 double_priority = 1,
#'                 use_diff_groups = TRUE)
#' 
#'
#' # Define eight combinations of parameters
#' arg <- data.frame(
#'   use_diff_groups = I(rep(list(TRUE), 8)),
#'   double_priority = 1,
#'   remove_diff_vars = TRUE,
#'   output = "publish"
#' )
#'
#' arg$output[4:5] <- "double"
#' arg$use_diff_groups[c(1, 6)] <- list(FALSE, FALSE)
#' arg$use_diff_groups[[3]] <- "extra"
#' arg$double_priority[5] <- 2
#' arg$remove_diff_vars[4:8] <- FALSE
#' arg$use_diff_groups[[8]] <- "extra"
#'
#' # Run suppression with the different parameter combinations
#' output <- vector("list", nrow(arg))
#'
#' for (i in seq_along(output)) {
#'   output[[i]] <- suppress_double(
#'     data = d,
#'     numVar = "value",
#'     formula = ~nace_2 * geo + nace_3 * eu,
#'     contributorVar = "company",
#'     k = c(80, 99),
#'     double_vars = list(
#'       nace_2 = c("nace_2_A", "nace_2_B"),
#'       nace_3 = c("nace_3_A", "nace_3_B")
#'     ),
#'     double_priority = arg$double_priority[i],
#'     remove_diff_vars = arg$remove_diff_vars[i],
#'     use_diff_groups = arg$use_diff_groups[[i]],
#'     output = arg$output[i]
#'   )
#' }
#'
#' arg
#'
#' # Number of suppressed cells with use_diff_groups FALSE, TRUE and "extra"
#' sum(output[[1]]$suppressed)
#' sum(output[[2]]$suppressed)
#' sum(output[[3]]$suppressed)
#'
#' # Effect of double_priority on the two classifications
#' sum(output[[4]]$a$suppressed)
#' sum(output[[4]]$b$suppressed)
#' sum(output[[5]]$a$suppressed)
#' sum(output[[5]]$b$suppressed)
#'
#' # Difference cells returned when output = "double"
#' output[[5]]$diff
#'
#' # Output variables with remove_diff_vars = FALSE and
#' # use_diff_groups FALSE, TRUE and "extra"
#' # With avoidHierarchical = TRUE, variables are not combined
#' # as they are in output[[8]]
#' head(output[[6]])
#' head(output[[7]])
#' head(output[[8]])
#' 
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
    
    formula_diff <- formula_setdiff(combine_formulas(unlist(formula)), 
                                    combine_formulas(unlist(formula_no_diff))) 
    
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
  
  
  if (!output %in% c("publish", "double") || is.null(double_vars)) {
    return(out)
  }
  
  if (!remove_diff_vars & output == "double") {
    diff_out <- FormulaSelection(out, formula_diff)
    remove_diff_vars <- TRUE
  } else {
    diff_out <- NULL
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
    out <- list(a = FormulaSelection(out, combine_formulas(unlist(formula_no_diff[1]))),
                b = FormulaSelection(out, combine_formulas(unlist(formula_no_diff[2]))))
    if (!is.null(diff_out)) {
      out <- c(out, list(diff = diff_out)) 
    }
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



formula_setdiff <- function(formula_x, formula_y) {
  diff_terms <- setdiff(
    SSBtools::formula_term_labels(formula_x),
    SSBtools::formula_term_labels(formula_y)
  )
  intercept <- "(Intercept)" %in% diff_terms
  reformulate(setdiff(diff_terms, "(Intercept)"), intercept = intercept)
}
  






