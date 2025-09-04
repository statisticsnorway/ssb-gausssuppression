

#' Cell suppression from input data containing inner cells
#' 
#' 
#' Aggregates are generated followed by 
#' primary suppression followed by 
#' secondary suppression by Gaussian elimination by \code{\link[SSBtools]{GaussSuppression}} 
#' 
#' The supplied functions for generating \code{\link[SSBtools]{GaussSuppression}} input takes the following arguments: 
#' `crossTable`,  `x`, `freq`, `num`, `weight`, `maxN`, `protectZeros`, `secondaryZeros`, `data`, `freqVar`, `numVar`, `weightVar`, `charVar`, `dimVar` 
#' `aggregatePackage`, `aggregateNA`, `aggregateBaseOrder`, `rowGroupsPackage`, `structuralEmpty`, and `...`. 
#' where the two first are  \code{\link[SSBtools]{ModelMatrix}} outputs (`modelMatrix` renamed to `x`).
#' The vector, `freq`, is aggregated counts (`t(x) %*% data[[freqVar]]`).
#' In addition, the supplied `singleton` function also takes `nUniqueVar` and (output from) `primary` as input.
#' 
#' Similarly, `num`, is a data frame of aggregated numerical variables.   
#' It is possible to supply several primary functions joined by `c`, e.g. (`c(FunPrim1, FunPrim2)`). 
#' All `NA`s returned from any of the functions force the corresponding cells not to be primary suppressed.
#' 
#' The effect of `maxN` , `protectZeros` and `secondaryZeros` depends on the supplied functions where these parameters are used. 
#' Their default values are inherited from the default values of the first `primary` function (several possible) or, 
#' in the case of `secondaryZeros`, the `candidates` function.   
#' When defaults cannot be inherited, they are set to `NULL`.   
#' In practice the function `formals` are still used to generate the defaults when `primary` and/or `candidates` are not functions. 
#' Then `NULL` is correctly returned, but `suppressWarnings` are needed.
#' 
#' Singleton handling can be turned off by `singleton = NULL` or `singletonMethod = "none"`. 
#' Both of these choices are identical in the sense that `singletonMethod` is set to `"none"` whenever `singleton` is `NULL` and vice versa.
#' 
#' Information about uncertain primary suppressions due to forced cells can be found 
#' as described by parameters `unsafeInOutput` and `output`  (`= "all"`). 
#' When forced cells affect singleton problems, this is not implemented. 
#' Some information can be seen from warnings. 
#' This can also be seen by choosing `output = "secondary"` together 
#' with `unsafeInOutput = "ifany"` or `unsafeInOutput = "always"`. 
#' Then, negative indices from \code{\link[SSBtools]{GaussSuppression}} using 
#' `unsafeAsNegative = TRUE` will be included in the output. 
#' Singleton problems may, however, be present even if it cannot be seen as warning/output. 
#' In some cases, the problems can be detected by \code{\link{GaussSuppressDec}}. 
#' 
#' In some cases, cells that are forced, hidden, or primary suppressed can overlap.
#' For these situations, forced has precedence over hidden and primary. 
#' That is, if a cell is both forced and hidden, it will be treated as a forced cell and thus published.
#' Similarly, any primary suppression of a forced cell will be ignored 
#' (see parameter `whenPrimaryForced` to \code{\link[SSBtools]{GaussSuppression}}).
#' It is, however, meaningful to combine primary and hidden. 
#' Such cells will be protected while also being assigned the `NA` value in the `suppressed` output variable.
#'
#' @param data Input data, typically a data frame, tibble, or data.table. 
#'             If `data` is not a classic data frame, it will be coerced to one internally 
#'             unless `preAggregate` is `TRUE` and `aggregatePackage` is `"data.table"`.
#' @param dimVar The main dimensional variables and additional aggregating variables. This parameter can be  useful when hierarchies and formula are unspecified. 
#' @param freqVar A single variable holding counts (name or number).
#' @param numVar  Other numerical variables to be aggregated 
#' @param weightVar weightVar Weights (costs) to be used to order candidates for secondary suppression
#' @param charVar Other variables possibly to be used within the supplied functions
#' @param hierarchies List of hierarchies, which can be converted by \code{\link[SSBtools]{AutoHierarchies}}.
#'        Thus, the variables can also be coded by `"rowFactor"` or `""`, which correspond to using the categories in the data.
#' @param formula A model formula
#' @param maxN Suppression parameter forwarded to the supplied functions.  
#'      With the default `primary` function, [PrimaryDefault()], cells with frequency `<= maxN` are marked as primary suppressed,  
#'      and the default value of `maxN` is `3`. See details below.  
#'      The parameter is also used by [NContributorsRule()].  
#'      For advanced use cases, including setups with multiple primary functions, `maxN` can be specified  
#'      as a named list or vector. See each primary function’s documentation for details.
#' @param protectZeros Suppression parameter. 
#'        When `TRUE`, cells with zero frequency or value are set as primary suppressed. 
#'        Using the default `primary` function, `protectZeros` is by default set to `TRUE`. See details.
#' @param secondaryZeros Suppression parameter. 
#'        When `TRUE`, cells with zero frequency or value are prioritized to be published so that they are not secondary suppressed.
#'        Using the default `candidates` function, `secondaryZeros` is by default set to `FALSE`. 
#'        See details.
#' @param candidates GaussSuppression input or a function generating it (see details) Default: \code{\link{CandidatesDefault}}
#' @param primary    GaussSuppression input or a function generating it (see details) Default: \code{\link{PrimaryDefault}}
#' @param forced     GaussSuppression input or a function generating it (see details)
#' @param hidden     GaussSuppression input or a function generating it (see details)
#' @param singleton  GaussSuppression input or a function generating it (see details) Default: \code{\link{SingletonDefault}}
#' @param singletonMethod \code{\link[SSBtools]{GaussSuppression}} input. The default value depends on parameter `secondaryZeros` which depends on `candidates` (see details).   
#' @param printInc        \code{\link[SSBtools]{GaussSuppression}} input
#' @param output One of `"publish"` (default), `"inner"`, `"publish_inner"`, `"publish_inner_x"`, `"publish_x"`, 
#'                      `"inner_x"`, `"input2functions"` (input to supplied functions),
#'                      `"inputGaussSuppression"`, `"inputGaussSuppression_x"`, 
#'                      `"outputGaussSuppression"`  `"outputGaussSuppression_x"`,
#'                      `"primary"`,  `"secondary"` and `"all"`.
#'               Here "inner" means input data (possibly pre-aggregated) and 
#'               "x" means dummy matrix (as input parameter x).   
#'               All input to and output from \code{\link[SSBtools]{GaussSuppression}}, except `...`, are returned when `"outputGaussSuppression_x"`. 
#'               Excluding x and only input are also possible.
#'               The code `"all"` means all relevant output after all the calculations.
#'               Currently, this means the same as `"publish_inner_x"` extended with the matrices (or NULL) `xExtraPrimary`  and `unsafe`. 
#'               The former matrix is usually made by \code{\link{KDisclosurePrimary}}.
#'               This latter matrix contains the columns representing unsafe primary suppressions. 
#'               In addition to `x` columns corresponding to unsafe in ordinary output (see parameter `unsafeInOutput` below), 
#'               possible columns from  `xExtraPrimary` may also be included in the unsafe matrix (see details). 
#'               
#' @param x `x` (`modelMatrix`) and `crossTable` can be supplied as input instead of generating it from  \code{\link[SSBtools]{ModelMatrix}}
#' @param crossTable See above.  
#' @param preAggregate When `TRUE`, the data will be aggregated within the function to an appropriate level. 
#'        This is defined by the dimensional variables according to `dimVar`, `hierarchies` or `formula` and in addition `charVar`.
#' @param extraAggregate When `TRUE`, the data will be aggregated by the dimensional variables according to `dimVar`, `hierarchies` or `formula`.
#'                       The aggregated data and the corresponding x-matrix will only be used as input to the singleton 
#'                       function and \code{\link[SSBtools]{GaussSuppression}}. 
#'                       This extra aggregation is useful when parameter `charVar` is used.
#'                       Supply `"publish_inner"`, `"publish_inner_x"`, `"publish_x"` or `"inner_x"` as `output` to obtain extra aggregated results.
#'                       Supply `"inner"` or `"input2functions"` to obtain other results. 
#' @param structuralEmpty  When `TRUE`, output cells with no contributing inner cells (only zeros in column of `x`) 
#'                         are forced to be not primary suppressed. 
#'                         Thus, these cells are considered as structural zeros. 
#'                         When `structuralEmpty` is `TRUE`, the following error message is avoided: 
#'                         `Suppressed` `cells` `with` `empty` `input` `will` `not` `be` `protected.` 
#'                         `Extend` `input` `data` `with` `zeros?`.    
#'                         When `removeEmpty` is `TRUE` (see "`...`" below), `structuralEmpty` is superfluous    
#' @param extend0  Data is automatically extended by `Extend0` when `TRUE`.
#'                 Can also be set to `"all"` which means that input codes in hierarchies are considered in addition to those in data.   
#'                 Parameter `extend0` can also be specified as a list meaning parameter `varGroups` to `Extend0`. 
#' @param spec `NULL` or a named list of arguments that will act as default values.
#' @param specLock When `TRUE`, arguments in `spec` cannot be changed.       
#' @param freqVarNew Name of new frequency variable generated when input `freqVar` is NULL and `preAggregate` is TRUE.  
#'                   Default is `"freq"` provided this is not found in `names(data)`.    
#' @param nUniqueVar Name of variable holding the number of unique contributors.
#'                   This variable will be generated in the `extraAggregate` step.
#'                   Default is `"nUnique"` provided this is not found in `names(data)`.
#'                   If an existing variable is passed as input, 
#'                   this variable will apply only when `preAggregate`/`extraAggregate` is not done.
#' @param  forcedInOutput Whether to include `forced` as an output column.      
#'               One of `"ifNonNULL"` (default), `"always"`, `"ifany"` and `"no"`. 
#'               In addition, `TRUE` and `FALSE` are allowed as alternatives to  `"always"` and `"no"`.   
#' @param  unsafeInOutput Whether to include `usafe` as an output column.
#'               One of `"ifForcedInOutput"` (default), `"always"`, `"ifany"` and `"no"`. 
#'               In addition, `TRUE` and `FALSE` are allowed as alternatives to  `"always"` and `"no"`.
#'               see details. 
#'               
#' @param  lpPackage 
#'  * **`lpPackage`:**
#'                   When non-NULL, intervals by \code{\link{ComputeIntervals}} 
#'                   will be included in the output.
#'                   See its documentation for valid parameter values for 'lpPackage'.
#'                   If, additionally, at least one of the two \code{\link{RangeLimitsDefault}} parameters below is specified, 
#'                   further suppression will be performed to satisfy the interval width requirements.
#'        Then, the values in the output variable `suppressed_integer` means: 
#'                   no suppression (0), 
#'                   primary suppression (1), 
#'                   secondary suppression (2), 
#'                   additional suppression applied by an interval algorithm limited to linearly independent cells (3), 
#'                   and further suppression according to the final gauss algorithm (4).
#'        Intervals, `[lo_1, up_1]`, are intervals calculated prior to additional suppression.         
#'    * **`rangePercent`:** Required interval width expressed as a percentage
#'    * **`rangeMin`:** Minimum required width of the interval
#'                                 
#' @param aggregatePackage Package used to preAggregate/extraAggregate. 
#'                         Parameter `pkg` to \code{\link[SSBtools]{aggregate_by_pkg}}.
#' @param aggregateNA Whether to include NAs in the grouping variables while preAggregate/extraAggregate. 
#'                    Parameter `include_na` to \code{\link[SSBtools]{aggregate_by_pkg}}. 
#'                    Note that NAs will not be present in the output table's dimensions regardless of the value of `aggregateNA`.
#'                    When using the formula interface, this is controlled by the `NAomit` parameter (default `TRUE`),
#'                    which is passed to the function [SSBtools::Formula2ModelMatrix()].
#'                    It is through this use of the formula interface that NAs in the input data make sense. 
#'                    Note that under normal circumstances, grouping variables should not use NA to represent a category.
#'                    As such, if NAs are present in the grouping variables, using the `dimVar` or `hierarchies` interfaces 
#'                    will result in errors.
#' @param aggregateBaseOrder Parameter `base_order` to \code{\link[SSBtools]{aggregate_by_pkg}},
#'                           used when preAggregate/extraAggregate. 
#'                           The parameter does not affect the ordering of ordinary output. 
#'                           Therefore, the default is set to `FALSE` to avoid unnecessary sorting operations.  
#'                           The parameter will have impact when, e.g `output = "inner"`.
#' @param rowGroupsPackage Parameter `pkg` to \code{\link[SSBtools]{RowGroups}}.
#'               The parameter is input to \code{\link[SSBtools]{Formula2ModelMatrix}} 
#'               via \code{\link[SSBtools]{ModelMatrix}}. 
#' @param linkedGauss Controls linked table suppression. Accepted values are described in the 
#'        documentation for [SuppressLinkedTables()]. 
#'        See also the note and the corresponding examples, which 
#'        demonstrate usage with alternative function interfaces. 
#'        In addition, `linkedGauss = "global"` is allowed and corresponds to standard execution 
#'        (i.e., when `linkedGauss` is not specified). 
#'        When `linkedGauss` is used, the `formula` parameter should be provided as a list of formulas. 
#'        Alternatively, `formula` may have an attribute `"table_formulas"` containing such a list.
#'        See also the `linkedTables` parameter below.
#' @param linkedIntervals  Determines how interval calculations, 
#'        triggered by the `lpPackage` parameter, are performed when `linkedGauss` is not `"global"`.  
#'        When `linkedGauss = "global"`, interval settings in `linkedIntervals` are ignored.  
#'        For allowed values and detailed behaviour, see the documentation of [SuppressLinkedTables()].
#'        
#'  - Note: With `linkedIntervals = "local-bdiag"`, common cells may have different table-specific intervals. 
#'    Since the output shows one interval per cell, it is constructed using the maximum lower bound and 
#'    minimum upper bound across the tables.
#' @param recordAware Parameter associated with `linkedGauss`. See [SuppressLinkedTables()].  
#' @param collapseAware Parameter associated with `linkedGauss`.
#'        In the linked‑tables algorithm, the model matrix is first *collapsed* by
#'        removing duplicate rows. 
#'        When `collapseAware = TRUE`, every cell that remains numerically derivable 
#'        after a pre‑aggregation corresponding to this row reduction will be treated 
#'        as a common cell. This
#'        maximizes coordination across tables, given the duplicate‑row removal,
#'        while adding limited additional computational overhead. In particular,
#'        the suppression algorithm automatically accounts for cells in one table
#'        that are sums of cells in another table.
#'        Note that any cell that `recordAware = TRUE` would introduce is already
#'        included automatically when `collapseAware = TRUE`.
#' @param linkedTables A list specifying how the tables referenced in the `formula` 
#'   parameter should be combined for use in the linked-tables algorithm. 
#'   Each element in the list contains one or more names of the tables in `formula`. 
#'   The corresponding tables will be combined and treated as a single table by the algorithm.
#'   For example: `linkedTables = list(c("table_1", "table_3"), "table_2")`.
#'   If `NULL` (default), each table in `formula` is used individually.
#' 
#' @param action_unused_dots Character string controlling how unused arguments
#'   in `...` are handled. Internally uses [ellipsis::check_dots_used()] with a
#'   custom action. One of "warn", "abort", "inform", or "none". The value "none"
#'   disables the check entirely. The default is taken from
#'   `getOption("GaussSuppression.action_unused_dots")`, falling back to "warn"
#'   if the option is not set. Users can change the default globally with e.g.
#'   `options(GaussSuppression.action_unused_dots = "abort")`.
#'
#' @param allowed_unused_dots Character vector of argument names ignored by the
#'   unused-argument check. May be useful when this function is wrapped by
#'   another function, or in other cases where a correctly spelled argument is
#'   nevertheless not registered as used. The default is taken from
#'   `getOption("GaussSuppression.allowed_unused_dots")`, falling back to
#'   `character(0)` if the option is not set. Users can change the default
#'   globally with e.g.
#'   `options(GaussSuppression.allowed_unused_dots = c("plotColor", "lineType"))`.
#'   
#' @param ... Further arguments to be passed to the supplied functions and to \code{\link[SSBtools]{ModelMatrix}} (such as `inputInOutput` and `removeEmpty`).
#'
#' @return Aggregated data with suppression information
#' @export
#' @importFrom SSBtools GaussSuppression ModelMatrix Extend0 NamesFromModelMatrixInput SeqInc aggregate_by_pkg Extend0fromModelMatrixInput IsExtend0 CheckInput combine_formulas
#' @importFrom Matrix crossprod as.matrix
#' @importFrom stats aggregate as.formula delete.response terms
#' @importFrom utils flush.console
#' @importFrom methods hasArg
#' @importFrom rlang warn
#' @importFrom ellipsis check_dots_used
#' 
#' @author Øyvind Langsrud and Daniel Lupp
#'
#' @examples
#' 
#' z1 <- SSBtoolsData("z1")
#' GaussSuppressionFromData(z1, 1:2, 3)
#' 
#' z2 <- SSBtoolsData("z2")
#' GaussSuppressionFromData(z2, 1:4, 5, protectZeros = FALSE)
#' 
#' 
#' # Data as in GaussSuppression examples
#' df <- data.frame(values = c(1, 1, 1, 5, 5, 9, 9, 9, 9, 9, 0, 0, 0, 7, 7), 
#'                  var1 = rep(1:3, each = 5), var2 = c("A", "B", "C", "D", "E"))
#' 
#' GaussSuppressionFromData(df, c("var1", "var2"), "values")
#' GaussSuppressionFromData(df, c("var1", "var2"), "values", formula = ~var1 + var2, maxN = 10)
#' GaussSuppressionFromData(df, c("var1", "var2"), "values", formula = ~var1 + var2, maxN = 10,
#'       protectZeros = TRUE, # Parameter needed by SingletonDefault and default not in primary  
#'       primary = function(freq, crossTable, maxN, ...) 
#'                    which(freq <= maxN & crossTable[[2]] != "A" & crossTable[, 2] != "C"))
#'                    
#' # Combining several primary functions 
#' # Note that NA & c(TRUE, FALSE) equals c(NA, FALSE)                      
#' GaussSuppressionFromData(df, c("var1", "var2"), "values", formula = ~var1 + var2, maxN = 10, 
#'        primary = c(function(freq, maxN, protectZeros = TRUE, ...) freq >= 45,
#'                    function(freq, maxN, ...) freq <= maxN,
#'                    function(crossTable, ...) NA & crossTable[[2]] == "C",  
#'                    function(crossTable, ...) NA & crossTable[[1]]== "Total" 
#'                                                 & crossTable[[2]]== "Total"))                    
#'                    
#' # Similar to GaussSuppression examples
#' GaussSuppressionFromData(df, c("var1", "var2"), "values", formula = ~var1 * var2, 
#'        candidates = NULL, singleton = NULL, protectZeros = FALSE, secondaryZeros = TRUE)
#' GaussSuppressionFromData(df, c("var1", "var2"), "values", formula = ~var1 * var2, 
#'        singleton = NULL, protectZeros = FALSE, secondaryZeros = FALSE)
#' GaussSuppressionFromData(df, c("var1", "var2"), "values", formula = ~var1 * var2, 
#'        protectZeros = FALSE, secondaryZeros = FALSE)
#' 
#'               
#' # Examples with zeros as singletons
#' z <- data.frame(row = rep(1:3, each = 3), col = 1:3, freq = c(0, 2, 5, 0, 0, 6:9))
#' GaussSuppressionFromData(z, 1:2, 3, singleton = NULL) 
#' GaussSuppressionFromData(z, 1:2, 3, singletonMethod = "none") # as above 
#' GaussSuppressionFromData(z, 1:2, 3)
#' GaussSuppressionFromData(z, 1:2, 3, protectZeros = FALSE, secondaryZeros = TRUE, singleton = NULL)
#' GaussSuppressionFromData(z, 1:2, 3, protectZeros = FALSE, secondaryZeros = TRUE)      
GaussSuppressionFromData = function(data, dimVar = NULL, freqVar=NULL, 
                                    ...,
                                    numVar = NULL, 
                                    weightVar = NULL, charVar = NULL, #  freqVar=NULL, numVar = NULL, name
                                    hierarchies = NULL, formula = NULL,
                           maxN = suppressWarnings(formals(c(primary)[[1]])$maxN), 
                           protectZeros = suppressWarnings(formals(c(primary)[[1]])$protectZeros), 
                           secondaryZeros = suppressWarnings(formals(candidates)$secondaryZeros),
                           candidates = CandidatesDefault,
                           primary = PrimaryDefault,
                           forced = NULL,
                           hidden = NULL,
                           singleton = SingletonDefault,
                           singletonMethod = ifelse(secondaryZeros, "anySumNOTprimary", "anySum"),
                           printInc = TRUE,
                           output = "publish", x = NULL, crossTable = NULL,
                           preAggregate = is.null(freqVar),
                           extraAggregate = preAggregate & !is.null(charVar), 
                           structuralEmpty = FALSE, 
                           extend0 = FALSE,
                           spec = NULL,
                           specLock = FALSE, 
                           freqVarNew = rev(make.unique(c(names(data), "freq")))[1],
                           nUniqueVar = rev(make.unique(c(names(data), "nUnique")))[1],
                           forcedInOutput = "ifNonNULL",
                           unsafeInOutput = "ifForcedInOutput",
                           lpPackage = NULL, 
                           aggregatePackage = "base",
                           aggregateNA = TRUE,
                           aggregateBaseOrder = FALSE,
                           rowGroupsPackage = aggregatePackage,
                           linkedGauss = NULL,
                           linkedIntervals = ifelse(linkedGauss == "local-bdiag", "local-bdiag", "super-consistent"),
                           recordAware = TRUE,
                           collapseAware = FALSE,
                           linkedTables = NULL,
                           action_unused_dots  = getOption("GaussSuppression.action_unused_dots", "warn"),
                           allowed_unused_dots = getOption("GaussSuppression.allowed_unused_dots", character(0))
                           ){ 
  if (!is.null(spec)) {
    if (is.call(spec)) {
      spec <- eval(spec)
    }
    if (is.list(spec)) {
      if (length(names(spec)[!(names(spec) %in% c("", NA))]) == length(spec)) {
        sysCall <- match.call()  #  sys.call() is similar to match.call, but does not expand the argument name (needed here)
        sysCall[["spec"]] <- NULL
        names_spec <- names(spec)
        names_spec_in_names_sysCall <- names_spec %in% names(sysCall)
        specLock <- any(c(specLock, spec[["specLock"]]))
        if (specLock) {
          if (any(names_spec_in_names_sysCall)) {
            stop(paste("Non-allowed argument(s) due to specLock:", paste(names_spec[names_spec_in_names_sysCall], collapse = ", ")))
          }
        } else {
          names_spec <- names_spec[!names_spec_in_names_sysCall]
        }
        sysCall[names_spec] <- spec[names_spec]
        parentFrame <- parent.frame()
        return(eval(sysCall, envir = parentFrame))
      }
    }
    stop("spec must be a properly named list")
  }
  
  CheckInput(action_unused_dots, type = "character", alt = c("warn", "abort", "inform", "none"), okNULL = FALSE)
  
  if (is.character(output)) {
    if (output %in% c("inner", "inner_x", "input2functions", "primary", "inputGaussSuppression", "inputGaussSuppression_x")) {
      action_unused_dots <- "none"
    }
  }
  
  if(action_unused_dots != "none") {
    # extra_allowed_unused since these arguments may not be registered as used 
    # in special cases, e.g. when no primary suppression occurs
    extra_allowed_unused <- c(
      "printXdim", "tolGauss", "whenEmptySuppressed", # SSBtools::GaussSuppression
      "whenPrimaryForced", "iWait", "iFunction",      # SSBtools::GaussSuppression
      "avoidHierarchical",    # SSBtools::Extend0fromModelMatrixInput and  SSBtools::FormulaSums
      "hierarchical_extend0"  # SSBtools::Extend0fromModelMatrixInput
      )
    allowed_unused_dots <- unique(c(allowed_unused_dots, extra_allowed_unused)) 
    if (hasArg("avoidHierarchical") & hasArg("avoid_hierarchical")) {   
      # i.e. called from SSBtools::tables_by_formulas()
      allowed_unused_dots <- unique(c(allowed_unused_dots, "avoid_hierarchical"))
    }
    rlang_warn_extra <- generate_rlang_warn_extra(action_unused_dots, 
            note = "See arguments `action_unused_dots` and `allowed_unused_dots` in `?GaussSuppressionFromData`.")
    ellipsis::check_dots_used(action = rlang_warn_extra)
    touch_dots <- generate_touch_dots(allowed_unused_dots)
    touch_dots(...)
  }
  
  
  CheckInput(linkedGauss, type = "character", alt = c("global", "local", "consistent", "back-tracking", "local-bdiag", "super-consistent"), okNULL = TRUE)
  
  if (!is.null(lpPackage) & !is.null(linkedGauss)) {
    if (linkedGauss != "global") {
      check_parameter_linkedIntervals(linkedGauss, linkedIntervals, TRUE)
    }
  }
  
  if (is.list(formula)) {
    table_formulas <- formula
    formula <- combine_formulas(table_formulas)
  } else {
    if (!is.null(linkedGauss)) {
      table_formulas <- attr(formula, "table_formulas")
      if (is.null(table_formulas)) {
        on.exit(add = FALSE)  # Avoids unused-dots check on error
        stop("With 'linkedGauss', 'formula' must be either a list of formulas or have a 'table_formulas' attribute.")
      } 
    }
  }
  if (!is.null(linkedGauss) & !is.null(linkedTables)) {
    if (!all(unlist(linkedTables) %in% names(table_formulas))) {
      on.exit(add = FALSE)  # Avoids unused-dots check on error
      stop("All tables in 'linkedTables' must exist in 'formula'.")
    }
    table_formulas <- lapply(linkedTables, function(x) combine_formulas(table_formulas[x]))
    formula <- combine_formulas(table_formulas)
  }
  
  if (!is.null(lpPackage) & !is.null(linkedGauss)) {
    # lpPackage <- NULL
    # warning("The 'lpPackage' parameter is currently ignored when 'linkedGauss' is specified.")
  }

  # Possible development function as input
  # Special temporary feature 
  if (is.function(output)) {
    OutputFunction <- output
    output <- "publish"
  } else {
    if (!is.null(lpPackage)) {
      if (!requireNamespace(lpPackage, quietly = TRUE)) {
        on.exit(add = FALSE)  # Avoids unused-dots check on error
        stop(paste0("Package '", lpPackage, "' is not available."))
      }
      if (hasArg(rangePercent) | hasArg(rangeMin)) {
        # if (!(hasArg(rangePercent) & hasArg(rangeMin))) {
        #   stop("Both rangePercent and rangeMin must be specified, not just one of them.")
        # }
        OutputFunction <- OutputFixRiskyIntervals
      } else {
        OutputFunction <- OutputIntervals
      }
      if( !(identical(linkedGauss, "global") | is.null(linkedGauss))) {    ### INTERVALS ANOTHER WAY 
        OutputFunction <- NULL
      } 
    } else {
      OutputFunction <- NULL
    }
  }
  
  
  if(!(output %in% c("publish", "inner", "publish_inner", "publish_inner_x", "publish_x", "inner_x", "input2functions", 
                     "inputGaussSuppression", "inputGaussSuppression_x", "outputGaussSuppression", "outputGaussSuppression_x",
                     "primary", "secondary", "all", "pre_gauss_env"))) {
    on.exit(add = FALSE)  # Avoids unused-dots check on error
    stop('Allowed values of parameter output are "publish", "inner", "publish_inner", "publish_inner_x", "publish_x", "inner_x", "input2functions",
         "inputGaussSuppression", "inputGaussSuppression_x", "outputGaussSuppression", "outputGaussSuppression_x",
                     "primary", "secondary", "all", "pre_gauss_env")')
  }
  
  
  innerReturn <- output %in% c("inner", "publish_inner", "publish_inner_x", "inner_x", "all")

  force(preAggregate)
  force(extraAggregate)
  force(nUniqueVar)
  
  if (length(singletonMethod)) { # Default is logical(0) when secondaryZeros is NULL
    if (all(singletonMethod == "none")) {
      singleton <- NULL
    }
  }
  if (is.null(singleton)) {
    singletonMethod <- "none"
  }
  if (!length(singletonMethod)) {
    on.exit(add = FALSE)  # Avoids unused-dots check on error
    stop("A value of singletonMethod is required.")
  }
  
  if (is.logical(forcedInOutput)) {
    if (forcedInOutput) {
      forcedInOutput <- "always"
    } else {
      forcedInOutput <- "no"
    }
  }
  if (is.logical(unsafeInOutput)) {
    if (unsafeInOutput) {
      unsafeInOutput <- "always"
    } else {
      unsafeInOutput <- "no"
    }
  }
  
  Vars_r_rnd = function(r_rnd = character(0), ...){
    r_rnd
  }
  r_rnd <- Vars_r_rnd(...)
  
  
  # Trick to ensure missing defaults transferred to NULL. Here is.name a replacement for rlang::is_missing.
  if (is.name(maxN)) maxN <- NULL
  if (is.name(protectZeros)) protectZeros <- NULL
  if (is.name(secondaryZeros)) secondaryZeros <- NULL
  
  if (structuralEmpty) {
    if (!is.function(c(primary)[[1]])) {  # Also handle non-function input 
      primary_values <- primary
      primary <- function(...) primary_values
    }
    primary <- c(primary, function(x, ...) NA & colSums(x) == 0)
  }
  
  if (is.null(dimVar) & is.null(hierarchies) & is.null(formula) & is.null(x)) {
    on.exit(add = FALSE)  # Avoids unused-dots check on error
    stop("dimVar, hierarchies or formula must be specified")
  }
  
  if (!(preAggregate & aggregatePackage == "data.table")) {
    data <- as.data.frame(data)
  }
  
  dimVar <- names(data[1, dimVar, drop = FALSE])
  freqVar <- names(data[1, freqVar, drop = FALSE])
  numVar <- names(data[1, numVar, drop = FALSE])
  weightVar <- names(data[1, weightVar, drop = FALSE])
  charVar <- names(data[1, charVar, drop = FALSE])
  
  
  if (preAggregate | extraAggregate){
    if(nUniqueVar %in% names(data)){
      warning("nUniqueVar in input data ignored when preAggregate/extraAggregate")
    }
  }
  
  isExtend0 <- IsExtend0(extend0)
  
  if (isExtend0 | preAggregate | extraAggregate | innerReturn | (is.null(hierarchies) & is.null(formula) & !length(dimVar))) {
    if (printInc & preAggregate) {
      cat("[preAggregate ", dim(data)[1], "*", dim(data)[2], "->", sep = "")
      flush.console()
    }
    
    dVar <- NamesFromModelMatrixInput(hierarchies = hierarchies, formula = formula, dimVar = dimVar)
    
    if (!length(dVar)) {
      freqPlusVarName <- c(freqVar, numVar, weightVar, charVar)
      if (!length(freqPlusVarName)) {
        dVar <- names(data)
      } else {
        dVar <- names(data[1, !(names(data) %in% freqPlusVarName), drop = FALSE])
      }
    }
    dVar <- unique(dVar)
    
    if (!length(dimVar)){
      dimVar <- dVar
    }
    
    if (preAggregate) {
      if (!length(freqVar)) {
        freqVar <- freqVarNew
        data[[freqVar]] <- 1L # entire data.frame is copied into memory when adding 1s. Improve?  
      } 
      # data <- aggregate(data[unique(c(freqVar, numVar, weightVar))], data[unique(c(dVar, charVar))], sum)
      data <- aggregate_by_pkg(
        data = data,
        by = unique(c(dVar, charVar)),
        var = unique(c(freqVar, numVar, weightVar, r_rnd)),
        pkg =  aggregatePackage,
        include_na = aggregateNA,
        fun = sum,
        base_order = aggregateBaseOrder)
        
      if (printInc) {
        cat(dim(data)[1], "*", dim(data)[2], "]\n", sep = "")
        flush.console()
      }
    } else {
      ### START ### preliminary hack to include sWeightVar in SuppressDominantCells
      data <- data[unique(c(dVar, charVar, freqVar, numVar, weightVar, r_rnd, MoreVars(...)))]
      ### END ###  preliminary hack
      # data <- data[unique(c(dVar, charVar, freqVar, numVar, weightVar))]
    }
  }
  
  if (isExtend0) {
    if (printInc) {
      cat("[extend0 ", dim(data)[1], "*", dim(data)[2], "->", sep = "")
      flush.console()
    }
    
    data <- Extend0fromModelMatrixInput(data = data, 
                                        freqName = freqVar, 
                                        hierarchies = hierarchies, 
                                        formula = formula,
                                        dimVar = dimVar,
                                        extend0 = extend0, 
                                        dVar = dVar, ...)
    if (printInc) {
      cat(dim(data)[1], "*", dim(data)[2], "]\n", sep = "")
      flush.console()
    }
  }
  
  
  if(innerReturn){
    attr(data, "freqVar") <- freqVar
    attr(data, "weightVar") <- weightVar
    attr(data, "numVar") <- numVar
  }
  

  if (output == "inner") {
    return(data)
  }
  
  if (is.null(x)) {
    if (is.null(formula) & is.null(hierarchies)) {
      x <- SSBtools::ModelMatrix(data[, dimVar, drop = FALSE], crossTable = TRUE, rowGroupsPackage = rowGroupsPackage, ...)
    } else {
      x <- SSBtools::ModelMatrix(data, hierarchies = hierarchies, formula = formula, crossTable = TRUE, rowGroupsPackage = rowGroupsPackage, ...)
    }
    crossTable <- as.data.frame(x$crossTable)  # fix i ModelMatrix 
    x <- x$modelMatrix
  }
  
  if (output == "inner_x") {
    return(list(inner = data, x = x))
  }
  
  if (!length(freqVar)) {
    freq <- NULL
  } else {
    freq <- as.vector(as.matrix(crossprod(x, as.matrix(data[, freqVar, drop = FALSE]))))
  }
  
  if (!length(numVar)) {
    num <- NULL
  } else {
    num <- as.data.frame(as.matrix(crossprod(x, as.matrix(data[, numVar, drop = FALSE]))))
  }
  
  
  if (!length(weightVar)) {
    weight <- NULL
  } else {
    weight <- as.vector(as.matrix(crossprod(x, as.matrix(data[, weightVar, drop = FALSE]))))
  }
  
  if (output == "input2functions")           return(list(crossTable = crossTable, x = x, freq = freq, num = num, weight = weight, maxN = maxN, protectZeros = protectZeros, secondaryZeros = secondaryZeros, data = data, freqVar = freqVar, numVar = numVar, weightVar = weightVar, charVar = charVar, dimVar = dimVar, aggregatePackage = aggregatePackage, aggregateNA = aggregateNA, aggregateBaseOrder = aggregateBaseOrder, rowGroupsPackage = rowGroupsPackage, structuralEmpty = structuralEmpty, ...))
  
  if (is.function(candidates)) candidates <-  candidates(crossTable = crossTable, x = x, freq = freq, num = num, weight = weight, maxN = maxN, protectZeros = protectZeros, secondaryZeros = secondaryZeros, data = data, freqVar = freqVar, numVar = numVar, weightVar = weightVar, charVar = charVar, dimVar = dimVar, aggregatePackage = aggregatePackage, aggregateNA = aggregateNA, aggregateBaseOrder = aggregateBaseOrder, rowGroupsPackage = rowGroupsPackage, structuralEmpty = structuralEmpty, ...)
  
  if (is.function(primary) | is.list(primary))  
               primary <-     Primary(primary = primary, crossTable = crossTable, x = x, freq = freq, num = num, weight = weight, maxN = maxN, protectZeros = protectZeros, secondaryZeros = secondaryZeros, data = data, freqVar = freqVar, numVar = numVar, weightVar = weightVar, charVar = charVar, dimVar = dimVar, aggregatePackage = aggregatePackage, aggregateNA = aggregateNA, aggregateBaseOrder = aggregateBaseOrder, rowGroupsPackage = rowGroupsPackage, structuralEmpty = structuralEmpty, ...)
               if (output == "primary") return(primary)
  
  if (is.function(forced))         forced <-      forced(crossTable = crossTable, x = x, freq = freq, num = num, weight = weight, maxN = maxN, protectZeros = protectZeros, secondaryZeros = secondaryZeros, data = data, freqVar = freqVar, numVar = numVar, weightVar = weightVar, charVar = charVar, dimVar = dimVar, aggregatePackage = aggregatePackage, aggregateNA = aggregateNA, aggregateBaseOrder = aggregateBaseOrder, rowGroupsPackage = rowGroupsPackage, structuralEmpty = structuralEmpty, ...)
  
  if (is.function(hidden))         hidden <-      hidden(crossTable = crossTable, x = x, freq = freq, num = num, weight = weight, maxN = maxN, protectZeros = protectZeros, secondaryZeros = secondaryZeros, data = data, freqVar = freqVar, numVar = numVar, weightVar = weightVar, charVar = charVar, dimVar = dimVar, aggregatePackage = aggregatePackage, aggregateNA = aggregateNA, aggregateBaseOrder = aggregateBaseOrder, rowGroupsPackage = rowGroupsPackage, structuralEmpty = structuralEmpty, ...)

  
  if(extraAggregate){
    if (printInc) {
      cat("[extraAggregate ", dim(data)[1], "*", dim(data)[2], "->", sep = "")
      flush.console()
    }
    uniqueCharVar <- charVar[!(charVar %in% dVar)]
    if (length(uniqueCharVar)) {
      if (length(uniqueCharVar) == 1) {
        funA <- function(x) x[1]
      } else {
        funA <- function(x) {
          if (all(x == x[1])) {
            return(x[1])
          }
          NA_character_
        }
      }
      charData <- aggregate_by_pkg(
        data = data,
        by = unique(dVar),
        var = uniqueCharVar,
        pkg =  aggregatePackage,
        include_na = aggregateNA,
        fun = funA,
        base_order = aggregateBaseOrder) 
    }
    data[[nUniqueVar]] <- 1L
    #data <- aggregate(data[unique(c(freqVar, numVar, weightVar, nUniqueVar))], data[unique(dVar)], sum) 
    data <- aggregate_by_pkg(
      data = data,
      by = unique(dVar),
      var = unique(c(freqVar, numVar, weightVar, r_rnd, nUniqueVar)),
      pkg =  aggregatePackage,
      include_na = aggregateNA,
      fun = sum,
      base_order = aggregateBaseOrder) 
    if (printInc) {
      cat(dim(data)[1], "*", dim(data)[2], "] ", sep = "")
      flush.console()
    }
    
    if(innerReturn){
      attr(data, "freqVar") <- freqVar
      attr(data, "weightVar") <- weightVar
      attr(data, "numVar") <- numVar
    }
  
    if (is.null(formula) & is.null(hierarchies)) {
      xExtra <- SSBtools::ModelMatrix(data[, dimVar, drop = FALSE], crossTable = TRUE, rowGroupsPackage = rowGroupsPackage, ...)
    } else {
      xExtra <- SSBtools::ModelMatrix(data, hierarchies = hierarchies, formula = formula, crossTable = TRUE, rowGroupsPackage = rowGroupsPackage, ...)
    }
    if (printInc) {
      cat("Checking .")
      flush.console()
    }
    if (length(uniqueCharVar)) {
      if (printInc) {
        cat(".")
        flush.console()
      }
      if (!isTRUE(all.equal(data[unique(dVar)], charData[unique(dVar)]))) {
        on.exit(add = FALSE)  # Avoids unused-dots check on error
        stop("dim variables not equal")
      }
      data[uniqueCharVar] <- charData[uniqueCharVar]
      rm(charData)
      if (length(uniqueCharVar) == 1) {    # already NA when length(uniqueCharVar) > 1
        data[uniqueCharVar][data[[nUniqueVar]] > 1, ] <- NA  # uniqueCharVar created as the first row is ok when the first row is the only row
      }
    }
    if (printInc) {
      cat(".")
      flush.console()
    }
    if(!isTRUE(all.equal(crossTable, as.data.frame(xExtra$crossTable)))){
      on.exit(add = FALSE)  # Avoids unused-dots check on error
      stop("crossTables not equal")
    }
    x <- xExtra$modelMatrix
    rm(xExtra)
  }
  
  #if (is.function(singleton))   singleton <-   singleton(crossTable = crossTable, x = x, freq = freq, num = num, weight = weight, maxN = maxN, protectZeros = protectZeros, secondaryZeros = secondaryZeros, data = data, freqVar = freqVar, numVar = numVar, weightVar = weightVar, charVar = charVar, dimVar = dimVar, primary = primary, ...)
  
  m <- ncol(x)
  
  if (is.null(candidates)) candidates <- 1:m
  
  freq_ <- freq
  num_ <- num
  weight_ <- weight 
  if (is.null(freq)) freq <- matrix(0, m, 0)
  if (is.null(num)) num <- matrix(0, m, 0)
  if (is.null(weight)) weight <- matrix(0, m, 0)
  
  if(extraAggregate){
    if (printInc) {
      cat(".")
      flush.console()
    }
    if(!isTRUE(all.equal(
      as.matrix(crossprod(x, as.matrix(data[, c(freqVar, numVar, weightVar), drop = FALSE]))), 
      as.matrix(cbind(freq, num, weight)),
      check.attributes = FALSE, check.names = FALSE)))
      warning("(freq, num, weight) all not equal when checked by all.equal")
    if (printInc) {
      cat(".\n")
      flush.console()
    }
  }
  
  # hack
  if(is.list(primary)){
    if (!is.null(primary$numExtra)) {
      num <- cbind(num, primary$numExtra)
    }
    xExtraPrimary <- primary$xExtraPrimary
    primary <- primary[[1]]
  } else {
    xExtraPrimary <- NULL
  }
  
  if (!is.null(xExtraPrimary) & extraAggregate) {
    on.exit(add = FALSE)  # Avoids unused-dots check on error
    stop("Combination of xExtraPrimary and extraAggregate is not implemented")
  }
  
  if (is.function(singleton)){   
    singleton <-   singleton(crossTable = crossTable, x = x, 
                             freq = freq_, num = num_, weight = weight_, 
                             maxN = maxN, protectZeros = protectZeros, secondaryZeros = secondaryZeros, 
                             data = data, freqVar = freqVar, numVar = numVar, weightVar = weightVar, 
                             charVar = charVar, dimVar = dimVar, 
                             nUniqueVar = nUniqueVar, primary = primary, 
                             aggregatePackage = aggregatePackage, 
                             aggregateNA = aggregateNA, 
                             aggregateBaseOrder = aggregateBaseOrder, 
                             rowGroupsPackage = rowGroupsPackage, 
                             structuralEmpty = structuralEmpty, ...)
  }
  
  if(!is.null(forced)){
    if (!is.logical(forced)) {   # logical allowed in  SSBtools::GaussSuppression
      if (length(forced)) {
        if (min(forced) < 0 | max(forced) > m) {
          on.exit(add = FALSE)  # Avoids unused-dots check on error
          stop("forced input outside range")
        }
      }
      forcedA <- rep(FALSE, m)
      forcedA[forced] <- TRUE
      forced <- forcedA
    } else {
      if(length(forced) != m){
        on.exit(add = FALSE)  # Avoids unused-dots check on error
        stop("wrong length of forced")
      }
    }
  } 

  
  if(output=="inputGaussSuppression_x"){
    return(list(candidates = candidates, primary = primary, forced = forced, hidden = hidden, singleton = singleton, singletonMethod = singletonMethod, printInc = printInc, xExtraPrimary = xExtraPrimary, x = x))
  }
  if(output=="inputGaussSuppression"){
    return(list(candidates = candidates, primary = primary, forced = forced, hidden = hidden, singleton = singleton, singletonMethod = singletonMethod, printInc = printInc, xExtraPrimary = xExtraPrimary))
  }
  
  z <- z_interval(..., freq = freq, freqVar = freqVar, num = num)
  rangeLimits <- RangeLimitsDefault(..., primary = primary, num = num, freq = freq, freqVar = freqVar)
  
  if( output %in% c("outputGaussSuppression", "outputGaussSuppression_x", "secondary")){
    rm(crossTable)
    rm(freq)
    num <- NULL
    rm(weight)
    rm(data)
  } 
  
  table_memberships <- NULL
  cell_grouping <- NULL
  if (!is.null(linkedGauss)) {
    if (linkedGauss == "super-consistent") {
      super_consistent <- TRUE
      linkedGauss <- "consistent"
    } else {
      super_consistent <- FALSE
    }
    if (linkedGauss %in% c("local", "consistent", "back-tracking", "local-bdiag")) {
      names(table_formulas) <- paste0("t", seq_len(length(table_formulas)))
      table_memberships <- as.data.frame(matrix(NA, ncol(x), length(table_formulas)))
      names(table_memberships) <- names(table_formulas)
      
      for (i in seq_along(table_formulas)) {
        table_memberships[[i]] <- SSBtools::formula_selection(x, table_formulas[[i]], logical = TRUE)
      }
      sum_1_table_memberships <- sum(table_memberships)
      if (collapseAware) {
        table_memberships <- collapse_aware_table_memberships(table_memberships, x)
      }
      sum_2_table_memberships <- sum(table_memberships)
      if (recordAware) {
        table_memberships <- record_consistent_table_memberships(table_memberships, x, aggregatePackage)
      }
      sum_3_table_memberships <- sum(table_memberships)
      if (recordAware & collapseAware) {
        if (sum_2_table_memberships != sum_3_table_memberships) {
          warning("recordAware matters when collapseAware is TRUE")
        }
      }
      cell_grouping <- linkedGauss == "consistent"
      iterBackTracking <- 0L
      if (linkedGauss %in% c("local", "consistent", "back-tracking", "local-bdiag")) {
        GaussSuppression <- gaussSuppression_linked
      }
      if (linkedGauss == "back-tracking") {
        cell_grouping <- NULL
        iterBackTracking <- Inf
      }
      if (linkedGauss == "local") {
        iterBackTracking <- "local"
      }
    }
  }
  
  if (output == "pre_gauss_env") {
    r_rnd <-as.matrix(crossprod(x, as.matrix(data[r_rnd])))
    rm(data)    # data needed when output %in% c("all", "publish_inner_x", "publish_inner")
    env <- environment()
    return(env)
  }
  
  
  
  # To calls to avoid possible error:  argument "whenEmptyUnsuppressed" matched by multiple actual arguments 
  if(hasArg("whenEmptyUnsuppressed") | !structuralEmpty){
    secondary <- GaussSuppression(x = x, candidates = candidates, primary = primary, forced = forced, hidden = hidden, singleton = singleton, singletonMethod = singletonMethod, printInc = printInc, xExtraPrimary = xExtraPrimary, 
                                  unsafeAsNegative = TRUE, table_memberships = table_memberships, cell_grouping = cell_grouping, iterBackTracking = iterBackTracking,
                                  super_consistent = super_consistent,
                                  z = z,
                                  rangeLimits = rangeLimits,
                                  lpPackage = lpPackage,
                                  linkedIntervals = linkedIntervals,
                                  ...)
  } else {
    secondary <- GaussSuppression(x = x, candidates = candidates, primary = primary, forced = forced, hidden = hidden, singleton = singleton, singletonMethod = singletonMethod, printInc = printInc, whenEmptyUnsuppressed = NULL, xExtraPrimary = xExtraPrimary, 
                                  unsafeAsNegative = TRUE, table_memberships = table_memberships, cell_grouping = cell_grouping, iterBackTracking = iterBackTracking, 
                                  super_consistent = super_consistent,
                                  z = z,
                                  rangeLimits = rangeLimits,
                                  lpPackage = lpPackage,
                                  linkedIntervals = linkedIntervals,
                                  ...)
  }
  
  # possible secondary with extra output 
  #   for intervals due to lpPackage
  # similar to primary 
  if (is.list(secondary)) {
    if (!is.null(num)) {
      num <- cbind(num, secondary[[2]])
    }
    secondary <- secondary[[1]]
  }
  
  # Use of special temporary feature
  if (!is.null(OutputFunction)) {
    environment(OutputFunction) <- environment()
    return(OutputFunction(...))
  }
  
  
  if (output == "secondary") {
    if (unsafeInOutput %in% c("ifany", "always")) {
      return(secondary)
    } else {
      return(secondary[secondary > 0])
    }
  }
  
  unsafePrimary <- -secondary[secondary < 0]
  secondary <- secondary[secondary > 0]
  
  if(output=="outputGaussSuppression_x"){
    return(list(secondary = secondary, candidates = candidates, primary = primary, forced = forced, hidden = hidden, singleton = singleton, singletonMethod = singletonMethod, printInc = printInc, xExtraPrimary = xExtraPrimary, x = x))
  }
  if(output=="outputGaussSuppression"){
    return(list(secondary = secondary, candidates = candidates, primary = primary, forced = forced, hidden = hidden, singleton = singleton, singletonMethod = singletonMethod, printInc = printInc, xExtraPrimary = xExtraPrimary))
  }
  
  suppressed <- rep(FALSE, m)
  suppressed[primary] <- TRUE
  primary <- suppressed
  suppressed[secondary] <- TRUE
  suppressed[hidden] <- NA
  suppressed[forced] <- FALSE
  
  
  if (length(freq)) {
    freq <- matrix(freq)
    colnames(freq) <- freqVar
  }
  if (length(weight)) {
    weight <- matrix(weight)
    colnames(weight) <- weightVar
  }
  
  if (ncol(num)) {
    colnames_num_in_fw <- colnames(num) %in% c(freqVar, weightVar)
    if (any(colnames_num_in_fw)) {
      num <- num[, !colnames_num_in_fw, drop = FALSE]
    }
  }
  
  forcedInOut <- NA
  if (is.null(forced)) {
    if (forcedInOutput == "always") {
      forced <- rep(FALSE, m)
      forcedInOut <- TRUE
    } else {
      forcedInOut <- FALSE
    }
  } else {
    if (forcedInOutput == "always") {
      forcedInOut <- TRUE
    }
    if (forcedInOutput == "ifNonNULL") {
      forcedInOut <- TRUE
    }
    if (forcedInOutput == "ifany") {
      forcedInOut <- any(forced)
    }
    if (forcedInOutput == "no") {
      forcedInOut <- FALSE
    }
  }
  if (is.na(forcedInOut)) {
    warning('Wrong forcedInOutput input treated as "ifNonNULL"')
    forcedInOut <- TRUE
  }
  
  
  unsafeInOut <- NA
  if (unsafeInOutput == "ifForcedInOutput") {
    unsafeInOut <- forcedInOut
  }
  if (unsafeInOutput == "always") {
    unsafeInOut <- TRUE
  }
  if (unsafeInOutput == "ifany") {
    unsafeInOut <- length(unsafePrimary) > 0
  }
  if (unsafeInOutput == "no") {
    unsafeInOut <- FALSE
  }
  if (is.na(unsafeInOut)) {
    warning('Wrong unsafeInOutput input treated as "ifForcedInOutput"')
    unsafeInOut <- forcedInOut
  }
  if (unsafeInOut) {
    unsafe <- rep(FALSE, m)
    unsafe[unsafePrimary[unsafePrimary <= m]] <- TRUE
    if (any(unsafe & !primary)) {
      warning("Calculation of unsafe failed. Non-primary found.")
    }
    unsafe <- matrix(unsafe)
    colnames(unsafe) <- "unsafe"
  } else {
    unsafe <- matrix(0, m, 0)
  }
  
  
  if (forcedInOut) {
    forced <- matrix(forced)
    colnames(forced) <- "forced"
  } else {
    forced <- matrix(0, m, 0)
  }
  
  publish <- cbind(as.data.frame(crossTable), freq, num, weight, primary = primary, forced, unsafe, suppressed = suppressed)
  rownames(publish) <- NULL
  
  startCol <- attr(x, "startCol", exact = TRUE)
  if (!is.null(startCol)) {
    attr(publish, "startRow") <- startCol
  }
  
  attr(publish, "totCode") <- FindTotCode2(x, crossTable)
  
  
  if(output == "all"){
    if( length(unsafePrimary) > 0){
      unsafe = x[, unsafePrimary[unsafePrimary <= m], drop = FALSE] # reuse object name unsafe here
      if(any(unsafePrimary > m) & !is.null(xExtraPrimary)){
        unsafePxEx = unsafePrimary[unsafePrimary > m] - m
        unsafePxEx = unsafePxEx[unsafePxEx <= ncol(xExtraPrimary)]
        unsafe = cbind(unsafe, xExtraPrimary[, unsafePxEx, drop = FALSE])
      }
      
    } else {
      unsafe = NULL
    }
    return(list(publish = publish, inner = data, x = x, xExtraPrimary = xExtraPrimary, unsafe = unsafe))
  }
  
  if (output == "publish_inner_x") {
    return(list(publish = publish, inner = data, x = x))
  }
  
  if (output == "publish_inner") {
    return(list(publish = publish, inner = data))
  }
  
  if (output == "publish_x") {
    return(list(publish = publish, x = x))
  }
  
  publish
}



### Part of hack to include sWeightVar in SuppressDominantCells
MoreVars = function(sWeightVar = character(0), ...){
  sWeightVar
}



record_consistent_table_memberships <- function(table_memberships, x, aggregatePackage) {
  dd <- DummyDuplicated(x, idx = TRUE, rnd = TRUE)
  table_dd <- table(dd)
  table_dd <- table_dd[table_dd > 1]
  if (!length(table_dd)) {
    return(table_memberships)
  }
  dd_duplicated <- as.integer(names(table_dd))
  selected <- dd %in% dd_duplicated
  selected_dd <- data.frame(selected_dd = dd[selected])
  
  selected_memberships <- aggregate_by_pkg(data = cbind(selected_dd, table_memberships[selected, , drop = FALSE]), 
                                           by = "selected_dd", 
                                           var = names(table_memberships), 
                                           pkg = aggregatePackage,
                                           fun = sum)
  for (i in SeqInc(2, ncol(selected_memberships))) {
    selected_memberships[[i]] <- as.logical(selected_memberships[[i]])
  }
  ma <- match(dd, selected_memberships[[1]])
  
  table_memberships[!is.na(ma), ] <- selected_memberships[ma[!is.na(ma)], -1]
  table_memberships
}


collapse_aware_table_memberships <- function(table_memberships, x, aggregatePackage) {
  
  r7 <- rnd_7(nrow(x))
  table_memberships_out <- table_memberships
  
  for(i in seq_along(table_memberships)){
    ti <- table_memberships[, i]
    idx <- DummyDuplicated(x[, ti, drop = FALSE], idx = TRUE, rows = TRUE, rnd = TRUE)
    useq <- SSBtools::UniqueSeq(idx)
    ord1 <- SortRows(cbind(idx, useq), index.return = TRUE)
    ord2 <- SortRows(cbind(idx, -useq), index.return = TRUE)
    cp1 <- Matrix::crossprod(x[ord1 , ,drop=FALSE], r7[ord1 , ,drop=FALSE])
    cp2 <- Matrix::crossprod(x[ord1 , ,drop=FALSE], r7[ord2 , ,drop=FALSE])
    table_memberships_out[, i] <- as.vector(rowSums(abs(cp2 -cp1)) == 0)
  }
  table_memberships_out
}


# Function to find vector with z-values for interval calculation
z_interval <- function(..., freq, freqVar, num, dominanceVar = NULL, intervalVar = NULL) {
  intervalVar <- intervalVar[1]  # Only single intervalVar is (for now) supported in functions where this is used
  if (identical(intervalVar, freqVar) | ncol(num) == 0) {
    z <- freq
  } else {
    if (is.null(intervalVar)) {
      if (is.null(dominanceVar)) {
        intervalVar <- names(num)[1]
      } else {
        intervalVar <- dominanceVar
      }
    }
    z <- num[[intervalVar]]
  }
  z
}



# The function generated by generate_rlang_warn_extra()
# Included here to avoid: All declared Imports should be used.
rlang_warn_extra <- function(message = NULL, ...) {
  message <- c(message, i = "See arguments `action_unused_dots` and `allowed_unused_dots`.")
  rlang::warn(message, ...)
}

generate_rlang_warn_extra <- function(
    action_unused_dots = "warn",
    note = "See arguments `action_unused_dots` and `allowed_unused_dots`.", 
    envir = parent.frame()) {
  fun_txt <- paste0("function (message = NULL, ...) { \n message <- c(message, i = \"", note, "\") \n  rlang::", action_unused_dots,"(message, ...) \n }")
  eval(parse(text = fun_txt), envir = envir)
}

generate_touch_dots <- function(allowed_unused_dots, envir = parent.frame()){
  a <- allowed_unused_dots
  fun_txt <- paste("function(", paste(paste(a,"= 1"), collapse = ", "), ", ...){\n ", paste(paste("force(", a, ")"), collapse = "\n "), "\n }")
  eval(parse(text = fun_txt), envir = envir)
}
