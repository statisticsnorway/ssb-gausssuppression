MA <- function(..., FUN) {
  a <- model_aggregate(..., fun = FUN, verbose = FALSE)
  a$primary <- FALSE
  a$suppressed <- FALSE
  a
}


KableMagnitudeTable <- function(data, ..., dimVar = NULL, numVar, timevar, 
                                contributorVar = NULL) {
  if (is.null(contributorVar)) {
    return(G(data, dim_var = dimVar, ..., 
             fun_vars = list(print_exp = list(f = numVar)), 
             FUN = c(f = function(x) {
               if (!length(x)) return("")
               paste(paste0(sprintf("%0.1f", x)), collapse = ",  ")
             }),
             mm_args = list(removeEmpty = FALSE), 
             fun = MA, print_expr = "print_exp", timevar = timevar))
  }
  G(data, dim_var = dimVar, ..., 
    fun_vars = list(print_exp = list(f = c("value", contributorVar))), 
    FUN = c(f = function(x, g) {
      if (!length(x)) return("")
      agg <- aggregate(x, list(g = g), sum)
      paste(paste0(agg$g, "=", sprintf("%0.1f", agg$x)), collapse = ", ")
      }), 
    mm_args = list(removeEmpty = FALSE), 
    fun = MA, print_expr = "print_exp", timevar = timevar)
}

