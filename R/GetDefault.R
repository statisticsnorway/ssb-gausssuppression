#' Get default value of a function
#' 
#' The value may be found in a spec. 
#' See \code{\link{PackageSpecs}}.
#' 
#' The result is evaluated if \code{\link{is.name}} returns `TRUE`.  
#' 
#'
#' @param fun A function
#' @param parameter parameter name
#' @param ifnotfound 
#'
#' @return The default parameter, possibly evaluated. 
#' @export
#' @keywords internal
#'
#' @examples
#' fun1 <- GetDefault(GaussSuppressionFromData, "candidates")
#' fun2 <- GetDefault(SuppressFewContributors, "primary")
#' fun3 <- GetDefault(SuppressDominantCells, "primary") 
GetDefault <- function(fun, parameter, ifnotfound = NULL) {
  param <- formals(fun)[[parameter]]
  if (is.null(param)) {
    param <- eval(formals(fun)$spec)[[parameter]]
    if (is.null(param)) {
      param <- ifnotfound
    }
  }
  if (is.name(param)) {
    param <- eval(param)
  }
  param
}


