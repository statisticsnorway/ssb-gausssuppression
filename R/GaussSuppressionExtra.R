
# GaussSuppression-wrapper with extra primary x-matrix as possibility 
GaussSuppressionExtra <- function(x, primary = NULL, forced = NULL, hidden = NULL, xExtraPrimary = NULL, ...) {
  if (!is.null(xExtraPrimary)) {
    n <- ncol(xExtraPrimary)
    if (is.logical(primary)) {
      if (length(primary) == 1) {  # Possible output is just FALSE (when only matrix output)
        primary <- rep(primary, ncol(x))
      }
      primary <- c(primary, rep(TRUE, n))
    } else {
      stop("logical primary expected")  # logical is output from Primary
    }
    if (is.logical(forced)) {
      forced <- c(forced, rep(FALSE, n))
    }
    if (is.logical(hidden)) {
      forced <- c(hidden, rep(FALSE, n))
    }
    x <- cbind(x, xExtraPrimary)
  }
  GaussSuppression(x = x, primary = primary, forced = forced, hidden = hidden, ...)
}
