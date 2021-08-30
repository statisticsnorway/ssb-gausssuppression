


#' GaussSuppressDec
#'
#' @param data Input daata as a data frame 
#' @param ... Further parameters to \code{\link{GaussSuppressionFromData}}
#' @param output output
#' @param digits digits 
#' @param nRep nRep
#' @param rmse rmse
#'
#' @return
#' 
#' @importFrom RegSDC SuppressDec
#' @export
#'
#' @examples
#' z1 <- SSBtoolsData("z1")
#' GaussSuppressDec(z1, 1:2, 3, nRep =3)$b
GaussSuppressDec = function(data, ..., output = "both", digits = 9, nRep = 1, rmse = pi/3){
  
  
  a <- GaussSuppressionFromData(data, ..., xReturn = TRUE, innerReturn = TRUE)
  
  z <- as.matrix(a$publish["freq"])
  y <- as.matrix(a$inner[attr(a$inner, "freqVar")])
  
  b <- SuppressDec(a$x, z=z, y=y, suppressed = a$publish$suppressed, digits = digits, nRep = nRep, rmse = rmse)
  
  list(a=a,b=b)
}