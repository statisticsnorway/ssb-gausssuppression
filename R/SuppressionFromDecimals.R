#' Cell suppression from synthetic decimal numbers
#' 
#' Decimal numbers, as calculated by \code{\link{GaussSuppressDec}}, are used to decide suppression (whole numbers or not). 
#' Technically, the calculations are done via \code{\link{GaussSuppressionFromData}}, 
#' but without running \code{\link[SSBtools]{GaussSuppression}}. 
#' All suppressed cells are primary suppressed.  
#' 
#' Several decimal number variables reduce the probability of obtaining whole numbers by chance. 
#'
#' @param data    Input data as a data frame 
#' @param decVar One ore several (`nRep>1`) decimal number variables. 
#' @param freqVar A single variable holding counts (not needed)
#' @param numVar Other numerical variables to be aggregated
#' @param preAggregate  Parameter to \code{\link{GaussSuppressionFromData}}
#' @param digits Parameter to \code{\link[SSBtools]{RoundWhole}}. Values close to whole numbers will be rounded.
#' @param ...   Other parameters to \code{\link{GaussSuppressionFromData}}
#'
#' @return Aggregated data with suppression information
#' @export
#' 
#' @author Øyvind Langsrud 
#'
#' @examples
#' z2 <- SSBtoolsData("z2")
#' 
#' # Find suppression and decimal numbers with "fylke" in model 
#' a1 <- GaussSuppressDec(z2,
#'                        fun = SuppressSmallCounts,   
#'                        dimVar = c("region", "fylke", "hovedint"), 
#'                        freqVar = "ant", protectZeros = FALSE, maxN = 2, 
#'                        output = "inner")
#' 
#' # Add decimal numbers to data 
#' z2$freqDec <- a1$freqDec
#' 
#' # Find suppression with "kostragr" in model 
#' a2 <- SuppressionFromDecimals(z2, dimVar = c("region", "kostragr", "hovedint"), 
#'                               freqVar = "ant", decVar = "freqDec")
#' tail(a2)
#' 
#' b1 <- GaussSuppressDec(data = SSBtoolsData("magnitude1"), 
#'                        fun = SuppressDominantCells, 
#'                        numVar = "value", 
#'                        formula = ~sector2 * geo + sector4 * eu,
#'                        contributorVar = "company", k = c(80, 99))
#'  
#' b2 <- SuppressionFromDecimals(b1[b1$isInner, ], 
#'                               formula = ~(sector2 + sector4) * eu, 
#'                               numVar = "value", 
#'                               decVar = "freqDec")
#' FormulaSelection(b2, ~sector2 * eu)                                 
#'                               
SuppressionFromDecimals = function(data, decVar, freqVar = NULL, numVar = NULL,
                                   preAggregate = FALSE, digits = 9,
                                   ...){

   GaussSuppressionFromData(data, freqVar = freqVar, 
                            numVar = c(decVar, numVar), nDec = length(decVar),
                            preAggregate = preAggregate, 
                            candidates = integer(0),
                            primary = PrimaryDecimals,
                            singleton = NULL,
                            singletonMethod = "none",
                            digitsPrimary = digits,
                            ...) 
}
                                        
                   
PrimaryDecimals <- function(freq, num, nDec, digitsPrimary, ...) {
  
  if(nDec<ncol(num)){
    num = num[,seq_len(nDec), drop=FALSE]
  }
  
  numRW = RoundWhole(num, digits = digitsPrimary)
  
  primary = !(rowSums(numRW == round(num)) == nDec)
  
  
  if (!is.null(freq)) {
    if(any(freq[!primary] != numRW[!primary,1])){
      warning("Mismatch between aggregated frequencies and decimals aggregated to whole numbers") 
    }
  }
  
  primary
}

                     
