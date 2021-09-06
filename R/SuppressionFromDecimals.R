SuppressionFromDecimals = function(data, freqVar, decVar, numVar = NULL,  ...){

   GaussSuppressionFromData(data, freqVar = freqVar, 
                            numVar = c(decVar, numVar), nDec = length(decVar),
                            primary = PrimaryDecimals,
                            singletonMethod = function(...){integer(0)},
                            ...) 
}
                                        
                   
PrimaryDecimals <- function(freq, num, nDec, ...) {
  
  if(nDec<ncol(num)){
    num = num[,seq_len(nDec), drop=FALSE]
  }
  
  numRW = RoundWhole(num)
  
  primary = !(rowSums(numRW == round(num)) == nDec)
  
  if(any(freq[!primary] != numRW[!primary,1])){
    warning("Mismatch between aggregated frequencies and decimals aggregated to whole numbers") 
  }
  
  primary
}

                     