#' Default singleton function
#'
#' Function for \code{\link{GaussSuppressionFromData}}
#' 
#' This function marks input cells as singletons according to the input frequencies (`freqVar`). 
#' Zero frequencies are set to singletons when `protectZeros` or `secondaryZeros` is `TRUE`. 
#' Otherwise, ones are set to singletons.
#' Empty `freqVar` is treated as all frequencies being ones. 
#'
#' @param data  Input data, possibly pre-aggregated within `GaussSuppressionFromData`  
#' @param freqVar A single variable holding counts (input to `GaussSuppressionFromData`)
#' @param protectZeros Suppression parameter (see `GaussSuppressionFromData`)
#' @param secondaryZeros Suppression parameter (see `GaussSuppressionFromData`)
#' @param ... Unused parameters 
#'
#' @return singleton, \code{\link[SSBtools]{GaussSuppression}} input 
#' @export
SingletonDefault <- function(data, freqVar, protectZeros, secondaryZeros, ...) {
  
  if(is.null(protectZeros))   stop("A non-NULL value of protectZeros is required.")
  if(is.null(secondaryZeros)) stop("A non-NULL value of secondaryZeros is required.")
  
  if (protectZeros | secondaryZeros){ 
    if (!length(freqVar)) {
      return(rep(FALSE, nrow(data)))
    }
    return(data[[freqVar]] == 0)
  }
  if (!length(freqVar)) {
    return(rep(TRUE, nrow(data)))
  }
  data[[freqVar]] == 1
}




#' Unique contributor singleton function
#' 
#' Function for \code{\link{GaussSuppressionFromData}}
#' 
#' This function marks input cells as singletons according to ones in 
#' `data[[nUniqueVar]]`, if available, and otherwise according to `data[[freqVar]]`.
#' The output vector can be logical or integer. When, integer, singletons are given as positive values. 
#' Their unique values represent the unique values/combinations of `data[[charVar]]`.
#'
#' @inheritParams SingletonDefault
#' @inheritParams DominanceRule
#' @param nUniqueVar  A single variable holding the number of unique contributors.
#' @param charVar Variable with contributor codes. 
#' @param removeCodes Vector, list or data frame of codes considered non-singletons.
#'                Single element lists and single column data frames behave just like vectors.
#'                In other cases, `charVar`-names must be used.
#'                With empty `charVar` a vector of row indices is assumed and conversion to integer is performed.
#'                See examples.
#' @param integerSingleton Integer output when `TRUE`. See details.
#' @param primary Vector (integer or logical) specifying primary suppressed cells.
#'                It will be ensured that any non-suppressed inner cell is not considered a singleton.
#' @param whenPrimaryMatters Function to be called when `primary` caused non-singleton. Supply `NULL` to do nothing.
#' @param whenNoVar When `TRUE`, and without `nUniqueVar` and `freqVar` in input, 
#'                all cells will be marked as singletons.     
#' @param specialMultiple When `TRUE`, and when `integerSingleton &` `length(charVar) > 1` `& length(nUniqueVar)`,
#'                a special method is used. 
#'                By re-coding to single `charVar` and by re-calculating `nUnique`.
#'                To be unique (`nUnique=1`), uniqueness is only required for a single `charvar`.
#'                Otherwise, the `charvar` combination must be unique.   
#'                                     
#' @param ... Unused parameters
#'
#' @return logical or integer vector
#' @export
#' @importFrom SSBtools RowGroups
#'
#' @examples
#' S <- function(data, ...) {
#'   cbind(data, singleton = SingletonUniqueContributor(data, ...))
#' }
#' d2 <- SSBtoolsData("d2")
#' d <- d2[d2$freq < 5, ]
#' d$nUnique <- round((5 - d$freq)/3)
#' d$freq <- round(d$freq/2)
#' d[7:8, 2:4] <- NA
#' rownames(d) <- NULL
#' 
#' S(d, freqVar = "freq", integerSingleton = FALSE)
#' S(d, freqVar = "freq", nUniqueVar = "nUnique", integerSingleton = TRUE, charVar = "main_income")
#' S(d, nUniqueVar = "nUnique", integerSingleton = TRUE, charVar = c("main_income", "k_group"))
#' S(d, freqVar = "freq", nUniqueVar = "nUnique", integerSingleton = FALSE, 
#'   charVar = "main_income", removeCodes = "other")
#' S(d, nUniqueVar = "nUnique", integerSingleton = FALSE, charVar = c("main_income", "k_group"), 
#'   removeCodes = c("other", "400"))
#' S(d, nUniqueVar = "nUnique", integerSingleton = FALSE, charVar = c("main_income", "k_group"), 
#'   removeCodes = data.frame(anyname = c("other", "400")))
#' S(d, nUniqueVar = "nUnique", integerSingleton = FALSE, charVar = c("main_income", "k_group"), 
#'   removeCodes = list(main_income = c("other", "pensions"), k_group = "300"))
#' S(d, nUniqueVar = "nUnique", integerSingleton = FALSE, charVar = c("main_income", "k_group"), 
#'   removeCodes = data.frame(main_income = "other", k_group = "400"))
#' S(d, nUniqueVar = "nUnique", integerSingleton = FALSE, removeCodes = 1:5)
#' 
#' x <- SSBtools::ModelMatrix(d, hierarchies = list(region = "Total"))
#' which(colSums(x) == 1)
#' which(rowSums(x[, colSums(x) == 1]) > 0)
#' # columns 2, 3, 4, 5, 7 correspond to inner cells: rows 3, 4, 5, 6, 8 
#' # with 2:4 not primary rows 3:5 are forced non-singleton
#' S(d, freqVar = "freq", nUniqueVar = "nUnique", integerSingleton = FALSE, x = x, primary = 5:8)
#' 
SingletonUniqueContributor <- function(data, 
                                       freqVar = NULL, 
                                       nUniqueVar=NULL, 
                                       charVar=NULL, 
                                       removeCodes = character(0), 
                                       integerSingleton = length(charVar) > 0,
                                       x,
                                       primary = integer(0),
                                       whenPrimaryMatters = warning,
                                       whenNoVar = TRUE,
                                       specialMultiple = TRUE,
                                       ...) {
  
  if (length(nUniqueVar)) {
    if (is.null(data[[nUniqueVar]])) {
      nUniqueVar <- NULL
    }
  }
  
  if (specialMultiple & integerSingleton & length(charVar) > 1L & length(nUniqueVar)) {
    ssmd <- SingletonSpecialMultipleData(data, charVar, nUniqueVar, removeCodes)
    data <- ssmd$data
    charVar <- names(data)[1]
    nUniqueVar <- names(data)[2]
    removeCodes <- ssmd$removeCodes
  }
  
  if (length(nUniqueVar)) {
    singleton <- data[[nUniqueVar]] == 1
  } else {
    if (length(freqVar)) {
      singleton <- data[[freqVar]] == 1
    } else {
      singleton <- rep(whenNoVar, nrow(data))
    }
  }
  
  if (length(removeCodes)) {
    if (length(charVar)) {
      if (is.list(removeCodes) & length(removeCodes) == 1) {
        removeCodes <- unlist(removeCodes)
      }
      if (is.list(removeCodes)) {
        if (is.data.frame(removeCodes)) {
          ma <- Match(data[charVar], removeCodes)
          singleton[!is.na(ma)] <- FALSE
        } else {
          for (i in seq_along(charVar)) {
            singleton[data[[charVar[i]]] %in% removeCodes[[charVar[i]]]] <- FALSE  # Ordinary when single charVar 
          }
        }
      } else {
        for (i in seq_along(charVar)) {
          singleton[data[[charVar[i]]] %in% removeCodes] <- FALSE  # Ordinary when single charVar 
        }
      }
    } else {
      if (!is.vector(removeCodes)) {
        stop("removeCodes must be vector when empty charVar")
      }
      singleton[as.integer(removeCodes)] <- FALSE
    }
  }
  
  if (is.logical(primary)) {
    primary <- which(primary)
  } else {
    primary <- unique(primary)
  }
  
  if (length(primary)) {
    inner <- rowSums(x[, colSums(x) == 1, drop = FALSE]) > 0
    innerprimary <- rowSums(x[, primary[colSums(x[, primary, drop = FALSE]) == 1], drop = FALSE]) > 0
    if (any(singleton[!innerprimary & inner])) {
      if (!is.null(whenPrimaryMatters)) {
        whenPrimaryMatters("primary caused non-singleton")
      }
      singleton[!innerprimary & inner] <- FALSE
    }
  }
  if (!integerSingleton) {
    return(singleton)
  }
  if (length(charVar)) {
    singleton_integer <- RowGroups(data[charVar])
  } else {
    singleton_integer <- seq_len(nrow(data))
  }
  singleton_integer[!singleton] <- 0L
  singleton_integer
}



#' @rdname SingletonUniqueContributor
#' @note   `SingletonUniqueContributor0` is a special version that produces singleton as 
#'         a two-element list. 
#'         See \code{\link[SSBtools]{GaussSuppression}} and \code{\link{SuppressDominantCells}}.
#'         
#' @export
SingletonUniqueContributor0 <- function(data, numVar, dominanceVar = NULL, ...) {
  if (is.null(dominanceVar)) {
    dominanceVar <- numVar[1]
  }
  list(freq = data[[dominanceVar]] == 0, num = SingletonUniqueContributor(data = data, ...))
}


SingletonSpecialMultipleData <- function(data, charVar, nUniqueVar, removeCodes) {
  charN <- matrix(0L, nrow(data), length(charVar))
  for (i in seq_along(charVar)) {
    tabi <- table(data[[charVar[i]]])
    charN[, i] <- tabi[data[[charVar[i]]]]
    charN[is.na(charN)] <- 0L
  }
  charN <- 2L * charN  # trick so that 1L can be used for removeCodes
  if (length(removeCodes)) {
    if (!is.list(removeCodes)) {
      for (i in seq_along(charVar)) {
        ma <- match(data[[charVar[i]]], removeCodes)
        charN[!is.na(ma), i] <- 1L
      }
    } else {
      if (is.data.frame(removeCodes)) {
        ma <- Match(data[charVar], removeCodes)
        charN[!is.na(ma), ] <- 1L
      } else {
        for (i in seq_along(charVar)) {
          ma <- match(data[[charVar[i]]], removeCodes[[charVar[i]]])
          charN[!is.na(ma), i] <- 1L
        }
      }
    }
    
  }
  charNA <- rowSums(charN) == 0
  charNok <- charN[!charNA, , drop = FALSE]
  wmax <- apply(charNok, 1, which.max)
  char <- rep(NA_character_, nrow(data))
  wmaxInd <- cbind(seq_len(length(wmax)), wmax)
  char[!charNA] <- data[!charNA, charVar, drop = FALSE][wmaxInd]
  charNmax <- charNok[wmaxInd]
  char[!charNA] <- paste(charVar[wmax], char[!charNA], sep = "_")
  removeCodes <- unique(char[!charNA][charNmax == 1L])
  nUnique <- rep(Inf, nrow(data))
  nUnique[!charNA] <- 1L
  list(data = data.frame(charVar = char, nUniqueVar = nUnique), removeCodes = removeCodes)
}









