library(digest)

DominanceRule <- function(data, formula, numVar, n = 1:2, k = c(90,95)) {
  if (length(n) != length(k))
    stop("You must provide an equal number of inputs for n and k.")
  if (length(numVar) != 1)
    stop("You must provide exactly one numeric variable.")
  mm <- ModelMatrix(z, formula = formula, crossTable = TRUE)
  x <- mm$modelMatrix
  dvars <- row.names(attr(terms(as.formula(f)), "factors"))
  
  df_args <- c(data[dvars], sep="")
  groups <- do.call(paste, df_args)
  groups <- sapply(groups, digest)
  ncont <- Ncontributors(x, groups)
  cell_value <- t(x) %*% data[[numVar]]

  primary <- mapply(function (a,b) single_dominance_rule(x,data, numVar, cell_value, a,b),
                    n,k)
  # out <- mm$crossTable
  # out$val <- cell_value[,1]
  # for (i in 1:length(n)) {
  #   max_cont <- MaxContribution(x,data[[numVar]], n = n[[i]])
  #   max_cont[is.na(max_cont)] <- 0
  #   out[[paste0("max",n[[i]])]] <- rowSums(max_cont)
  #   out[[paste0("perc",k[[i]])]] <- rowSums(max_cont) / cell_value[,1]
  # }
  # # out$max_cont <- dominant_cont
  # # out$perc <- out$max_cont / out$val
  # out$primary <- as.data.frame(primary)
  # print(out)
  Reduce(`|`,as.data.frame(primary))
}

single_dominance_rule <- function(x,data, numVar, cell_value, n, k) {
  max_cont <- MaxContribution(x,data[[numVar]], n = n)
  max_cont[is.na(max_cont)] <- 0
  cell_value[,1] > 0 & rowSums(max_cont) > cell_value[,1]*k/100
}

