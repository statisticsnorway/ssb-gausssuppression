highsVal2 <- function(mat, dir, rhs, types) {
  
  n <- ncol(mat)
  
  lhs <- rhs
  rhs2 <- rhs
  rhs2[dir == ">="] <- Inf
  lhs[dir == "<="] <- -Inf
  
  if (length(types) == 1) {
    types <- rep(types, n)
  }
  
  model <- highs::highs_model(
    Q = NULL,
    L = numeric(n),
    lower = rep(0, n),
    upper = rep(Inf, n),
    A = mat,
    lhs = lhs,
    rhs = rhs2,
    types = types
  )
  
  solver <- highs::highs_solver(model)
  
  old_obj <- numeric(n)
  
  function(max, obj) {
    
    change <- which(obj != old_obj)
    
    if (length(change)) {
      solver$L(change, obj[change])
      old_obj[change] <<- obj[change]
    }
    
    solver$maximum(max)
    solver$solve()
    
    if (solver$status_message() == "Optimal") {
      solver$info()$objective_function_value
    } else {
      NA_real_
    }
  }
}