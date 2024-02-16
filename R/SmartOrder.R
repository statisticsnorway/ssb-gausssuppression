
# Each value two times. 
# The last values coded negative and these are in order.
SmartOrder <- function(n) {
  a <- SmartOrder0(n)
  b <- integer(0)
  k <- 1L
  for (i in seq_len(n)) {
    b <- c(b, a[i])
    if (a[i] == k) {
      b <- c(b, -k)
      k <- k + 1L
      a_ <- a[seq_len(i - 1)]
      ok <- TRUE
      while (ok) {
        if ((k) %in% a_) {
          b <- c(b, -k)
          k <- k + 1L
        } else {
          ok <- FALSE
        }
      }
    }
  }
  b
}


SmartOrder0 <- function(n) {
  if (n <= 2L) {
    return(seq_len(n))
  }
  half <- n%/%2
  c(half, SmartOrder0(half - 1), n, half + SmartOrder0(n - half - 1))
}