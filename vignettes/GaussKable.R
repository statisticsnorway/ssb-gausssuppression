library(formattable)
library(kableExtra)
library(SSBtools)
library(GaussSuppression)


# stats::reshape combined with sorting where order can be defined from a single input string   
sort_reshape <- function(data, s = NULL, direction = NULL, ...) {
  if (!is.null(s)) {
    char <- which(sapply(data, is.character))
    for (i in char) {
      u <- unique(data[[i]])
      u <- unique(c(s[s %in% u], u))
      data[[i]] <- factor(data[[i]], levels = u)
    }
  }
  data <- SortRows(data)
  if (!is.null(direction)) {
    data <- reshape(data, direction = direction, ...)
  }
  if (!is.null(s)) {
    char <- which(sapply(data, is.factor))
    for (i in char) {
      data[[i]] <- as.character(data[[i]])
    }
  }
  rownames(data) <- NULL
  data
}


SSBbla1 <- "#dcf1fc"
red <- "red"
lilla <- "#8F00EF"
SSBgronn1 <- "#e3f1e6"
SSBgronn2 <- "#90cc93"
SSBgronn3 <- "#1a9d49"
SSBgronn4 <- "#00824d"


Background <- function(primary, suppressed) {
  if (!suppressed) {
    return(NULL)
  }
  if (primary) {
    return(red)
  }
  return(lilla)
}

Color <- function(primary, suppressed) {
  if (!suppressed) {
    return(NULL)
  }
  return("white")
}



# HTML suppression table using packages kableExtra and formattable
# Inner, primary and suppressed needed in addition to suppressed
KableSuppressionTable <- function(a, printvar = "freq", caption = NULL, font_size = 14) {
  pvar <- grep(paste0(printvar, "."), names(a))
  inner <- grep(paste0("inner", "."), names(a))
  primary <- grep(paste0("primary", "."), names(a))
  suppressed <- grep(paste0("suppressed", "."), names(a))
  
  b <- a[pvar]
  names(b) <- gsub(paste0(printvar, "."), "", names(b))
  
  icol <- which(colSums(as.matrix(a[inner])) > 0)
  pcol <- which(!colSums(as.matrix(a[inner])) > 0)
  prow <- which(!rowSums(as.matrix(a[inner])) > 0)
  
  for (i in 1:nrow(b)) {
    for (j in 1:ncol(b)) {
      if (is.na(b[i, j])) {
        b[i, j] <- cell_spec("", "html")
      } else {
        b[i, j] <- cell_spec(b[i, j], "html", 
                             bold = !a[inner][i, j], 
                             underline = a[primary][i, j], 
                             background = Background(a[primary][i, j], a[suppressed][i, j]), 
                             color = Color(a[primary][i, j], a[suppressed][i, j]))
      }
    }
  }
  
  # d0 = a[!grepl('.', names(a), fixed = TRUE)]
  d0 <- a[!(seq_len(ncol(a)) %in% c(pvar, inner, primary, suppressed))]
  d <- cbind(d0, b)
  
  #kable(d, "html", caption = caption, escape = FALSE, align = "r") |>
  #  kable_styling(full_width = F, bootstrap_options = c("bordered"), font_size = font_size, position = "left") |>
  #  column_spec(ncol(d0) + pcol, background = SSBbla1, bold = T) |>
  #  row_spec(prow, background = SSBbla1, bold = T) |>
  #  row_spec(0, bold = T, background = SSBgronn2) |>
  #  column_spec(seq_len(ncol(d0)), background = SSBgronn1)
  
  
  # Extra code since vertical lines disappear in vignette.
  slg <- "1px solid LightGray"
  
  d <- kbl(d, "html", caption = caption, escape = FALSE, align = "r") |>
    kable_styling(bootstrap_options = c("bordered"), font_size = font_size, position = "left") 
  
  # include_thead = TRUE in column_spec not working, bug
  # see https://github.com/haozhu233/kableExtra/issues/534
  
  d <- row_spec(d, prow, background = SSBbla1, bold = T) |> row_spec(0, bold = T, background = SSBgronn2)
  
  for(i in ncol(d0) + icol)     
    d <- column_spec(d, i , border_right = slg)
  for(i in ncol(d0) + pcol)     
    d <- column_spec(d, i , background = SSBbla1, bold = T, border_right = slg)
  for(i in seq_len(ncol(d0)))
    d <- column_spec(d, i, background = SSBgronn1, border_left = slg, border_right = slg)
  d
}


G <- function(data, dimVar = NULL, freqVar = NULL,
              ...,
              fun = GaussSuppressionFromData,
              hierarchies = NULL, formula = NULL,   
              caption = NULL, 
              print_expr = freqVar, # expression as text possible 
              timevar = 1,         # name or number
              fullExtend = TRUE, 
              font_size = 14,   # Codes found in s will be ordered as in s
              s = c("young", "old", "Iceland", "Portugal", "Spain", "nonEU", "EU")) {
  
  out <- fun(data = data, 
             hierarchies = hierarchies, formula = formula, dimVar = dimVar, 
             freqVar = freqVar, printInc = FALSE, ...)
  dimVar <- NamesFromModelMatrixInput(data = data, 
                                      hierarchies = hierarchies, formula = formula, dimVar = dimVar, ...)
  dimVar <- dimVar[dimVar %in% names(out)]
  
  out$printtext <- with(out, eval(parse(text = print_expr)))
  n <- nrow(out)
  
  if (length(dimVar) == 1) {
    idvar <- dimVar
    timevar <- "dImVaR_2"
    out$dImVaR_2 <- " "
  } else {
    if (is.numeric(timevar)) {
      timevar <- dimVar[timevar]
    }
    idvar <- dimVar[!(dimVar %in% timevar)]
    
    if (fullExtend) {
      out <- Extend0(out, dimVar = c(timevar, idvar))
    } else {
      out <- Extend0(out, varGroups = list(out[timevar], out[idvar]))
    }
  }
  
  out <- out[c(timevar, idvar, "primary", "suppressed", "printtext")]
  
  out[SeqInc(n + 1, nrow(out)), "printtext"] <- NA
  out[SeqInc(n + 1, nrow(out)), c("primary", "suppressed")] <- FALSE
  out$inner <- rep(TRUE, nrow(out))
  for (v in dimVar) {
    out$inner <- out$inner & out[[v]] %in% data[[v]]
  }
  s0 <- character(0)
  for (v in dimVar) {
    #s0 <- c(s0, unique(c(data[[v]], out[[v]])))
    s0 <- c(s0, unique(c(
      sort_num_nchar(data[[v]]), 
      sort_num_nchar(out[[v]])
    )))
  }
  s0 <- s0[s0 != "Total"]
  s <- unique(c(s, s0, "Total"))
  a <- sort_reshape(out, direction = "wide", timevar = timevar, idvar = idvar, s = s)
  KableSuppressionTable(a, printvar = "printtext", caption = caption, font_size = font_size)
}



# Example: 
# x <- c("a","B2", 1, -3.14, 3.14, 0, 9, -88, 10, 7, "08", "0001", "1234", 123456, "000000", "000002", "000314")
# sort(x)
# stringr::str_sort(x, numeric = TRUE)
# sort_num_nchar(x)
sort_num_nchar <- function(x) {
  numx <- suppressWarnings(as.numeric(x))
  
  intx <- suppressWarnings(as.integer(x))
  intx[intx != numx] <- NA
  nonnegint <- !is.na(intx)
  nonnegint[nonnegint][numx[nonnegint] < 0] <- FALSE
  
  signx <- sign(numx)
  signx[is.na(signx)] <- 2
  signx[signx == 0] <- 1
  
  numnchar <- pmax(1, ceiling(log10(abs(numx)) + 1e-12))
  numnchar[is.na(numnchar)] <- 0
  numnchar[nonnegint] <- pmax(nchar(x[nonnegint]), numnchar[nonnegint])
  
  numx[is.na(numx)] <- 0
  
  df <- data.frame(signx, numnchar, numx, x)
  ord <- SortRows(df, index.return = TRUE)
  
  x[ord]
}


# Similar layout as KableSuppressionTable But just a table
KableTable <- function(data, nvar = 1, caption = NULL, font_size = 14, header = NULL) {
  
  icol <- SeqInc(nvar + 1, ncol(data))
  pvar <- names(data)[icol]
  
  # Extra code since vertical lines disappear in vignette.
  slg <- "1px solid LightGray"
  
  d <- kbl(data, "html", caption = caption, escape = FALSE, align = "r") |>
    kable_styling(full_width = F, bootstrap_options = c("bordered"), font_size = font_size, position = "left")
  
  d <- row_spec(d, 0, bold = T, background = SSBgronn2)
  
  for (i in icol) d <- column_spec(d, i, border_right = slg)
  
  for (i in seq_len(nvar)) d <- column_spec(d, i, background = SSBgronn1, border_left = slg, border_right = slg)
  
  if (!is.null(header)) {
    header_above <- c(nvar, ncol(data) - nvar)
    if (length(header) == 1) {
      header <- c(" ", header)
    }
    names(header_above) <- header
    d <- add_header_above(d, header_above, background = SSBgronn4, 
                          color = "white")
    
  }
  
  d
}
