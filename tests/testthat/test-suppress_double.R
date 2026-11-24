test_that("suppress_double", {
  
  printInc <- FALSE
  
  # A test using modified code from the suppress_double() examples
  
  # First, generate example data to be used below 
  a <- SSBtoolsData("magnitude1")
  k <- c("50.1", "50.2", "70.1", "70.2")
  b <- data.frame(nace_3_A = c(k, k), nace_3_B = c(k, k))
  b[5:8, 2] <- b[c(6, 5, 8, 5), 2]
  b$nace_2_A <- substr(b[[1]], 1, 2)
  b$nace_2_B <- substr(b[[2]], 1, 2)
  set.seed(559)
  d <- cbind(a, b[sample.int(8, 40, replace = TRUE, prob = 8:1), ])
  d$id <- paste0("id_", 1:40)
  rownames(d) <- NULL
  
  
  # In this first example, no primary suppressed cells are visible in the output.
  # Only difference cells are primary suppressed, but these are not shown
  # in the default output (remove_diff_vars = TRUE).
  out1 <- suppress_double(d, numVar = "value", formula = ~nace_2 * eu + nace_3 + geo,
                  contributorVar = "id", pPercent = 5,
                  double_vars = list(nace_2 = c("nace_2_A", "nace_2_B"),
                                     nace_3 = c("nace_3_A", "nace_3_B")),
                  double_priority = 1,
                  use_diff_groups = TRUE,
                  remove_diff_vars = FALSE,
                  printInc = printInc)

  
  
  collapse_vars1 <- list(
    nace = c("nace_3_A", "nace_3_B"),
    region = "geo"
  )
  
  collapse_vars2 <- list(
    nace = c("nace_2", "nace_3"),
    region = c("geo", "eu")
  )
  
  out1 <- SSBtools::total_collapse(out1, collapse_vars1)
    
  
  # Similar to the first example, but using tables_by_formulas()
  out2 <- tables_by_formulas(
    d,
    table_fun = suppress_double,
    numVar = "value",
    table_formulas = list(
      table_1 = ~nace_2 * eu,
      table_2 = ~nace_3 + geo
    ),
    contributorVar = "id",
    pPercent = 5,
    double_vars = list(
      nace_2 = c("nace_2_A", "nace_2_B"),
      nace_3 = c("nace_3_A", "nace_3_B")
    ),
    double_priority = 1,
    use_diff_groups = TRUE,
    collapse_vars = list(
      nace = c("nace_2", "nace_3"),
      region = c("geo", "eu")
    ),
    printInc = printInc
  )
  
  nam <- intersect(names(out1), names(out2))
  
  out1 <- out1[nam]
  out2 <- out2[nam]
  ma <- SSBtools::Match(out2, out1)
  out1 <- out1[ma, ]
  
  rownames(out1) <- NULL
  rownames(out2) <- NULL
  
  expect_equal(out1, out2)
  
  expect_equal(sum(out1$suppressed), 6)
  

  # Examples demonstrating the suppressed_data parameter
  
  # Suppression based on the first classification only
  out3 <- SuppressDominantCells(
    d, numVar = "value", formula = ~nace_2_A * geo + nace_3_A * eu,
    contributorVar = "company", pPercent = 1,
    printInc = printInc
  )
  
  # Suppression of both classifications, taking previous suppression into account
  out4 <- suppress_double(
    d, numVar = "value", formula = ~nace_2 * geo + nace_3 * eu,
    contributorVar = "company", pPercent = 1,
    double_vars = list(nace_2 = c("nace_2_A", "nace_2_B"),
                       nace_3 = c("nace_3_A", "nace_3_B")),
    use_diff_groups = TRUE,
    suppressed_data = out3,
    output = "double",
    printInc = printInc
  )
  
  out3 <- out3[names(out3)]
  out5 <- out4$a[names(out3)]
  ma <- SSBtools::Match(out3[1:2], out5[1:2])
  out5 <- out5[ma, ]
  rownames(out3) <- NULL
  rownames(out5) <- NULL
  expect_equal(out3, out5)
  
  expect_equal(sum(out4$b$suppressed), 14)
  
  
})
