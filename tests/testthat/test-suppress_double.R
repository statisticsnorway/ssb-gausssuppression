test_that("suppress_double", {
  
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
                  remove_diff_vars = FALSE)

  
  
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
    )
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
  
})
