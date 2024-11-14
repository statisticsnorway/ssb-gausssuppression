num <- c(100,
         90, 10,
         80, 20,
         70, 30,
         80, 10, 10,
         70, 10, 10, 10,
         60, 20, 10, 10)
v1 <- c("v1",
        rep(c("v2", "v3", "v4"), each = 2),
        rep("v5", 3),
        rep(c("v6", "v7"), each = 4))
sw2 <- c(1, 2, 1, 2, 1, 2, 1, 2, 1, 1, 2, 1, 1, 1, 2, 1, 1, 1)
sw3 <- c(1, 0.9, 1, 2, 1, 2, 1, 2, 1, 1, 2, 1, 1, 1, 2, 1, 1, 1)
char0 <- paste0("char", 1:18)
char <- c("A", "C", "B", "A", "C", "C", "C", "C", "B", "A", "A", "B", 
          "A", "A", "C", "A", "B", "A")
d <-
  data.frame(
    v1 = v1,
    num = as.numeric(num),
    sw1 = 1,
    sw2 = sw2,
    sw3 = sw3,
    char0 = char0,
    char = char
  )

mm <-
  SSBtools::ModelMatrix(d, formula = ~ v1 - 1, crossTable = TRUE)
test_that("Unweighted dominance", {
  p1 <-
    DominanceRule(
      d,
      x = mm$modelMatrix,
      crossTable = mm$crossTable,
      numVar = "num",
      n = 2,
      k = 90
    )
  p2 <-
    DominanceRule(
      d,
      x = mm$modelMatrix,
      crossTable = mm$crossTable,
      numVar = "num",
      n = 2,
      k = 90,
      sWeightVar = "sw1",
      domWeightMethod = "tauargus"
    )
  p3 <-
    DominanceRule(
      d,
      x = mm$modelMatrix,
      crossTable = mm$crossTable,
      numVar = "num",
      n = 2,
      k = 90,
      sWeightVar = "sw1",
      allDominance = TRUE,
      outputWeightedNum = TRUE
    )
  expect_true(all.equal(as.logical(p1), p2$primary, p3$primary))
  
  # as p1 but with allDominance,  outputWeightedNum
  p1_ <-
    DominanceRule(
      d,
      x = mm$modelMatrix,
      crossTable = mm$crossTable,
      numVar = "num",
      n = 2,
      k = 90,
      allDominance = TRUE,
      outputWeightedNum = TRUE
    )
  
  p4 <-
    DominanceRule(
      d,
      x = mm$modelMatrix,
      crossTable = mm$crossTable,
      numVar = "num",
      n = 2,
      k = 90,
      allDominance = TRUE,
      outputWeightedNum = TRUE,
      charVar = "char0"
    )
  
  p1_$numExtra <-  p1_$numExtra[1:3]
  p4$numExtra <-  p4$numExtra[1:3]
  
  expect_equal(p1_, p4)
    
  p5 <-
    DominanceRule(
      d,
      x = mm$modelMatrix,
      crossTable = mm$crossTable,
      numVar = "num",
      n = 2,
      k = 90,
      sWeightVar = "sw1",
      allDominance = TRUE,
      outputWeightedNum = TRUE,
      charVar = "char0"
    )
  
  p3$numExtra <- p3$numExtra[1:3]
  p5$numExtra <-  p5$numExtra[1:3]
  
  expect_equal(p3, p5)
  
  
  p6 <-
    DominanceRule(
      d,
      x = mm$modelMatrix,
      crossTable = mm$crossTable,
      numVar = "num",
      n = 2,
      k = 98,
      charVar = "char",
      allDominance = TRUE,
      outputWeightedNum = TRUE
    )
  expect_equal(p6$numExtra[["primary.2:98"]], c(1, 1, 1, 1, 0.9, 1, 0.9))
})



test_that("Default weighted dominance", {
  
  options(GaussSuppression.test_maxContribution = TRUE)
  on.exit(options(GaussSuppression.test_maxContribution = NULL), add = TRUE) # option removed
  
  p <-
    DominanceRule(
      d,
      x = mm$modelMatrix,
      crossTable = mm$crossTable,
      numVar = "num",
      n = 1:2,
      k = c(70, 90),
      sWeightVar = "sw2",
      allDominance = TRUE
    )
  expect_equal(p$primary, c(T, rep(F, 6)))
  expect_equal(p$numExtra[[2]], c(100, 190, 180, 170, 180, 170, 160))
  
  p_char0 <-
    DominanceRule(
      d,
      x = mm$modelMatrix,
      crossTable = mm$crossTable,
      numVar = "num",
      n = 1:2,
      k = c(70, 90),
      sWeightVar = "sw2",
      allDominance = TRUE,
      charVar = "char0"
    )
  
  p$numExtra <- p$numExtra[1:4]
  p_char0$numExtra <- p_char0$numExtra[1:4]
  
  expect_equal(p, p_char0)
  
  
  p_char <-
    DominanceRule(
      d,
      x = mm$modelMatrix,
      crossTable = mm$crossTable,
      numVar = "num",
      n = 1:2,
      k = c(70, 90),
      sWeightVar = "sw2",
      allDominance = TRUE,
      charVar = "char"
    )
  
  p_char_no_Weight <-
    DominanceRule(
      d,
      x = mm$modelMatrix,
      crossTable = mm$crossTable,
      numVar = "num",
      n = 1:2,
      k = c(70, 90),
      allDominance = TRUE,
      charVar = "char",
      outputWeightedNum = TRUE
    )
  
  f34 <- p_char_no_Weight$numExtra$weighted.num  / p_char$numExtra$weighted.num
  c34 <- p_char_no_Weight$numExtra[3:4] * f34
  expect_equal( p_char$numExtra[3:4], c34) 
    
})

test_that("tauargus dominance", {
  p <-
    DominanceRule(
      d,
      x = mm$modelMatrix,
      crossTable = mm$crossTable,
      numVar = "num",
      n = 2,
      k = 90,
      sWeightVar = "sw2",
      domWeightMethod = "tauargus"
    )
  expect_equal(p$primary, c(T, T, F, F, F, F, F))
  expect_warning(
    DominanceRule(
      d,
      x = mm$modelMatrix,
      crossTable = mm$crossTable,
      numVar = "num",
      n = 2,
      k = 90,
      sWeightVar = "sw3",
      domWeightMethod = "tauargus"
    )
  )
  expect_error(
    DominanceRule(
      d,
      x = mm$modelMatrix,
      crossTable = mm$crossTable,
      numVar = "num",
      n = 2,
      k = 90,
      charVar = "v1",
      sWeightVar = "sw1",
      domWeightMethod = "tauargus"
    )
  )
})
