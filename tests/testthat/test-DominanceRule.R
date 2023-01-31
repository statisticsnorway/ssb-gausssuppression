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
d <-
  data.frame(
    v1 = v1,
    num = as.numeric(num),
    sw1 = 1,
    sw2 = sw2,
    sw3 = sw3
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
    )
  expect_true(all.equal(as.logical(p1), p2$primary, p3$primary))
})

test_that("Default weighted dominance", {
  p <-
    DominanceRule(
      d,
      x = mm$modelMatrix,
      crossTable = mm$crossTable,
      numVar = "num",
      n = 2,
      k = 90,
      sWeightVar = "sw2",
    )
  expect_equal(p$primary, c(T, rep(F, 6)))
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
