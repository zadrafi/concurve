context("Class Type")

# Test class structure

# Testing curve_mean() ----------------------------------------------------
test_that("curve_mean", {
  GroupA <- rnorm(500)
  GroupB <- rnorm(500)

  RandomData <- data.frame(GroupA, GroupB)

  intervalsdf <- curve_mean(GroupA, GroupB,
    data = RandomData, method = "default"
  )

  expect_s3_class(intervalsdf, "concurve")
  expect_s3_class(intervalsdf[[1]], "concurve")
  expect_s3_class(intervalsdf[[2]], "concurve")
  expect_s3_class(intervalsdf[[3]], "concurve")
})

# Testing curve_gen() ----------------------------------------------------
test_that("curve_gen", {
  GroupA2 <- rnorm(500)
  GroupB2 <- rnorm(500)

  RandomData2 <- data.frame(GroupA2, GroupB2)

  model <- lm(GroupA2 ~ GroupB2, data = RandomData2)

  randomframe <- curve_gen(model, "GroupB2")

  expect_s3_class(randomframe, "concurve")
  expect_s3_class(randomframe[[1]], "concurve")
  expect_s3_class(randomframe[[2]], "concurve")
  expect_s3_class(randomframe[[3]], "concurve")
})

# Testing curve_rev() ----------------------------------------------------
test_that("curve_rev", {
  lik1 <- curve_rev(point = 1.7, LL = 1.1, UL = 2.6, type = "l", measure = "ratio", steps = 10000)
  expect_s3_class(lik1, "concurve")
  expect_s3_class(lik1[[1]], "concurve")
  expect_s3_class(lik1[[2]], "concurve")
})

# Testing curve_boot() ----------------------------------------------------
test_that("curve_boot", {
  data(diabetes, package = "bcaboot")
  Xy <- cbind(diabetes$x, diabetes$y)
  rfun <- function(Xy) {
    y <- Xy[, 11]
    X <- Xy[, 1:10]
    return(summary(lm(y ~ X))$adj.r.squared)
  }

  x <- curve_boot(data = Xy, func = rfun, method = "bca", replicates = 200, steps = 1000)
  expect_s3_class(x, "concurve")
  expect_s3_class(x[[1]], "concurve")
  expect_s3_class(x[[2]], "concurve")
  expect_s3_class(x[[3]], "concurve")
  expect_s3_class(x[[4]], "concurve")
  expect_s3_class(x[[5]], "concurve")
  expect_s3_class(x[[6]], "concurve")
})
