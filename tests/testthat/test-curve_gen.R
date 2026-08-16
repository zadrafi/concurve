# Tests for curve_gen - the primary user-facing function
# Covers linear models, GLMs, robust models, and edge cases

test_that("curve_gen works with lm models", {
  model <- lm(mpg ~ wt + hp, data = mtcars)

  # Test with default parameters (table=TRUE by default)
  result <- suppressMessages(
    curve_gen(model, "wt", method = "lm", steps = 100, table = TRUE)
  )

  expect_type(result, "list")
  expect_length(result, 3) # With table=TRUE, returns 3 elements
  expect_s3_class(result[[1]], "concurve")
  expect_s3_class(result[[1]], "data.frame")

  # Check structure of output dataframe
  expect_named(
    result[[1]],
    c(
      "lower.limit", "upper.limit", "intrvl.width", "intrvl.level",
      "cdf", "pvalue", "svalue"
    )
  )
  expect_equal(nrow(result[[1]]), 98) # steps - 2 (first and last excluded)
})

test_that("curve_gen produces monotonic interval widths", {
  model <- lm(mpg ~ wt + hp, data = mtcars)
  result <- suppressMessages(
    curve_gen(model, "wt", method = "lm", steps = 50, table = FALSE)
  )

  df <- result[[1]]

  # Interval widths should generally increase with confidence level
  # (allowing small numerical deviations)
  widths <- df$intrvl.width
  is_increasing <- all(diff(widths) >= -1e-6)
  expect_true(is_increasing, info = "Interval widths should be monotonically increasing")
})

test_that("curve_gen handles GLM models", {
  model <- glm(am ~ wt + hp, data = mtcars, family = binomial)

  # Suppress warnings about fitted probabilities (expected in profile likelihood)
  result <- suppressWarnings(suppressMessages(
    curve_gen(model, "wt", method = "glm", steps = 50, table = FALSE)
  ))

  expect_type(result, "list")
  expect_s3_class(result[[1]], "concurve")
  expect_true(all(is.finite(result[[1]]$lower.limit)))
  expect_true(all(is.finite(result[[1]]$upper.limit)))
})

test_that("curve_gen handles RLM models when MASS is available", {
  skip_if_not_installed("MASS")
  library(MASS)

  model <- rlm(mpg ~ wt + hp, data = mtcars)

  result <- suppressMessages(
    curve_gen(model, "wt", method = "rlm", steps = 50, table = FALSE)
  )

  expect_type(result, "list")
  expect_s3_class(result[[1]], "concurve")
})

test_that("curve_gen with table=TRUE returns 3 elements", {
  model <- lm(mpg ~ wt + hp, data = mtcars)

  result <- suppressMessages(
    curve_gen(model, "wt", method = "lm", steps = 50, table = TRUE)
  )

  expect_length(result, 3)
  expect_named(
    result,
    c("Intervals Dataframe", "Intervals Density", "Intervals Table")
  )
  expect_s3_class(result[[3]], "data.frame")
})

test_that("curve_gen with table=FALSE returns 2 elements", {
  model <- lm(mpg ~ wt + hp, data = mtcars)

  result <- suppressMessages(
    curve_gen(model, "wt", method = "lm", steps = 50, table = FALSE)
  )

  expect_length(result, 2)
})

test_that("curve_gen respects log parameter for exponentiation", {
  model <- glm(am ~ wt + hp, data = mtcars, family = binomial)

  # Suppress both messages and warnings from GLM fitting
  result_nolog <- suppressWarnings(suppressMessages(
    curve_gen(model, "wt", method = "glm", steps = 30, log = FALSE, table = FALSE)
  ))

  result_log <- suppressWarnings(suppressMessages(
    curve_gen(model, "wt", method = "glm", steps = 30, log = TRUE, table = FALSE)
  ))

  # With log=TRUE, limits should all be positive (exponentiated)
  expect_true(all(result_log[[1]]$lower.limit > 0))
  expect_true(all(result_log[[1]]$upper.limit > 0))

  # They should not be equal
  expect_false(
    identical(result_nolog[[1]]$lower.limit, result_log[[1]]$lower.limit)
  )
})

test_that("curve_gen validates input parameters", {
  model <- lm(mpg ~ wt + hp, data = mtcars)

  # Invalid method
  expect_error(
    curve_gen(model, "wt", method = 123),
    "must be a character vector"
  )

  # Invalid steps
  expect_error(
    curve_gen(model, "wt", steps = "not_numeric"),
    "must be a numeric vector"
  )
})

test_that("curve_gen handles single-predictor models", {
  model <- lm(mpg ~ wt, data = mtcars)

  result <- suppressMessages(
    curve_gen(model, "wt", method = "lm", steps = 50, table = FALSE)
  )

  expect_s3_class(result[[1]], "concurve")
  expect_equal(nrow(result[[1]]), 48) # steps - 2
})

test_that("curve_gen with Bonferroni adjustment", {
  model <- lm(mpg ~ wt + hp + drat, data = mtcars)

  result <- suppressMessages(
    curve_gen(model, "wt",
      method = "lm", steps = 50,
      penalty = "bonferroni", m = 3, table = FALSE
    )
  )

  expect_type(result, "list")
  expect_s3_class(result[[1]], "concurve")

  # With Bonferroni adjustment, confidence intervals should be wider
  # (higher confidence levels adjusted upward)
  expect_true(nrow(result[[1]]) > 0)
})

test_that("curve_gen output matches confint.default for key values", {
  model <- lm(mpg ~ wt + hp, data = mtcars)

  result <- suppressMessages(
    curve_gen(model, "wt", method = "lm", steps = 1000, table = FALSE)
  )

  df <- result[[1]]

  # Find 95% CI from curve_gen
  idx_95 <- which.min(abs(df$intrvl.level - 0.95))
  curve_gen_95 <- unname(c(df$lower.limit[idx_95], df$upper.limit[idx_95]))

  # Compare to standard confint at 0.95
  model_ci <- unname(confint.default(model, level = 0.95)["wt", ])

  # Should be very close (within 1% relative error)
  expect_equal(curve_gen_95, model_ci, tolerance = 0.01)
})

test_that("curve_gen density output is reasonable", {
  model <- lm(mpg ~ wt + hp, data = mtcars)

  result <- suppressMessages(
    curve_gen(model, "wt", method = "lm", steps = 100, table = FALSE)
  )

  dens <- result[[2]]

  expect_s3_class(dens, "data.frame")
  expect_true(nrow(dens) > 0)
  expect_true(all(dens$y > 0), info = "Density values should be positive")
})

test_that("curve_gen parallel processing option (cores parameter)", {
  skip_if_not(Sys.info()["sysname"] %in% c("Darwin", "Linux"))

  model <- lm(mpg ~ wt + hp + drat + qsec, data = mtcars)

  # Single core
  result_1 <- suppressMessages(
    curve_gen(model, "wt", method = "lm", steps = 50, cores = 1, table = FALSE)
  )

  # Multiple cores (if available)
  result_2 <- suppressMessages(
    curve_gen(model, "wt", method = "lm", steps = 50, cores = 2, table = FALSE)
  )

  # Results should be identical or very close
  expect_equal(result_1[[1]], result_2[[1]], tolerance = 1e-6)
})
