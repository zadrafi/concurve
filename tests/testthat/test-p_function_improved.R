# tests/testthat/test-p_function_improved.R
# p_function is planned but not yet implemented; skip until available.

test_that("p_function validates inputs correctly", {
  skip("p_function not yet implemented")
  model <- lm(mpg ~ wt + hp, data = mtcars)

  # Invalid ci_levels
  expect_error(
    p_function(model, ci_levels = "invalid"),
    "ci_levels must be numeric"
  )

  expect_error(
    p_function(model, ci_levels = c(-0.1, 0.5)),
    "ci_levels must be between 0 and 1"
  )

  expect_error(
    p_function(model, ci_levels = c(0.5, 0.5, 0.9)),
    "duplicate values"
  )

  # Invalid resolution
  expect_error(
    p_function(model, resolution = 0.5),
    "resolution must be between 0.001 and 0.1"
  )

  expect_error(
    p_function(model, resolution = c(0.01, 0.02)),
    "single numeric value"
  )
})


test_that("p_function handles edge cases", {
  skip("p_function not yet implemented")
  # Model with perfect fit (intercept only)
  perfect_data <- data.frame(y = rep(5, 10), x = 1:10)
  model_perfect <- lm(y ~ 1, data = perfect_data)

  result <- p_function(model_perfect, verbose = FALSE)
  expect_s3_class(result, "parameters_p_function")

  # Model with near-zero standard errors (should warn)
  singular_data <- data.frame(
    y = 1:10,
    x1 = 1:10,
    x2 = 1:10 # Perfect collinearity
  )

  expect_warning(
    lm(y ~ x1 + x2, data = singular_data),
    NA # Model fitting warning is expected
  )
})


test_that("p_function performance optimization works", {
  skip("p_function not yet implemented")
  model <- lm(mpg ~ wt + hp + drat + qsec, data = mtcars)

  # Time the function with default resolution
  time_default <- system.time({
    result_default <- p_function(model, verbose = FALSE)
  })

  # Time with coarser resolution (should be faster)
  time_coarse <- system.time({
    result_coarse <- p_function(model, resolution = 0.05, verbose = FALSE)
  })

  # Coarse resolution should be faster
  expect_lt(time_coarse[["elapsed"]], time_default[["elapsed"]])

  # Both should return valid results
  expect_s3_class(result_default, "parameters_p_function")
  expect_s3_class(result_coarse, "parameters_p_function")

  # Coarse should have fewer rows
  expect_lt(nrow(result_coarse), nrow(result_default))
})


test_that("p_function handles different model types", {
  skip("p_function not yet implemented")
  # Linear model
  lm_model <- lm(mpg ~ wt, data = mtcars)
  lm_result <- p_function(lm_model, verbose = FALSE)
  expect_s3_class(lm_result, "parameters_p_function")

  # GLM
  glm_model <- glm(am ~ wt + hp, data = mtcars, family = binomial)
  glm_result <- p_function(glm_model, verbose = FALSE)
  expect_s3_class(glm_result, "parameters_p_function")

  # Check exponentiation for GLM
  glm_exp <- p_function(glm_model, exponentiate = TRUE, verbose = FALSE)
  expect_true(all(glm_exp$CI_low > 0)) # All ORs should be positive
})


test_that("p_function output structure is correct", {
  skip("p_function not yet implemented")
  model <- lm(mpg ~ wt + hp, data = mtcars)
  result <- p_function(model, verbose = FALSE)

  # Check required columns
  expect_true(all(c("Parameter", "CI", "CI_low", "CI_high", "group") %in% names(result)))

  # Check attributes
  expect_true(!is.null(attr(result, "data")))
  expect_true(!is.null(attr(result, "point_estimate")))
  expect_true(!is.null(attr(result, "resolution")))

  # Point estimates should be named vector
  pe <- attr(result, "point_estimate")
  expect_type(pe, "double")
  expect_true(!is.null(names(pe)))

  # Check monotonicity
  for (param in unique(result$Parameter)) {
    param_data <- result[result$Parameter == param, ]
    param_data <- param_data[order(param_data$CI), ]
    widths <- param_data$CI_high - param_data$CI_low
    expect_true(is.unsorted(widths) == FALSE)
  }
})


test_that("p_function filtering works", {
  skip("p_function not yet implemented")
  model <- lm(mpg ~ wt + hp + drat, data = mtcars)

  # Keep only specific parameters
  result_keep <- p_function(model, keep = "wt", verbose = FALSE)
  expect_true(all(result_keep$Parameter == "wt"))

  # Drop specific parameters
  result_drop <- p_function(model, drop = "wt", verbose = FALSE)
  expect_false("wt" %in% result_drop$Parameter)
})


test_that("p_function handles mixed models correctly", {
  skip("p_function not yet implemented")
  skip_if_not_installed("lme4")

  # Fit mixed model
  library(lme4)
  model <- lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy)

  # Should suggest profile methods
  expect_message(
    result <- p_function(model, verbose = TRUE),
    "Profile likelihood methods"
  )

  # Should still return valid results
  expect_s3_class(result, "parameters_p_function")
})


test_that("p_function validation catches issues", {
  skip("p_function not yet implemented")
  # Create problematic model
  set.seed(123)
  problem_data <- data.frame(
    y = rnorm(20),
    x = c(rep(0, 19), 100) # Extreme outlier
  )

  model <- lm(y ~ x, data = problem_data)

  # Should complete without error but may warn about extreme widths
  result <- p_function(model, verbose = TRUE)
  expect_s3_class(result, "parameters_p_function")
})


test_that("p_function respects ci_levels emphasis", {
  skip("p_function not yet implemented")
  model <- lm(mpg ~ wt, data = mtcars)

  result <- p_function(
    model,
    ci_levels = c(0.5, 0.8, emph = 0.95),
    verbose = FALSE
  )

  # Check that emphasis is applied
  expect_true(2 %in% result$group) # group 2 = emphasized
  emph_rows <- result[result$group == 2, ]
  expect_equal(unique(emph_rows$CI), 0.95)
})


test_that("format and print methods work", {
  skip("p_function not yet implemented")
  model <- lm(mpg ~ wt + hp, data = mtcars)
  result <- p_function(model, verbose = FALSE)

  # Test format method
  formatted <- format(result, digits = 3)
  expect_s3_class(formatted, "data.frame")

  # Test print method (should not error)
  expect_output(print(result), "Consonance")
})


test_that("p_function handles robust vcov", {
  skip("p_function not yet implemented")
  skip_if_not_installed("sandwich")

  model <- lm(mpg ~ wt + hp, data = mtcars)

  # With robust SE
  result_robust <- p_function(
    model,
    vcov = "HC3",
    verbose = FALSE
  )

  # With default SE
  result_default <- p_function(model, verbose = FALSE)

  # Should produce different intervals
  expect_false(identical(result_robust$CI_low, result_default$CI_low))
})
