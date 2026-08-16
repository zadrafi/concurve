# tests/testthat/test-construct_likelihood.R

test_that("construct_likelihood works with lm", {
  model <- lm(mpg ~ wt + hp, data = mtcars)
  lik <- construct_likelihood(model)

  expect_s3_class(lik, "likelihood_function")
  expect_equal(length(lik$mle), 3)
  expect_true(lik$converged)

  # MLE should match model coefficients
  expect_equal(lik$mle, coef(model), tolerance = 1e-6)
})


test_that("construct_likelihood works from scratch", {
  lik <- construct_likelihood(
    data = mtcars,
    formula = mpg ~ wt + hp,
    family = gaussian()
  )

  expect_s3_class(lik, "likelihood_function")

  # Should match lm results
  model <- lm(mpg ~ wt + hp, data = mtcars)
  expect_equal(lik$mle, coef(model), tolerance = 1e-4)
})


test_that("profile likelihood works", {
  model <- lm(mpg ~ wt, data = mtcars)
  lik <- construct_likelihood(model)

  prof <- lik$profile("wt", seq(-7, -3, length.out = 20))

  expect_s3_class(prof, "data.frame")
  expect_equal(nrow(prof), 20)
  expect_true(all(c("parameter", "value", "loglik", "deviance") %in% names(prof)))

  # Deviance should be minimized at MLE
  expect_equal(which.min(prof$deviance),
    which.min(abs(prof$value - lik$mle["wt"])),
    tolerance = 2
  )
})


test_that("likelihood-based CI works", {
  model <- lm(mpg ~ wt + hp, data = mtcars)
  lik <- construct_likelihood(model)

  ci <- confint(lik, parm = "wt", level = 0.95)

  expect_type(ci, "double")
  expect_equal(length(ci), 2)
  expect_true(ci[1] < lik$mle["wt"])
  expect_true(ci[2] > lik$mle["wt"])

  # Should be similar to Wald CI (for well-behaved models)
  wald_ci <- confint.default(model, parm = "wt")
  expect_equal(ci, wald_ci, tolerance = 0.5)
})


test_that("likelihood works for GLM", {
  model <- glm(am ~ wt + hp, data = mtcars, family = binomial)
  lik <- construct_likelihood(model)

  expect_s3_class(lik, "likelihood_function")
  expect_equal(lik$family$family, "binomial")

  # MLE should match GLM coefficients
  expect_equal(lik$mle, coef(model), tolerance = 1e-6)
})


test_that("vcov method works", {
  model <- lm(mpg ~ wt + hp, data = mtcars)
  lik <- construct_likelihood(model)

  vcov_lik <- vcov(lik)
  vcov_model <- vcov(model)

  expect_equal(dim(vcov_lik), dim(vcov_model))
  expect_equal(vcov_lik, vcov_model, tolerance = 1e-6)
})


test_that("logLik method works", {
  model <- lm(mpg ~ wt + hp, data = mtcars)
  lik <- construct_likelihood(model)

  ll_lik <- logLik(lik)
  ll_model <- logLik(model)

  expect_s3_class(ll_lik, "logLik")
  expect_equal(as.numeric(ll_lik), as.numeric(ll_model), tolerance = 1e-6)
})


test_that("plotting doesn't error", {
  model <- lm(mpg ~ wt + hp, data = mtcars)
  lik <- construct_likelihood(model)

  expect_silent(plot(lik, parameter = "wt"))
  expect_silent(plot(lik, parameter = "wt", type = "deviance"))
})


test_that("handles edge cases", {
  # Single predictor
  model <- lm(mpg ~ wt, data = mtcars)
  lik <- construct_likelihood(model)
  expect_equal(length(lik$mle), 2)

  # Intercept only
  model <- lm(mpg ~ 1, data = mtcars)
  lik <- construct_likelihood(model)
  expect_equal(length(lik$mle), 1)
})
