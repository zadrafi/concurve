# curve_from_ratio() / curve_from_se() are thin wrappers around curve_rev().
# Reference: a normal-theory interval reconstructed from the reported bounds.
#   ratio:  se = log(UL/LL) / (2 * qnorm(1 - (1 - conf.level)/2)),
#           limits at level L = exp(log(ratio) -/+ qnorm((1 + L)/2) * se)
#   mean:   limits at level L = estimate -/+ qnorm((1 + L)/2) * se
# Compare by joining on intrvl.level, never on row index.

test_that("curve_from_ratio returns the concurve interval contract", {
  res <- curve_from_ratio(ratio = 2, lower = 1.5, upper = 8 / 3, steps = 200)

  expect_s3_class(res, "concurve")
  expect_identical(
    names(res),
    c("Intervals Dataframe", "Intervals Density", "Intervals Table")
  )
  df <- res[[1]]
  expect_identical(class(df), c("data.frame", "concurve"))
  expect_identical(
    names(df),
    c("lower.limit", "upper.limit", "intrvl.width", "intrvl.level",
      "cdf", "pvalue", "svalue")
  )
  expect_identical(ncol(res[[2]]), 1L)
  expect_true(all(df$lower.limit > 0))
  expect_true(all(df$lower.limit < df$upper.limit))
  expect_equal(df$pvalue, 1 - df$intrvl.level)
  expect_equal(df$svalue, -log2(df$pvalue))
})

test_that("curve_from_ratio matches the log-normal closed form at every level", {
  ratio <- 2
  lower <- 1.5
  upper <- 8 / 3 # exactly log-symmetric around 2, so the 95% CI is recovered
  res <- curve_from_ratio(ratio, lower, upper, steps = 500)
  df <- res[[1]]

  se <- log(upper / lower) / (2 * qnorm(0.975))
  z <- qnorm((1 + df$intrvl.level) / 2)
  expect_equal(df$lower.limit, exp(log(ratio) - z * se), tolerance = 1e-10)
  expect_equal(df$upper.limit, exp(log(ratio) + z * se), tolerance = 1e-10)

  row95 <- df[which.min(abs(df$intrvl.level - 0.95)), ]
  expect_equal(row95$lower.limit, lower, tolerance = 1e-8)
  expect_equal(row95$upper.limit, upper, tolerance = 1e-8)
})

test_that("curve_from_ratio honours conf.level for the reported interval", {
  res <- curve_from_ratio(1.8, 1.2, 2.7, conf.level = 0.90, steps = 500)
  df <- res[[1]]
  se <- log(2.7 / 1.2) / (2 * qnorm(0.95))
  z <- qnorm((1 + df$intrvl.level) / 2)
  expect_equal(df$upper.limit, exp(log(1.8) + z * se), tolerance = 1e-10)
})

test_that("curve_from_ratio validates its inputs", {
  expect_error(curve_from_ratio(-1, 0.5, 2), "must be positive")
  expect_error(curve_from_ratio(1.5, 0, 2), "must be positive")
  expect_error(curve_from_ratio(1.5, 2, 1), "Lower bound must be less")
  expect_warning(
    curve_from_ratio(3, 1.1, 2, steps = 50),
    "outside the confidence interval"
  )
})

test_that("curve_from_ratio does not print to the console", {
  expect_silent(curve_from_ratio(2, 1.5, 8 / 3, steps = 50))
})

test_that("curve_from_se matches the normal closed form", {
  res <- curve_from_se(estimate = 0.3, se = 0.1, steps = 500)
  df <- res[[1]]
  expect_s3_class(res, "concurve")
  z <- qnorm((1 + df$intrvl.level) / 2)
  expect_equal(df$lower.limit, 0.3 - z * 0.1, tolerance = 1e-10)
  expect_equal(df$upper.limit, 0.3 + z * 0.1, tolerance = 1e-10)
  expect_equal(df$intrvl.width, df$upper.limit - df$lower.limit)
})

test_that("curve_from_se with measure = 'ratio' uses the delta-method log-scale se", {
  # estimate and se are on the ratio scale; log-scale se = se / estimate
  res <- curve_from_se(estimate = 2, se = 0.3, measure = "ratio", steps = 500)
  df <- res[[1]]
  z <- qnorm((1 + df$intrvl.level) / 2)
  expect_equal(df$lower.limit, exp(log(2) - z * 0.15), tolerance = 1e-10)
  expect_equal(df$upper.limit, exp(log(2) + z * 0.15), tolerance = 1e-10)
})

test_that("curve_from_se validates its inputs", {
  expect_error(curve_from_se("a", 0.1), "must be numeric")
  expect_error(curve_from_se(0.3, 0), "must be positive")
  expect_error(curve_from_se(-1, 0.1, measure = "ratio"), "estimate must be positive")
})
