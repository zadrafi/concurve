test_that("curve_from_ratio produces valid output", {
  result <- curve_from_ratio(point = 1.5, lower_ci = 1.1, upper_ci = 2.0)

  expect_s3_class(result, "concurve")
  expect_true(all(result[[1]]$lower.limit > 0)) # ratios are positive
  expect_true(all(result[[1]]$upper.limit > 0))
})

test_that("curve_from_ratio recovers input CI", {
  result <- curve_from_ratio(point = 2.0, lower_ci = 1.5, upper_ci = 2.7, ci_level = 0.95)

  # Find 95% CI in output
  df <- result[[1]]
  ci95_row <- df[which.min(abs(df$intrvl.level - 0.95)), ]

  expect_equal(ci95_row$lower.limit, 1.5, tolerance = 0.05)
  expect_equal(ci95_row$upper.limit, 2.7, tolerance = 0.05)
})
