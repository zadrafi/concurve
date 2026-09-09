test_that("curve_table() defaults to the conventional levels", {
  curves <- curve_gen(lm(mpg ~ wt, data = mtcars), "wt")

  tbl <- curve_table(curves[[1]], format = "data.frame")

  expect_s3_class(tbl, "concurve")
  expect_equal(nrow(tbl), 9L)
  expect_equal(
    tbl[["Interval Level (%)"]],
    c(25, 50, 75, 80, 85, 90, 95, 97.5, 99)
  )
})

test_that("curve_table() honours a supplied levels argument", {
  # Regression test: `levels` was overwritten in both branches from the
  # commit that introduced the function, so any value was discarded.
  curves <- curve_gen(lm(mpg ~ wt, data = mtcars), "wt")

  tbl <- curve_table(curves[[1]], levels = c(0.50, 0.95), format = "data.frame")

  expect_equal(nrow(tbl), 2L)
  expect_equal(tbl[["Interval Level (%)"]], c(50, 95))

  # Different requests must give different tables.
  other <- curve_table(curves[[1]], levels = 0.80, format = "data.frame")
  expect_equal(nrow(other), 1L)
  expect_false(identical(tbl, other))
})

test_that("curve_table() warns about levels that match no rows", {
  curves <- curve_gen(lm(mpg ~ wt, data = mtcars), "wt")

  expect_warning(
    tbl <- curve_table(
      curves[[1]],
      levels = c(0.95, 0.123456),
      format = "data.frame"
    ),
    "0.123456"
  )
  expect_equal(nrow(tbl), 1L)
})

test_that("curve_table() validates levels", {
  curves <- curve_gen(lm(mpg ~ wt, data = mtcars), "wt")

  expect_error(curve_table(curves[[1]], levels = "0.95"), "numeric")
  expect_error(curve_table(curves[[1]], levels = numeric(0)), "numeric")
  expect_error(curve_table(curves[[1]], levels = c(0.95, NA)), "numeric")
  expect_error(curve_table(curves[[1]], levels = 1.5), "between 0 and 1")
})
