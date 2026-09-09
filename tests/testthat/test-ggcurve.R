consonance_curves <- function() {
  list(
    curve_gen(lm(mpg ~ wt, data = mtcars), "wt", steps = 100),
    curve_gen(lm(mpg ~ hp, data = mtcars), "hp", steps = 100)
  )
}

test_that("ggcurve() validates type, measure and position", {
  df <- consonance_curves()[[1]][[1]]

  # Before validation an unrecognised value fell through every branch and
  # returned NULL invisibly, so a typo produced no plot and no error.
  expect_error(ggcurve(df, type = "s1"), "must be one of")
  expect_error(ggcurve(df, measure = "log"), "must be one of")
  expect_error(ggcurve(df, position = "sideways"), "must be one of")

  # "pd" was documented for years but never implemented; the branch is "cdf".
  expect_error(ggcurve(df, type = "pd"), "must be one of")
})

test_that("ggcurve() still accepts every documented type", {
  curves <- consonance_curves()[[1]]

  for (ty in c("c", "s")) {
    expect_s3_class(ggcurve(curves[[1]], type = ty), "ggplot")
  }
  # "cdf" and "cd" read the density data frame, not the interval limits.
  for (ty in c("cdf", "cd")) {
    expect_s3_class(ggcurve(curves[[2]], type = ty), "ggplot")
  }
})

test_that("plot_compare() validates type, measure and position", {
  curves <- consonance_curves()
  d1 <- curves[[1]][[1]]
  d2 <- curves[[2]][[1]]

  expect_error(plot_compare(d1, d2, type = "nope"), "must be one of")
  expect_error(plot_compare(d1, d2, measure = "log"), "must be one of")
  expect_error(plot_compare(d1, d2, position = "sideways"), "must be one of")

  # "cdf" and "cd" are ggcurve() types; plot_compare() implements neither,
  # though its help page used to advertise them.
  expect_error(plot_compare(d1, d2, type = "cdf"), "must be one of")
  expect_error(plot_compare(d1, d2, type = "cd"), "must be one of")
})

test_that("plot_compare() still accepts its documented types", {
  curves <- consonance_curves()
  d1 <- curves[[1]][[1]]
  d2 <- curves[[2]][[1]]

  for (ty in c("c", "s")) {
    expect_s3_class(plot_compare(d1, d2, type = ty), "ggplot")
  }
})
