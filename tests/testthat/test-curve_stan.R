test_that("curve_stan() reproduces quantile-based limits with the standard structure", {
  set.seed(7713)
  n <- 12
  y <- rnorm(n, 3.2, 1.4)
  ybar <- mean(y)
  se <- sd(y) / sqrt(n)
  draws <- ybar + se * rt(50000, df = n - 1)

  out <- curve_stan(draws, steps = 1000)

  expect_s3_class(out, "concurve")
  expect_length(out, 3)
  expect_named(out, c("Intervals Dataframe", "Intervals Density", "Intervals Table"))
  expect_identical(
    names(out[[1]]),
    c("lower.limit", "upper.limit", "intrvl.width", "intrvl.level", "cdf", "pvalue", "svalue")
  )
  expect_identical(names(out[[2]]), "x")
  expect_equal(nrow(out[[1]]), 999)

  df <- out[[1]]
  expect_true(all(diff(df$intrvl.width) >= 0))
  expect_true(all(df$lower.limit <= df$upper.limit))

  # 95% limits close to the exact t interval (Monte Carlo tolerance)
  row95 <- df[which.min(abs(df$intrvl.level - 0.95)), ]
  ref <- ybar + se * qt(c(0.025, 0.975), df = n - 1)
  expect_lt(max(abs(c(row95$lower.limit, row95$upper.limit) - ref)), 0.05 * se)
})

test_that("curve_stan() validates its inputs", {
  expect_error(curve_stan(rnorm(50)), "at least 100")
  expect_error(curve_stan(rnorm(500), steps = 5), "steps")
  out <- curve_stan(rnorm(500), table = FALSE)
  expect_length(out, 2)
})

test_that("bundled Stan programs are shipped and locatable", {
  for (m in c("normal_gfd", "normal_profile", "normal_mle")) {
    path <- concurve_stan_file(m)
    expect_true(file.exists(path))
    expect_match(readLines(path), "^model \\{", all = FALSE)
  }
  expect_error(concurve_stan_file("nope"))
})

test_that("bundled Stan programs parse", {
  skip_on_cran()
  skip_if_not_installed("rstan")
  # stanc() is a syntax check only; no C++ compilation
  for (m in c("normal_gfd", "normal_profile", "normal_mle")) {
    expect_true(rstan::stanc(concurve_stan_file(m))$status)
  }
})

test_that("curve_stan_fit() fails informatively without a usable model", {
  skip_if_not_installed("rstan")
  expect_error(curve_stan_fit(42, data = list(), parameter = "mu"), "stanmodel")
  expect_error(curve_stan_fit("normal_gfd", data = list(), parameter = c("a", "b")), "single")
})
