# tests/testthat/test-curve_analytic.R
# Every expectation here was verified standalone against concurve 3.0.0
# (master) before being encoded as a test.

test_that("curve_analytic z matches curve_gen lm limits exactly", {
  set.seed(1)
  GA <- rnorm(80)
  GB <- rnorm(80)
  mod <- lm(GA ~ GB)
  est <- coef(mod)["GB"]
  se <- sqrt(diag(vcov(mod)))["GB"]

  gen <- suppressMessages(curve_gen(mod, "GB", method = "lm", steps = 1000))
  ana <- curve_analytic(estimate = est, se = se, dist = "z", steps = 1000)

  expect_lt(max(abs(gen[[1]]$lower.limit - ana[[1]]$lower.limit)), 1e-8)
  expect_lt(max(abs(gen[[1]]$upper.limit - ana[[1]]$upper.limit)), 1e-8)
  expect_identical(names(gen[[1]]), names(ana[[1]]))
  expect_identical(class(gen[[1]]), class(ana[[1]]))
  expect_identical(names(gen[[2]]), names(ana[[2]]))
})

test_that("curve_analytic corr matches curve_corr on aligned levels", {
  set.seed(2)
  x <- rnorm(40)
  y <- 0.5 * x + rnorm(40)

  cc <- suppressWarnings(
    curve_corr(x, y, alternative = "two.sided", method = "pearson", steps = 1000)
  )
  ca <- curve_analytic(estimate = cor(x, y), n = 40, dist = "corr", steps = 1000)

  mg <- merge(
    cc[[1]][, c("intrvl.level", "lower.limit", "upper.limit")],
    ca[[1]][, c("intrvl.level", "lower.limit", "upper.limit")],
    by = "intrvl.level"
  )
  expect_gt(nrow(mg), 900)
  expect_lt(max(abs(mg$lower.limit.x - mg$lower.limit.y)), 1e-6)
  expect_lt(max(abs(mg$upper.limit.x - mg$upper.limit.y)), 1e-6)
})

test_that("curve_analytic t matches the closed-form t interval", {
  tt <- curve_analytic(estimate = 1.5, se = 0.6, df = 24, dist = "t")
  row95 <- tt[[1]][which.min(abs(tt[[1]]$intrvl.level - 0.95)), ]
  expect_lt(abs(row95$lower.limit - (1.5 - qt(0.975, 24) * 0.6)), 1e-6)
  expect_lt(abs(row95$upper.limit - (1.5 + qt(0.975, 24) * 0.6)), 1e-6)
})

test_that("curve_analytic var matches the chi-square interval", {
  vv <- curve_analytic(estimate = 4.2, n = 25, dist = "var")
  row95 <- vv[[1]][which.min(abs(vv[[1]]$intrvl.level - 0.95)), ]
  expect_lt(abs(row95$lower.limit - 24 * 4.2 / qchisq(0.975, 24)), 1e-6)
  expect_lt(abs(row95$upper.limit - 24 * 4.2 / qchisq(0.025, 24)), 1e-6)
})

test_that("curve_analytic prop stays in [0, 1] and log = TRUE exponentiates", {
  ww <- curve_analytic(estimate = 12, n = 40, dist = "prop")
  expect_true(all(ww[[1]]$lower.limit >= 0 & ww[[1]]$upper.limit <= 1))

  hr <- curve_analytic(estimate = log(0.80), se = 0.16, dist = "z", log = TRUE)
  expect_lt(abs(hr[[1]]$lower.limit[1] - 0.80), 0.01)
})

test_that("curve_analytic validates its inputs", {
  expect_error(curve_analytic(estimate = 1, dist = "z"), "se")
  expect_error(curve_analytic(estimate = 1, se = 1, dist = "t"), "df")
  expect_error(curve_analytic(estimate = 1.2, n = 30, dist = "corr"), "correlation")
  expect_error(curve_analytic(estimate = 0.5, n = 30, dist = "corr", log = TRUE), "log")
  expect_error(curve_analytic(estimate = -1, n = 10, dist = "var"), "positive")
  expect_error(curve_analytic(estimate = 50, n = 40, dist = "prop"), "count")
})

test_that("curve_region recovers exact normal-theory probabilities", {
  hr <- curve_analytic(estimate = log(0.80), se = 0.16, dist = "z", log = TRUE)

  reg <- curve_region(hr[[1]], lower = 0, upper = 1, nullvalue = 1)
  expect_lt(abs(reg$conf.region - pnorm((0 - log(0.80)) / 0.16)), 2e-4)
  expect_lt(abs(reg$pvalue - 2 * (1 - pnorm(abs(log(0.80)) / 0.16))), 2e-4)
  expect_lt(abs(reg$counternull - exp(2 * log(0.80))), 2e-3)

  eq <- curve_region(hr[[1]], lower = 0.9, upper = 1.1)
  truth_eq <- pnorm((log(1.1) - log(0.8)) / 0.16) - pnorm((log(0.9) - log(0.8)) / 0.16)
  expect_lt(abs(eq$conf.region - truth_eq), 2e-4)
})

test_that("curve_region works on inversion-based concurve objects too", {
  set.seed(1)
  GA <- rnorm(80)
  GB <- rnorm(80)
  mod <- lm(GA ~ GB)
  est <- coef(mod)["GB"]
  se <- sqrt(diag(vcov(mod)))["GB"]

  gen <- suppressMessages(curve_gen(mod, "GB", method = "lm", steps = 1000))
  r2 <- curve_region(gen[[1]], lower = -Inf, upper = 0)
  expect_lt(abs(r2$conf.region - pnorm((0 - est) / se)), 2e-4)
})

test_that("curve_region rejects malformed input", {
  expect_error(curve_region(data.frame(a = 1)), "concurve")
  hr <- curve_analytic(estimate = 0, se = 1, dist = "z")
  expect_error(curve_region(hr[[1]], lower = 2, upper = 1), "less than")
})
