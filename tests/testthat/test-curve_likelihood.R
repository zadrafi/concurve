# tests/testthat/test-curve_likelihood.R
# All expectations verified standalone against concurve 3.0.0 (master),
# ProfileLikelihood 1.2, and stats::confint profile intervals.

test_that("as_curve_lik returns the curve_lik object contract", {
  p <- seq(0.001, 0.999, length.out = 2000)
  lik <- as_curve_lik(p, 8 * log(p) + 12 * log(1 - p))

  expect_s3_class(lik[[1]], "concurve")
  expect_identical(class(lik), "concurve")
  expect_named(lik, c("Intervals Dataframe", "Intervals Table"))
  expect_named(
    lik[[1]],
    c("values", "likelihood", "loglikelihood", "support", "deviancestat")
  )
  expect_equal(max(lik[[1]]$support), 1)
  expect_true(all(lik[[1]]$deviancestat >= 0))
  expect_equal(lik[[1]]$values[which.max(lik[[1]]$support)], 0.4, tolerance = 1e-2)
})

test_that("as_curve_lik validates input", {
  expect_error(as_curve_lik(1:5, 1:4), "same length")
  expect_error(as_curve_lik("a", 1), "numeric")
  expect_error(as_curve_lik(1:5, rep(NA_real_, 5)), "finite")
})

test_that("curve_lik_glm matches ProfileLikelihood on its canonical example", {
  skip_if_not_installed("ProfileLikelihood")
  library(ProfileLikelihood)
  data(dataglm)

  mod <- glm(y ~ group + x1 + x2, family = binomial, data = dataglm)
  lik_new <- curve_lik_glm(mod, "group", steps = 200)

  xx <- suppressWarnings(profilelike.glm(
    y ~ x1 + x2,
    data = dataglm, profile.theta = "group",
    family = binomial(link = "logit"), length = 500, round = 2
  ))
  lik_old <- curve_lik(xx, dataglm)

  f_new <- approxfun(lik_new[[1]]$values, lik_new[[1]]$loglikelihood)
  ov <- lik_old[[1]]$values[
    lik_old[[1]]$values > min(lik_new[[1]]$values) &
      lik_old[[1]]$values < max(lik_new[[1]]$values)
  ]
  dev <- max(
    abs(f_new(ov) - lik_old[[1]]$loglikelihood[match(ov, lik_old[[1]]$values)]),
    na.rm = TRUE
  )
  expect_lt(dev, 5e-3)
})

test_that("1/6.83 support interval reproduces confint profile CI for glm", {
  skip_if_not_installed("ProfileLikelihood")
  library(ProfileLikelihood)
  data(dataglm)

  mod <- glm(y ~ group + x1 + x2, family = binomial, data = dataglm)
  lik <- curve_lik_glm(mod, "group", steps = 200)
  si <- curve_support(lik[[1]], cutoffs = 6.83)
  ci <- suppressMessages(confint(mod, "group", level = 0.95))

  expect_lt(abs(si$lower.limit - ci[1]), 0.01)
  expect_lt(abs(si$upper.limit - ci[2]), 0.01)
  expect_lt(abs(si$mle - coef(mod)["group"]), 2e-3)
})

test_that("curve_lik_glm works for lm objects", {
  lmod <- lm(mpg ~ wt + hp, data = mtcars)
  lik <- curve_lik_glm(lmod, "wt", steps = 200)
  si <- curve_support(lik[[1]], cutoffs = 6.83)
  ci <- confint(lmod, "wt")

  # chi-square cutoff vs t interval: close but not identical at n = 32
  expect_lt(abs(si$lower.limit - ci[1]), 0.08)
  expect_lt(abs(si$upper.limit - ci[2]), 0.08)
  expect_lt(abs(si$mle - coef(lmod)["wt"]), 2e-3)
})

test_that("curve_lik_glm validates input", {
  lmod <- lm(mpg ~ wt, data = mtcars)
  expect_error(curve_lik_glm("not a model", "wt"), "lm")
  expect_error(curve_lik_glm(lmod, "nope"), "not a coefficient")
})

test_that("curve_lik_exact prop/or/rr peak at the design's estimator", {
  lp <- curve_lik_exact(type = "prop", x = 8, n = 20)
  expect_equal(lp[[1]]$values[which.max(lp[[1]]$support)], 0.4, tolerance = 1e-2)

  # conditional MLE solves E[A; psi] = a (score identity), != ad/bc
  lo <- curve_lik_exact(type = "or", a = 12, b = 8, c = 5, d = 15)
  psi <- exp(lo[[1]]$values[which.max(lo[[1]]$support)])
  n1 <- 20; n0 <- 20; m1 <- 17
  ks <- max(0, m1 - n0):min(m1, n1)
  w <- lchoose(n1, ks) + lchoose(n0, m1 - ks) + ks * log(psi)
  w <- exp(w - max(w)); w <- w / sum(w)
  expect_lt(abs(sum(ks * w) - 12), 0.05)
  expect_gt(abs(psi - (12 * 15) / (8 * 5)), 0.05) # differs from ad/bc

  lr <- curve_lik_exact(type = "rr", a = 30, t1 = 1000, b = 18, t0 = 1200)
  expect_equal(exp(lr[[1]]$values[which.max(lr[[1]]$support)]), 2, tolerance = 1e-2)
})

test_that("curve_lik_exact mean/var/corr peak at closed-form estimators", {
  set.seed(9)
  dd <- rnorm(40, 5, 2)

  lm2 <- curve_lik_exact(type = "mean", data = dd)
  expect_equal(lm2[[1]]$values[which.max(lm2[[1]]$support)], mean(dd), tolerance = 1e-2)

  lv <- curve_lik_exact(type = "var", data = dd)
  expect_equal(lv[[1]]$values[which.max(lv[[1]]$support)], var(dd), tolerance = 0.15)

  set.seed(10)
  u <- rnorm(35)
  v <- 0.5 * u + rnorm(35)
  lc <- curve_lik_exact(type = "corr", data = cbind(u, v), steps = 300)
  expect_equal(lc[[1]]$values[which.max(lc[[1]]$support)], cor(u, v), tolerance = 1e-2)
})

test_that("curve_lik_exact validates input", {
  expect_error(curve_lik_exact(type = "prop", x = 25, n = 20), "\\[0, n\\]")
  expect_error(curve_lik_exact(type = "or", a = 1, b = 2, c = 3), "cell counts")
  expect_error(curve_lik_exact(type = "mean", data = "x"), "numeric")
  expect_error(curve_lik_exact(type = "corr", data = 1:10), "two-column")
})

test_that("curve_support interpolates off-grid and nests across cutoffs", {
  lp <- curve_lik_exact(type = "prop", x = 8, n = 20)
  sp <- curve_support(lp[[1]])

  expect_equal(nrow(sp), 3)
  expect_true(all(diff(sp$lower.limit) < 0)) # wider as k grows
  expect_true(all(diff(sp$upper.limit) > 0))
  expect_true(all(sp$lower.limit < sp$mle & sp$mle < sp$upper.limit))
  expect_lt(abs(sp$mle[1] - 0.4), 1e-3)
})

test_that("curve_support rejects malformed input", {
  expect_error(curve_support(data.frame(a = 1)), "concurve")
  lp <- curve_lik_exact(type = "prop", x = 8, n = 20)
  expect_error(curve_support(lp[[1]], cutoffs = 0.5), "greater than 1")
})

test_that("ggcurve renders all likelihood types from native constructors", {
  lp <- curve_lik_exact(type = "prop", x = 8, n = 20)
  for (tp in c("l1", "l2", "l3", "d")) {
    expect_s3_class(ggcurve(lp[[1]], type = tp), "ggplot")
  }
})
