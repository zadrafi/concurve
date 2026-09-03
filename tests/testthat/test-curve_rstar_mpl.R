context("curve_rstar and curve_mpl")

test_that("curve_rstar validates its inputs", {
  expect_error(curve_rstar(data.frame(x = 1)), "rstarci")
  fake <- structure(list(psivals = 1:5, rsvals = 5:1), class = "rstarci")
  expect_error(curve_rstar(fake, statistic = "bogus"), "statistic")
  fake_ronly <- structure(list(psivals = 1:5, rvals = 5:1), class = "rstarci")
  expect_error(curve_rstar(fake_ronly, statistic = "rstar"), "ronly")
})

test_that("curve_mpl validates its inputs", {
  skip_if_not_installed("likelihoodAsy")
  expect_error(
    curve_mpl(list(),
      mle = 0, floglik = "no", datagen = identity,
      indpsi = 1, lo = 0, hi = 1
    ),
    "functions"
  )
  expect_error(
    curve_mpl(list(),
      mle = 0, floglik = identity, datagen = identity,
      indpsi = 1, lo = 2, hi = 1
    ),
    "lo < hi"
  )
})

# Shared fixtures: 2x2 Poisson table (full exponential family, so small R
# suffices and results are stable) -- Pierce & Bellio, Example 4, without
# the continuity correction so the r statistic matches glm profiling.
loglik_pois <- function(theta, data) {
  mu <- exp(data$X %*% theta)
  sum(data$y * log(mu) - mu)
}
gendat_pois <- function(theta, data) {
  mu <- exp(data$X %*% theta)
  data$y <- rpois(n = length(data$y), lambda = mu)
  data
}
make_2x2 <- function() {
  rowf <- c(1, 0, 1, 0)
  colf <- c(1, 1, 0, 0)
  intf <- c(0, 0, 0, 1)
  list(y = c(15, 9, 7, 13), X = cbind(rep(1, 4), rowf, colf, intf))
}

test_that("curve_rstar reproduces likelihoodAsy's own confidence limits", {
  skip_if_not_installed("likelihoodAsy")
  skip_on_cran()
  data_2x2 <- make_2x2()
  rs_int <- likelihoodAsy::rstar.ci(
    data = data_2x2, thetainit = rep(0, 4), floglik = loglik_pois,
    fpsi = function(theta) theta[4], datagen = gendat_pois,
    trace = FALSE, R = 50, seed = 42
  )
  out <- suppressMessages(curve_rstar(rs_int, steps = 1000))

  # object contract
  expect_identical(
    names(out),
    c("Intervals Dataframe", "Intervals Density", "Intervals Table")
  )
  expect_identical(class(out), "concurve")
  df <- out[[1]]
  expect_identical(
    names(df),
    c(
      "lower.limit", "upper.limit", "intrvl.width", "intrvl.level",
      "cdf", "pvalue", "svalue"
    )
  )
  expect_identical(class(df), c("data.frame", "concurve"))
  expect_identical(names(out[[2]]), "x")

  # the key invariant: limits at 90/95/99% equal the object's own CIrs
  levs <- c(0.90, 0.95, 0.99)
  for (i in seq_along(levs)) {
    mine <- unlist(df[
      abs(df$intrvl.level - levs[i]) < 1e-9,
      c("lower.limit", "upper.limit")
    ])
    expect_equal(unname(mine), sort(rs_int$CIrs[i, ]), tolerance = 1e-8)
  }

  # first-order curve matches CIr
  out_r <- suppressMessages(curve_rstar(rs_int, statistic = "r", steps = 1000))
  mine_r <- unlist(out_r[[1]][
    abs(out_r[[1]]$intrvl.level - 0.95) < 1e-9,
    c("lower.limit", "upper.limit")
  ])
  expect_equal(unname(mine_r), sort(rs_int$CIr[2, ]), tolerance = 1e-8)

  # independent reference: the r-based 95% interval should approximate the
  # glm profile-likelihood CI for the interaction (same likelihood root)
  tab <- data.frame(y = data_2x2$y, data_2x2$X[, -1])
  fit <- glm(y ~ rowf + colf + intf, family = poisson(), data = tab)
  ref <- suppressMessages(confint(fit, "intf", level = 0.95))
  expect_equal(unname(mine_r), unname(ref), tolerance = 0.05)
})

test_that("curve_mpl emits the curve_lik contract and a sane MPL", {
  skip_if_not_installed("likelihoodAsy")
  skip_on_cran()
  data_2x2 <- make_2x2()
  mle <- coef(glm(data_2x2$y ~ data_2x2$X - 1, family = poisson()))
  out <- curve_mpl(
    data = data_2x2, mle = unname(mle), floglik = loglik_pois,
    datagen = gendat_pois, indpsi = 4, lo = -1, hi = 3,
    steps = 25, R = 50, seed = 42
  )
  expect_identical(names(out), c("Intervals Dataframe", "Intervals Table"))
  expect_identical(class(out), "concurve")
  lf <- out[[1]]
  expect_identical(
    names(lf),
    c("values", "likelihood", "loglikelihood", "support", "deviancestat")
  )
  expect_identical(class(lf), c("data.frame", "concurve"))
  expect_true(all(is.finite(lf$loglikelihood)))
  expect_equal(max(lf$support), 1)
  expect_equal(lf$deviancestat, -lf$loglikelihood)
  # MPL of a full exponential family tracks the conditional likelihood;
  # its maximum must lie near the unadjusted MLE of the log odds ratio
  argmax <- lf$values[which.max(lf$support)]
  expect_true(abs(argmax - unname(mle[4])) < 0.5)
})
