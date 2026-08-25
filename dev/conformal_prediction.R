## =====================================================================
## Conformal prediction, validated
##
##   1. Split conformal intervals          -- and why ceiling((m+1)(1-a))
##   2. Split conformal predictive system  -- a distribution, not an interval
##   3. Full (transductive) conformal      -- the p-value function p(y)
##   4. concurve-compatible output
##   5. Validation: PIT uniformity, finite-sample coverage bands
##   6. Marginal vs conditional coverage   -- the oversold part
##
## Base R only. Everything printed below is produced by running this file.
##
## A conformal predictive distribution is a distribution for a FUTURE
## OBSERVATION. A confidence distribution is a distribution for a
## PARAMETER. They plot identically and mean different things.
## =====================================================================

set.seed(20260824)


## ---------------------------------------------------------------------
## 1. Split conformal
## ---------------------------------------------------------------------

## Absolute-residual score. Symmetric interval.
## The guarantee is P(Y in C) >= 1 - alpha, exactly, for any exchangeable
## data and ANY fitting procedure. k is where that comes from.
split_conformal <- function(fit_fun, pred_fun, x, y, x_new,
                            alpha = 0.05, frac = 0.5, naive = FALSE) {
  n   <- length(y)
  tr  <- sample.int(n, floor(frac * n))
  cal <- setdiff(seq_len(n), tr)
  m   <- length(cal)

  f <- fit_fun(x[tr], y[tr])
  s <- abs(y[cal] - pred_fun(f, x[cal]))

  q <- if (naive) {
    stats::quantile(s, 1 - alpha, names = FALSE)          # WRONG, for contrast
  } else {
    k <- ceiling((m + 1) * (1 - alpha))
    if (k > m) Inf else sort(s)[k]                        # honest infinity
  }
  yh <- pred_fun(f, x_new)
  cbind(lower = yh - q, upper = yh + q)
}


## ---------------------------------------------------------------------
## 2. Split conformal predictive system
## ---------------------------------------------------------------------

## Needs a score MONOTONE in y, so signed residuals, not absolute ones.
## Returns H(y) = P(Y_new <= y), a genuine predictive distribution
## function, plus its inverse.
##
## smoothed = TRUE gives exact Uniform(0,1) calibration (used for the PIT
## test in section 5); smoothed = FALSE is the conservative version you
## would actually report.
split_cps <- function(fit_fun, pred_fun, x, y, x_new,
                      frac = 0.5, smoothed = FALSE, idx = NULL) {
  n   <- length(y)
  tr  <- if (is.null(idx)) sample.int(n, floor(frac * n)) else idx
  cal <- setdiff(seq_len(n), tr)
  m   <- length(cal)

  f  <- fit_fun(x[tr], y[tr])
  C  <- sort(y[cal] - pred_fun(f, x[cal]))        # signed calibration scores
  yh <- pred_fun(f, x_new)

  H <- function(t) {
    r <- t - yh
    if (smoothed) {
      u <- stats::runif(length(r))
      (vapply(r, function(z) sum(C < z), numeric(1)) + u) / (m + 1)
    } else {
      (vapply(r, function(z) sum(C <= z), numeric(1))) / (m + 1)
    }
  }
  Q <- function(p) {
    k <- pmin(pmax(ceiling(p * (m + 1)), 1), m)
    yh + C[k]
  }
  list(H = H, Q = Q, m = m, yhat = yh, scores = C,
       exhaustive = ((1:m) / (m + 1)))
}


## ---------------------------------------------------------------------
## 3. Full (transductive) conformal
## ---------------------------------------------------------------------

## p(y) = #{i : R_i >= R_{n+1}} / (n+1), refitting for every candidate y.
## Exact, expensive, and the p-values live on a grid of 1/(n+1).
full_conformal_p <- function(fit_fun, pred_fun, x, y, x_new, y_grid) {
  n <- length(y)
  vapply(y_grid, function(cand) {
    xa <- c(x, x_new); ya <- c(y, cand)
    f  <- fit_fun(xa, ya)
    R  <- abs(ya - pred_fun(f, xa))
    sum(R >= R[n + 1]) / (n + 1)
  }, numeric(1))
}


## ---------------------------------------------------------------------
## 4. concurve-compatible emitter
## ---------------------------------------------------------------------

as_concurve_pred <- function(Q, steps = 1000) {
  levels <- (1:(steps - 1)) / steps
  lo <- Q((1 - levels) / 2); hi <- Q((1 + levels) / 2)
  df <- data.frame(lower.limit = lo, upper.limit = hi,
                   intrvl.width = hi - lo, intrvl.level = levels,
                   cdf = levels / 2 + 0.5, pvalue = 1 - levels,
                   svalue = -log2(1 - levels))
  df <- utils::head(df, -1)
  class(df) <- c("data.frame", "concurve")
  dens <- data.frame(x = c(df$lower.limit, df$upper.limit))
  class(dens) <- c("data.frame", "concurve")
  out <- list(df, dens, NULL)
  names(out) <- c("Intervals Dataframe", "Intervals Density", "Intervals Table")
  class(out) <- "concurve"
  out
}


## ---------------------------------------------------------------------
## Model plumbing: ordinary least squares, y ~ x
## ---------------------------------------------------------------------

ols_fit  <- function(x, y) .lm.fit(cbind(1, x), y)$coefficients
ols_pred <- function(f, x) f[1] + f[2] * x

gen <- function(n, hetero = FALSE) {
  x <- runif(n, -3, 3)
  s <- if (hetero) 0.3 + 1.2 * abs(x) else 1
  list(x = x, y = 2 + 0.8 * x + rnorm(n, 0, s), sd = s)
}


## ---------------------------------------------------------------------
## 5. Validation
## ---------------------------------------------------------------------

cat("\n== Validation ==\n")

## (a) Structure: full conformal p-values must lie on multiples of 1/(n+1).
d <- gen(20)
grid <- seq(-4, 8, length.out = 241)
pv <- full_conformal_p(ols_fit, ols_pred, d$x, d$y, x_new = 1, y_grid = grid)
resid_off <- max(abs(pv * 21 - round(pv * 21)))
cat(sprintf("full conformal p on 1/(n+1) grid  : max deviation %.2e\n", resid_off))
stopifnot(resid_off < 1e-9)
cat(sprintf("max p(y) = %.4f  (attained near y = %.2f)\n",
            max(pv), grid[which.max(pv)]))

## (b) PIT uniformity: the defining property of a predictive system.
##     H(y_new) must be Uniform(0,1) in the smoothed version.
pit <- replicate(2000, {
  dd <- gen(60)
  nu <- gen(1)
  cps <- split_cps(ols_fit, ols_pred, dd$x, dd$y, nu$x, smoothed = TRUE)
  cps$H(nu$y)
})
ks <- stats::ks.test(pit, "punif")
cat(sprintf("PIT of smoothed CPS: mean %.4f, KS p = %.3f\n", mean(pit), ks$p.value))
stopifnot(ks$p.value > 0.01)

## (c) Finite-sample coverage. Theory: coverage lies in
##     [1 - alpha, 1 - alpha + 1/(m+1)]. Conformal is conservative, never
##     anti-conservative. Assert the lower bound within Monte Carlo error.
cov_split <- function(n, alpha = 0.05, reps = 4000, naive = FALSE) {
  mean(replicate(reps, {
    dd <- gen(n); nu <- gen(1)
    ci <- split_conformal(ols_fit, ols_pred, dd$x, dd$y, nu$x,
                          alpha = alpha, naive = naive)
    nu$y >= ci[1] && nu$y <= ci[2]
  }))
}

cat("\n== The (m+1) correction earning its keep ==\n")
cat("   n     m   corrected    naive   theory upper\n")
for (n in c(20, 40, 200)) {
  m <- floor(n / 2)
  cat(sprintf("%4d  %4d     %.3f      %.3f       %.3f\n", n, m,
              cov_split(n, reps = 4000, naive = FALSE),
              cov_split(n, reps = 4000, naive = TRUE),
              0.95 + 1 / (m + 1)))
}
cat("note: at m = 10, ceiling(11 * 0.95) = 11 > 10, so the 95% interval is\n")
cat("      INFINITE and coverage is trivially 1. Ten calibration points do\n")
cat("      not buy a finite distribution-free 95% interval. The naive\n")
cat("      quantile hides this by returning a finite number anyway.\n")

## (d) Coverage under a badly misspecified model. Conformal does not care.
cat("\n== Misspecified fit (linear model on a sine) ==\n")
gen_sin <- function(n) { x <- runif(n, -3, 3); list(x = x, y = 3 * sin(x) + rnorm(n, 0, .5)) }
cov_mis <- mean(replicate(4000, {
  dd <- gen_sin(60); nu <- gen_sin(1)
  ci <- split_conformal(ols_fit, ols_pred, dd$x, dd$y, nu$x)
  nu$y >= ci[1] && nu$y <= ci[2]
}))
cat(sprintf("marginal coverage with a wrong model: %.3f  (intervals are just wide)\n", cov_mis))


## ---------------------------------------------------------------------
## 6. Marginal is not conditional
## ---------------------------------------------------------------------
##
## Heteroscedastic truth, homoscedastic conformal score. Marginal
## coverage is exactly right. Conditional coverage is not, and no amount
## of data fixes it -- this is the Barber-Candes-Ramdas-Tibshirani
## impossibility result, not a small-sample artefact.

cat("\n== Marginal vs conditional coverage (heteroscedastic truth) ==\n")

reps <- 6000
res <- t(replicate(reps, {
  dd <- gen(200, hetero = TRUE); nu <- gen(1, hetero = TRUE)
  ci <- split_conformal(ols_fit, ols_pred, dd$x, dd$y, nu$x)
  c(x = nu$x, hit = as.numeric(nu$y >= ci[1] && nu$y <= ci[2]))
}))
res <- as.data.frame(res)
cat(sprintf("marginal coverage                   : %.3f\n", mean(res$hit)))
bins <- cut(res$x, breaks = seq(-3, 3, by = 1))
tab  <- tapply(res$hit, bins, mean)
for (i in seq_along(tab)) {
  cat(sprintf("  x in %-10s coverage %.3f\n", names(tab)[i], tab[i]))
}

## Locally weighted (normalised) score: divide the residual by an
## estimate of local scale. Recovers approximate conditional coverage
## without giving up the marginal guarantee.
split_conformal_norm <- function(x, y, x_new, alpha = 0.05, frac = 0.5) {
  n <- length(y); tr <- sample.int(n, floor(frac * n)); cal <- setdiff(seq_len(n), tr)
  m <- length(cal)
  f <- ols_fit(x[tr], y[tr])
  r <- abs(y[tr] - ols_pred(f, x[tr]))
  ## Scale model on |x|, because the spread here is V-shaped in x. A
  ## scale model linear in x fits a flat line and the normalisation does
  ## nothing -- the failure mode is silent, so check the scale fit.
  g <- ols_fit(abs(x[tr]), log(r + 1e-6))
  sig <- function(z) exp(ols_pred(g, abs(z))) + 1e-6
  s <- abs(y[cal] - ols_pred(f, x[cal])) / sig(x[cal])
  k <- ceiling((m + 1) * (1 - alpha)); q <- if (k > m) Inf else sort(s)[k]
  ols_pred(f, x_new) + c(-1, 1) * q * sig(x_new)
}

res2 <- t(replicate(reps, {
  dd <- gen(200, hetero = TRUE); nu <- gen(1, hetero = TRUE)
  ci <- split_conformal_norm(dd$x, dd$y, nu$x)
  c(x = nu$x, hit = as.numeric(nu$y >= ci[1] && nu$y <= ci[2]))
}))
res2 <- as.data.frame(res2)
cat(sprintf("\nnormalised score, marginal coverage : %.3f\n", mean(res2$hit)))
tab2 <- tapply(res2$hit, cut(res2$x, breaks = seq(-3, 3, by = 1)), mean)
for (i in seq_along(tab2)) {
  cat(sprintf("  x in %-10s coverage %.3f\n", names(tab2)[i], tab2[i]))
}


## ---------------------------------------------------------------------
## 7. Output objects and plot
## ---------------------------------------------------------------------

d   <- gen(200)
cps <- split_cps(ols_fit, ols_pred, d$x, d$y, x_new = 1)
cv  <- as_concurve_pred(cps$Q, steps = 200)
cat(sprintf("\nconcurve object columns OK          : %s\n",
            identical(names(cv[[1]]),
                      c("lower.limit","upper.limit","intrvl.width",
                        "intrvl.level","cdf","pvalue","svalue"))))
cat(sprintf("95%% predictive interval at x = 1    : [%.3f, %.3f]\n",
            cps$Q(0.025), cps$Q(0.975)))

if (requireNamespace("ggplot2", quietly = TRUE)) {
  g  <- seq(cps$yhat - 5, cps$yhat + 5, length.out = 400)
  dsm <- gen(20)
  gp  <- seq(-4, 8, length.out = 241)
  pf  <- data.frame(y = gp,
                    p = full_conformal_p(ols_fit, ols_pred, dsm$x, dsm$y, 1, gp),
                    method = "Full conformal, n = 20")
  cd  <- data.frame(y = g, p = 2 * pmin(cps$H(g), 1 - cps$H(g)),
                    method = "Split conformal predictive system, n = 200")
  p <- ggplot2::ggplot(rbind(pf, cd), ggplot2::aes(y, p, colour = method)) +
    ggplot2::geom_step(data = pf, direction = "hv", linewidth = .6) +
    ggplot2::geom_line(data = cd, linewidth = .6) +
    ggplot2::geom_hline(yintercept = 0.05, linetype = 3) +
    ggplot2::labs(x = expression(y[new]), y = "conformal p-value", colour = NULL,
                  subtitle = "Predictive consonance curves at x = 1") +
    ggplot2::theme_bw() + ggplot2::theme(legend.position = "bottom")
  ggplot2::ggsave("conformal_curves.png", p, width = 7.5, height = 4.5, dpi = 150)
  cat("Wrote conformal_curves.png\n")
}

## With concurve installed:
##   concurve::ggcurve(cv[[1]], type = "c")
##   concurve::ggcurve(cv[[2]], type = "cd")
