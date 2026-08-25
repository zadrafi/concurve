## =====================================================================
## Confidence distributions with Stan
##
##   Route A  Generalized fiducial (Hannig): sample  L(theta) * J(y,theta)
##   Route B  Stan as a likelihood engine: profile deviance -> signed root
##   Route C  Coverage simulation -- the part you are not allowed to skip
##
## Worked on the normal location-scale model, where an exact confidence
## distribution is known, so every number below has a reference to be
## checked against.
## =====================================================================

library(cmdstanr)

## Point this at your CmdStan installation, or drop the line if
## cmdstanr already knows where it is.
if (nzchar(Sys.getenv("CMDSTAN"))) set_cmdstan_path(Sys.getenv("CMDSTAN"))

set.seed(1859)


## ---------------------------------------------------------------------
## 1. Stan programs
## ---------------------------------------------------------------------

## Route A -- generalized fiducial distribution.
##
## Data-generating equation:  y_i = mu + sigma * z_i,  z_i ~ N(0,1).
## Hannig's Jacobian formula gives the GFD density
##
##     r(theta | y)  proportional to  L(y, theta) * J(y, theta),
##     J = det( grad_theta F' grad_theta F )^(1/2).
##
## Here grad_theta F has rows (1, u_i) with u_i = (y_i - mu)/sigma, so
##
##     det(.) = n*sum(u^2) - (sum u)^2 = n(n-1)s^2 / sigma^2
##     J      = sqrt(n(n-1)) * s / sigma   proportional to  1/sigma.
##
## Derived, not assumed: for other models you must redo this. Note that
## J depends on the DATA. Stan does not care -- it needs a log density up
## to a constant and has no opinion about where the terms came from.
gfi_code <- "
data {
  int<lower=1> N;
  vector[N] y;
}
parameters {
  real mu;
  real<lower=0> sigma;
}
model {
  target += normal_lpdf(y | mu, sigma);   // likelihood
  target += -log(sigma);                  // fiducial Jacobian, up to a constant
}
"

## Route B -- the same likelihood, with mu passed as DATA so that
## optimising over sigma returns the profile log-likelihood at mu.
prof_code <- "
data {
  int<lower=1> N;
  vector[N] y;
  real mu_fixed;
}
parameters {
  real<lower=0> sigma;
}
model {
  target += normal_lpdf(y | mu_fixed, sigma);
}
"

## Unrestricted MLE, for the maximum of the profile.
mle_code <- "
data {
  int<lower=1> N;
  vector[N] y;
}
parameters {
  real mu;
  real<lower=0> sigma;
}
model {
  target += normal_lpdf(y | mu, sigma);
}
"

gfi_mod  <- cmdstan_model(write_stan_file(gfi_code))
prof_mod <- cmdstan_model(write_stan_file(prof_code))
mle_mod  <- cmdstan_model(write_stan_file(mle_code))


## ---------------------------------------------------------------------
## 2. Confidence distribution objects
## ---------------------------------------------------------------------

## A CD is a function, not a sample. Wrap draws in one.
cd_from_draws <- function(draws) {
  draws <- sort(as.numeric(draws))
  structure(
    list(
      H = stats::ecdf(draws),                                  # H(theta) = P(Theta <= theta)
      Q = function(p) as.numeric(stats::quantile(draws, p, type = 8, names = FALSE)),
      draws = draws,
      method = "monte carlo"
    ),
    class = "cdist"
  )
}

## Analytic version, for CDs available in closed form.
cd_from_functions <- function(H, Q, method = "analytic") {
  structure(list(H = H, Q = Q, draws = NULL, method = method), class = "cdist")
}

## p-value function (two-sided) and surprisal, the usual companions.
cd_pvalue  <- function(cd, theta) 2 * pmin(cd$H(theta), 1 - cd$H(theta))
cd_svalue  <- function(cd, theta) -log2(cd_pvalue(cd, theta))
cd_interval <- function(cd, level = 0.95) cd$Q(c((1 - level) / 2, (1 + level) / 2))

## Emit a concurve-compatible consonance object so ggcurve() works.
## Contract (concurve 3.0.0): list of 3, classed "concurve";
##   [[1]] 7 columns exactly; [[2]] one column named x; [[3]] curve_table.
as_concurve <- function(cd, steps = 1000) {
  levels <- (1:(steps - 1)) / steps
  lo <- cd$Q((1 - levels) / 2)
  hi <- cd$Q((1 + levels) / 2)
  df <- data.frame(
    lower.limit  = lo,
    upper.limit  = hi,
    intrvl.width = hi - lo,
    intrvl.level = levels,
    cdf          = levels / 2 + 0.5,
    pvalue       = 1 - levels,
    svalue       = -log2(1 - levels)
  )
  df <- utils::head(df, -1)
  class(df) <- c("data.frame", "concurve")

  dens <- data.frame(x = c(df$lower.limit, df$upper.limit))
  class(dens) <- c("data.frame", "concurve")

  tbl <- if (requireNamespace("concurve", quietly = TRUE)) {
    concurve::curve_table(
      df,
      levels = c(.25, .5, .75, .8, .85, .9, .95, .975, .99),
      type = "c", format = "data.frame"
    )
  } else NULL

  out <- list(df, dens, tbl)
  names(out) <- c("Intervals Dataframe", "Intervals Density", "Intervals Table")
  class(out) <- "concurve"
  out
}


## ---------------------------------------------------------------------
## 3. Route A -- fit the GFD
## ---------------------------------------------------------------------

n <- 12
y <- rnorm(n, mean = 3.2, sd = 1.4)
sdat <- list(N = n, y = y)

gfi_fit <- gfi_mod$sample(
  data = sdat, seed = 1859,
  chains = 4, parallel_chains = 4,
  iter_warmup = 2000, iter_sampling = 20000,
  refresh = 0, show_messages = FALSE
)

cd_gfi <- cd_from_draws(gfi_fit$draws("mu"))


## The exact CD for mu in this model, for comparison.
ybar <- mean(y); s <- sd(y); se <- s / sqrt(n)
cd_exact <- cd_from_functions(
  H = function(t) stats::pt((t - ybar) / se, df = n - 1),
  Q = function(p) ybar + se * stats::qt(p, df = n - 1),
  method = "exact (Student t)"
)


## ---------------------------------------------------------------------
## 4. Route B -- profile deviance via Stan's optimiser -> signed root
## ---------------------------------------------------------------------

stan_profile_loglik <- function(mod, y, mu_grid) {
  vapply(mu_grid, function(m) {
    o <- mod$optimize(
      data = list(N = length(y), y = y, mu_fixed = m),
      jacobian = FALSE, seed = 1, refresh = 0, show_messages = FALSE
    )
    o$lp()
  }, numeric(1))
}

mu_grid <- seq(ybar - 5 * se, ybar + 5 * se, length.out = 121)
lp_prof <- stan_profile_loglik(prof_mod, y, mu_grid)

mle_opt <- mle_mod$optimize(data = sdat, jacobian = FALSE, seed = 1,
                            refresh = 0, show_messages = FALSE)
lp_max  <- mle_opt$lp()
mu_hat  <- mle_opt$mle("mu")

## Signed root of the likelihood ratio; the CD is Phi(r).
r_signed  <- sign(mu_grid - mu_hat) * sqrt(pmax(0, 2 * (lp_max - lp_prof)))
H_prof_grid <- stats::pnorm(r_signed)

cd_prof <- cd_from_functions(
  H = stats::approxfun(mu_grid, H_prof_grid, rule = 2),
  Q = stats::approxfun(H_prof_grid, mu_grid, rule = 2),
  method = "signed root LR (first order)"
)


## ---------------------------------------------------------------------
## 5. Validation -- every number gets a reference
## ---------------------------------------------------------------------

cat("\n== Validation ==\n")

## (a) GFD marginal for mu must be exactly t_{n-1}(ybar, se).
grid <- seq(ybar - 3 * se, ybar + 3 * se, length.out = 41)
d_H <- max(abs(cd_gfi$H(grid) - cd_exact$H(grid)))
cat(sprintf("GFD vs exact t CD, max |H diff|      : %.5f\n", d_H))
stopifnot(d_H < 0.005)                                   # Monte Carlo tolerance

## (b) 95% interval from the GFD vs t.test().
ci_gfi <- cd_interval(cd_gfi, 0.95)
ci_ref <- as.numeric(stats::t.test(y)$conf.int)
cat(sprintf("GFD 95%% interval                     : [%.4f, %.4f]\n", ci_gfi[1], ci_gfi[2]))
cat(sprintf("t.test() 95%% interval                : [%.4f, %.4f]\n", ci_ref[1], ci_ref[2]))
stopifnot(max(abs(ci_gfi - ci_ref)) < 0.02)

## (c) CD ordinate equals one minus the one-sided p-value. By construction,
##     but if this ever fails a sign convention has been flipped.
mu0 <- 3.0
p_one_sided <- stats::t.test(y, mu = mu0, alternative = "less")$p.value
cat(sprintf("H(3.0) vs 1 - one-sided p            : %.6f vs %.6f\n",
            cd_exact$H(mu0), 1 - p_one_sided))
stopifnot(abs(cd_exact$H(mu0) - (1 - p_one_sided)) < 1e-8)

## (d) Stan's optimiser reproduces the analytic profile log-likelihood.
prof_analytic <- vapply(mu_grid, function(m) {
  s2 <- mean((y - m)^2)
  -n / 2 * log(2 * pi * s2) - n / 2
}, numeric(1))
d_lp <- max(abs(lp_prof - prof_analytic))
cat(sprintf("Stan profile lp vs analytic, max diff: %.3e\n", d_lp))
stopifnot(d_lp < 1e-5)

## (e) The first-order route is NOT exact. Report the error rather than
##     hiding it -- this is the whole reason section 6 exists.
d_prof <- max(abs(cd_prof$H(grid) - cd_exact$H(grid)))
cat(sprintf("Signed-root CD vs exact, max |H diff|: %.5f  (approximation, n=%d)\n",
            d_prof, n))

cat(sprintf("concurve object columns OK           : %s\n",
            identical(names(as_concurve(cd_exact, steps = 100)[[1]]),
                      c("lower.limit","upper.limit","intrvl.width",
                        "intrvl.level","cdf","pvalue","svalue"))))


## ---------------------------------------------------------------------
## 6. Route C -- coverage. Nothing inside HMC knows what this is.
## ---------------------------------------------------------------------
##
## Both CDs above are built analytically once validated against Stan, so
## the simulation runs in seconds instead of hours. If you change the
## model, re-validate first and only then substitute the fast version.

cd_coverage <- function(make_cd, n, mu_true = 0, sd_true = 1,
                        level = 0.95, reps = 4000) {
  hit <- vapply(seq_len(reps), function(i) {
    yy <- stats::rnorm(n, mu_true, sd_true)
    ci <- cd_interval(make_cd(yy), level)
    ci[1] <= mu_true && mu_true <= ci[2]
  }, logical(1))
  mean(hit)
}

make_exact <- function(yy) {
  m <- mean(yy); e <- stats::sd(yy) / sqrt(length(yy)); d <- length(yy) - 1
  cd_from_functions(function(t) stats::pt((t - m) / e, d),
                    function(p) m + e * stats::qt(p, d))
}

## First-order signed root, closed form for this model.
make_signed_root <- function(yy) {
  nn <- length(yy); m <- mean(yy)
  H <- function(t) {
    s2m <- vapply(t, function(v) mean((yy - v)^2), numeric(1))
    s2h <- mean((yy - m)^2)
    stats::pnorm(sign(t - m) * sqrt(pmax(0, nn * log(s2m / s2h))))
  }
  Q <- function(p) {
    vapply(p, function(pp) stats::uniroot(
      function(v) H(v) - pp,
      interval = m + c(-50, 50) * stats::sd(yy) / sqrt(nn)
    )$root, numeric(1))
  }
  cd_from_functions(H, Q)
}

cat("\n== Coverage of nominal 95% intervals ==\n")
for (nn in c(5, 12, 40)) {
  cat(sprintf("n = %2d   exact t: %.3f    signed root: %.3f\n", nn,
              cd_coverage(make_exact, nn, reps = 4000),
              cd_coverage(make_signed_root, nn, reps = 4000)))
}


## ---------------------------------------------------------------------
## 7. Plot
## ---------------------------------------------------------------------

if (requireNamespace("ggplot2", quietly = TRUE)) {
  pg <- seq(ybar - 4 * se, ybar + 4 * se, length.out = 400)
  pd <- rbind(
    data.frame(theta = pg, p = cd_pvalue(cd_exact, pg), method = "Exact (t)"),
    data.frame(theta = pg, p = cd_pvalue(cd_gfi,   pg), method = "GFI via Stan"),
    data.frame(theta = pg, p = cd_pvalue(cd_prof,  pg), method = "Signed root (Stan optimiser)")
  )
  p <- ggplot2::ggplot(pd, ggplot2::aes(theta, p, colour = method)) +
    ggplot2::geom_line(linewidth = 0.7) +
    ggplot2::labs(x = expression(mu), y = "two-sided p-value", colour = NULL) +
    ggplot2::theme_bw() + ggplot2::theme(legend.position = "bottom")
  ggplot2::ggsave("consonance_stan.png", p, width = 7, height = 4.5, dpi = 150)
  cat("\nWrote consonance_stan.png\n")
}

## If concurve is installed:
##   cv <- as_concurve(cd_gfi)
##   concurve::ggcurve(cv[[1]], type = "c")
##   concurve::ggcurve(cv[[2]], type = "cd")
