## =====================================================================
## Confidence distributions with Stan (rstan edition)
##
##   Route A  Generalized fiducial (Hannig): sample  L(theta) * J(y,theta)
##   Route B  Stan as a likelihood engine: profile deviance -> signed root
##   Route C  Coverage simulation -- the part you are not allowed to skip
##
## Worked on the normal location-scale model, where an exact confidence
## distribution is known, so every number below has a reference to be
## checked against.
##
## Requirements: rstan (a Suggests of concurve, not an Imports) and a C++
## toolchain. The Stan programs live in inst/stan/ and are compiled here
## at runtime; nothing in the package build depends on them. Run
## devtools::load_all() (or install concurve) first so curve_stan() and
## concurve_stan_file() are available.
## =====================================================================

if (!requireNamespace("rstan", quietly = TRUE)) {
  stop("This script needs the 'rstan' package: install.packages(\"rstan\")")
}
if (!requireNamespace("concurve", quietly = TRUE)) {
  stop("Run devtools::load_all() or install concurve first.")
}

## Chains run in parallel on however many cores are present. Do NOT set
## rstan_options(auto_write = TRUE): it writes platform-specific .rds
## files next to the .stan sources in inst/stan, which would ship in the
## tarball. Compiled models live only in this session.
options(mc.cores = max(1L, parallel::detectCores() - 1L))

set.seed(1859)


## ---------------------------------------------------------------------
## 1. Stan programs -- shipped in inst/stan/, see ?concurve_stan_file
## ---------------------------------------------------------------------

## When running from the source tree, prefer inst/stan so edits to the
## .stan files are picked up without reinstalling.
stan_file <- function(name) {
  local <- file.path("inst", "stan", paste0(name, ".stan"))
  if (file.exists(local)) local else concurve::concurve_stan_file(name)
}

gfi_mod  <- rstan::stan_model(file = stan_file("normal_gfd"))
prof_mod <- rstan::stan_model(file = stan_file("normal_profile"))
mle_mod  <- rstan::stan_model(file = stan_file("normal_mle"))


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
cd_pvalue   <- function(cd, theta) 2 * pmin(cd$H(theta), 1 - cd$H(theta))
cd_svalue   <- function(cd, theta) -log2(cd_pvalue(cd, theta))
cd_interval <- function(cd, level = 0.95) cd$Q(c((1 - level) / 2, (1 + level) / 2))


## ---------------------------------------------------------------------
## 3. Route A -- fit the GFD
## ---------------------------------------------------------------------

n <- 12
y <- rnorm(n, mean = 3.2, sd = 1.4)
sdat <- list(N = n, y = y)

## rstan counts warmup inside iter: 2000 warmup + 20000 kept per chain.
gfi_fit <- rstan::sampling(
  gfi_mod, data = sdat, seed = 1859,
  chains = 4, iter = 22000, warmup = 2000,
  refresh = 0
)

gfi_draws <- rstan::extract(gfi_fit, pars = "mu")$mu
cd_gfi <- cd_from_draws(gfi_draws)

## The same thing through the package API: a concurve object straight
## from the draws, so ggcurve()/curve_table() work on it.
cv_gfi <- concurve::curve_stan(gfi_draws)

## Or, in one step (compiles via the same cached path):
## cv_gfi <- concurve::curve_stan_fit("normal_gfd", data = sdat, parameter = "mu",
##                                    chains = 4, iter = 22000, warmup = 2000, seed = 1859)


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

## rstan::optimizing() reports the log density on the constrained scale
## without the Jacobian adjustment, i.e. the log-likelihood here.
stan_profile_loglik <- function(mod, y, mu_grid) {
  vapply(mu_grid, function(m) {
    o <- rstan::optimizing(
      mod, data = list(N = length(y), y = y, mu_fixed = m),
      seed = 1, hessian = FALSE, verbose = FALSE
    )
    o$value
  }, numeric(1))
}

mu_grid <- seq(ybar - 5 * se, ybar + 5 * se, length.out = 121)
lp_prof <- stan_profile_loglik(prof_mod, y, mu_grid)

mle_opt <- rstan::optimizing(mle_mod, data = sdat, seed = 1, hessian = FALSE, verbose = FALSE)
lp_max  <- mle_opt$value
mu_hat  <- unname(mle_opt$par["mu"])

## Signed root of the likelihood ratio; the CD is Phi(r).
r_signed    <- sign(mu_grid - mu_hat) * sqrt(pmax(0, 2 * (lp_max - lp_prof)))
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

## (b') curve_stan() must agree with the hand-rolled CD at the same level.
row95 <- cv_gfi[[1]][which.min(abs(cv_gfi[[1]]$intrvl.level - 0.95)), ]
stopifnot(max(abs(c(row95$lower.limit, row95$upper.limit) - ci_gfi)) < 1e-6)

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
            identical(names(cv_gfi[[1]]),
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
  ggplot2::ggsave("dev/consonance_stan.png", p, width = 7, height = 4.5, dpi = 150)
  cat("\nWrote dev/consonance_stan.png\n")
}

## Package-level plots of the same object:
##   concurve::ggcurve(cv_gfi[[1]], type = "c")
##   concurve::ggcurve(cv_gfi[[2]], type = "cd")
