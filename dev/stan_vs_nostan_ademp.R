## =====================================================================
## ADEMP study: Stan-based vs non-Stan confidence-distribution methods
##
## A  Aims. Decide when the Stan machinery earns its compute cost for
##    constructing CDs. Three sharp sub-questions:
##      (i)   same construction, two engines: does Stan's MCMC version
##            of a CD differ in coverage from the identical analytic
##            Monte Carlo version? (It must not; asserted.)
##      (ii)  correct parametric model, awkward estimand (lognormal
##            MEAN, exp(mu + sigma^2/2)): do Stan's general-purpose
##            routes match the best analytic parametric method, and do
##            both beat generic nonparametric constructors?
##      (iii) wrong parametric model (gamma data, lognormal working
##            model): does Stan rescue anything?
##
## D  DGMs. x > 0, estimand = population mean.
##      lognormal : x ~ LN(0,1),  theta = exp(0.5) = 1.6487  (model right)
##      gamma     : x ~ Ga(2,1),  theta = 2                  (model wrong:
##                  the lognormal working model targets
##                  exp(digamma(2) + trigamma(2)/2) ~ 2.107, a 5% bias
##                  that no amount of sampling can remove)
##      n in {10, 50}.
##
## E  Estimand. The population mean; nominal 95% central region.
##
## M  Methods.
##    Stan-based (lognormal working model on log x):
##      stan_gfd   GFD sampled by HMC: target += lik - log(sigma);
##                 theta draws = exp(mu + sigma^2/2)
##      stan_prof  profile signed-root via Stan's optimiser (coverage by
##                 membership: profile deviance at truth vs chi^2_1)
##    Non-Stan:
##      fid        the SAME GFD drawn analytically (sigma^2 from the
##                 chi-square, mu conjugate-style) -- engine control
##      cox        Cox's method: log-scale Wald with
##                 var = s^2/n + s^4/(2(n-1))
##      wald       raw-scale mean +/- 1.96 sd/sqrt(n)
##      boot       percentile bootstrap of the raw mean, B = 600
##      el         empirical likelihood, F-calibrated (membership)
##
## P  Performance. Coverage (+MCSE), median 95% width where the method
##    yields limits directly (stan_prof reported coverage-only: interval
##    inversion costs ~12 optimiser process launches per replicate and
##    adds nothing to the coverage question), median seconds/dataset.
##    n_sim = 400 per cell (coverage MCSE ~ 0.011). Cell seeds recorded.
##
## ---------------------------------------------------------------------
## Provenance: this script produced dev/stan_vs_nostan.csv and
## dev/stan_vs_nostan.png, which were committed in f1fc3e6 while the
## script itself was left unsaved in the editor. Recovered 2026-09-04.
##
## Requires cmdstanr and a CmdStan installation. cmdstanr is not on CRAN
## and is NOT a concurve dependency -- this is a dev/ study, not package
## code, and nothing here is run by R CMD check. The package's own Stan
## support uses rstan; see dev/stan_confidence_distribution.R.
##
## Runtime is substantial: 4 cells x 400 replicates, each fitting one
## HMC model and launching two optimiser processes.
## =====================================================================

suppressPackageStartupMessages(library(cmdstanr))
if (nzchar(Sys.getenv("CMDSTAN"))) set_cmdstan_path(Sys.getenv("CMDSTAN"))

## Outputs go into dev/, not the package root. Run this from the package
## root; the earlier version of this script wrote to the working
## directory, which is how stan_vs_nostan.csv and .png ended up loose at
## the top level and had to be moved in c9c7c14.
OUT <- if (dir.exists("dev")) "dev" else "."
out_path <- function(f) file.path(OUT, f)

n_sim <- 400; B <- 600; master <- 20260903

gfd_code <- "
data { int<lower=1> N; vector[N] w; }
parameters { real mu; real<lower=0> sigma; }
model { target += normal_lpdf(w | mu, sigma); target += -log(sigma); }
generated quantities { real theta = exp(mu + square(sigma)/2); }
"
prof_code <- "
data { int<lower=1> N; vector[N] w; real theta0; }
parameters { real<lower=0> sigma; }
transformed parameters { real mu = log(theta0) - square(sigma)/2; }
model { target += normal_lpdf(w | mu, sigma); }
"
mle_code <- "
data { int<lower=1> N; vector[N] w; }
parameters { real mu; real<lower=0> sigma; }
model { target += normal_lpdf(w | mu, sigma); }
"
m_gfd  <- cmdstan_model(write_stan_file(gfd_code))
m_prof <- cmdstan_model(write_stan_file(prof_code))
m_mle  <- cmdstan_model(write_stan_file(mle_code))

el_stat <- function(x, theta) {
  z <- x - theta; n <- length(x)
  if (min(z) >= 0 || max(z) <= 0) return(Inf)
  lam <- uniroot(function(l) sum(z/(1+l*z)),
                 c((1/n-1)/max(z)+1e-12, (1/n-1)/min(z)-1e-12), tol=1e-12)$root
  2*sum(log1p(lam*z))
}

## Analytic GFD for the lognormal mean (engine control for stan_gfd).
fid_draws <- function(w, K = 4000) {
  n <- length(w); mh <- mean(w); s2 <- var(w)
  sig2 <- (n-1)*s2 / rchisq(K, n-1)
  mu   <- mh + sqrt(sig2/n)*rnorm(K)
  exp(mu + sig2/2)
}

one_rep <- function(dgm, n, theta_true) {
  x <- if (dgm == "lognormal") rlnorm(n) else rgamma(n, 2, 1)
  w <- log(x); out <- list(); tm <- list()

  tm$stan_gfd <- system.time({
    f <- m_gfd$sample(data = list(N=n, w=w), seed=1, chains=2,
                      iter_warmup=500, iter_sampling=1250, refresh=0,
                      show_messages=FALSE, show_exceptions=FALSE)
    th <- as.numeric(f$draws("theta"))
    q <- quantile(th, c(.025,.975), names=FALSE)
    out$stan_gfd <<- c(hit = q[1] <= theta_true && theta_true <= q[2],
                       width = q[2]-q[1])
  })["elapsed"]

  tm$stan_prof <- system.time({
    lmax <- m_mle$optimize(data=list(N=n,w=w), jacobian=FALSE, seed=1,
                           refresh=0, show_messages=FALSE)$lp()
    lp0  <- m_prof$optimize(data=list(N=n,w=w,theta0=theta_true),
                            jacobian=FALSE, seed=1, refresh=0,
                            show_messages=FALSE)$lp()
    out$stan_prof <<- c(hit = 2*(lmax-lp0) <= qchisq(.95,1), width = NA)
  })["elapsed"]

  tm$fid <- system.time({
    th <- fid_draws(w)
    q <- quantile(th, c(.025,.975), names=FALSE)
    out$fid <<- c(hit = q[1] <= theta_true && theta_true <= q[2],
                  width = q[2]-q[1])
  })["elapsed"]

  tm$cox <- system.time({
    mh <- mean(w); s2 <- var(w)
    est <- mh + s2/2; se <- sqrt(s2/n + s2^2/(2*(n-1)))
    q <- exp(est + c(-1,1)*qnorm(.975)*se)
    out$cox <<- c(hit = q[1] <= theta_true && theta_true <= q[2],
                  width = q[2]-q[1])
  })["elapsed"]

  tm$wald <- system.time({
    q <- mean(x) + c(-1,1)*qnorm(.975)*sd(x)/sqrt(n)
    out$wald <<- c(hit = q[1] <= theta_true && theta_true <= q[2],
                   width = q[2]-q[1])
  })["elapsed"]

  tm$boot <- system.time({
    mb <- colMeans(matrix(sample(x, n*B, replace=TRUE), n))
    q <- quantile(mb, c(.025,.975), names=FALSE, type=8)
    out$boot <<- c(hit = q[1] <= theta_true && theta_true <= q[2],
                   width = q[2]-q[1])
  })["elapsed"]

  tm$el <- system.time({
    s <- el_stat(x, theta_true)
    out$el <<- c(hit = s*(n-1)/n <= qf(.95,1,n-1), width = NA)
  })["elapsed"]

  data.frame(method = names(out),
             hit = vapply(out, `[`, 1, 1),
             width = vapply(out, `[`, 1, 2),
             secs = vapply(names(out), function(k) tm[[k]], 1))
}

## ---------------------------------------------------------------------
## Validation before the race: same construction, two engines
## ---------------------------------------------------------------------
cat("== Validation: stan_gfd vs analytic fid on shared datasets ==\n")
set.seed(11)
dmax <- max(vapply(1:5, function(i) {
  w <- log(rlnorm(20))
  f <- m_gfd$sample(data=list(N=20,w=w), seed=i, chains=4,
                    iter_warmup=1000, iter_sampling=10000, refresh=0,
                    show_messages=FALSE, show_exceptions=FALSE)
  qs <- quantile(as.numeric(f$draws("theta")), c(.025,.25,.5,.75,.975))
  qa <- quantile(fid_draws(w, 2e5), c(.025,.25,.5,.75,.975))
  max(abs(log(qs) - log(qa)))
}, 1))
cat(sprintf("max |log-quantile difference| over 5 datasets : %.4f\n", dmax))
stopifnot(dmax < 0.05)   # Monte Carlo tolerance on shared quantiles

## ---------------------------------------------------------------------
## Run
## ---------------------------------------------------------------------
cells <- expand.grid(dgm = c("lognormal","gamma"), n = c(10, 50),
                     stringsAsFactors = FALSE)
cells$theta <- ifelse(cells$dgm == "lognormal", exp(0.5), 2)
res <- vector("list", nrow(cells)); t0 <- Sys.time()
for (ci in seq_len(nrow(cells))) {
  set.seed(master + ci)
  rr <- do.call(rbind, replicate(n_sim,
          one_rep(cells$dgm[ci], cells$n[ci], cells$theta[ci]),
          simplify = FALSE))
  rr$dgm <- cells$dgm[ci]; rr$n <- cells$n[ci]; rr$seed <- master + ci
  res[[ci]] <- rr
  cat(sprintf("cell %d/%d done, %.1f min elapsed\n", ci, nrow(cells),
              as.numeric(difftime(Sys.time(), t0, units="mins"))))
}
res <- do.call(rbind, res)

perf <- do.call(rbind, lapply(split(res, list(res$dgm, res$n, res$method)),
  function(d) data.frame(dgm=d$dgm[1], n=d$n[1], method=d$method[1],
    coverage = mean(d$hit),
    cov_mcse = sqrt(mean(d$hit)*(1-mean(d$hit))/nrow(d)),
    med_width = median(d$width),
    med_secs = median(d$secs))))
rownames(perf) <- NULL

## engine assertion: identical construction, coverage within joint MC error
for (dg in c("lognormal","gamma")) for (nn in c(10,50)) {
  a <- perf$coverage[perf$dgm==dg & perf$n==nn & perf$method=="stan_gfd"]
  b <- perf$coverage[perf$dgm==dg & perf$n==nn & perf$method=="fid"]
  stopifnot(abs(a-b) < 4*sqrt(2)*0.011)
}
cat("\nengine check: stan_gfd and fid coverage indistinguishable in all cells\n")

cat("\n== Results (coverage MCSE ~ 0.011; width = median 95% width) ==\n")
perf <- perf[order(perf$dgm, perf$n, -perf$coverage),]
print(format(perf[, c("dgm","n","method","coverage","med_width","med_secs")],
             digits=3), row.names = FALSE)
write.csv(perf, out_path("stan_vs_nostan.csv"), row.names = FALSE)

if (requireNamespace("ggplot2", quietly = TRUE)) {
  pd <- perf; pd$engine <- ifelse(grepl("^stan", pd$method), "Stan", "no Stan")
  g <- ggplot2::ggplot(pd, ggplot2::aes(method, coverage, colour = engine)) +
    ggplot2::geom_hline(yintercept = .95, linetype = 3) +
    ggplot2::geom_pointrange(ggplot2::aes(ymin = coverage-1.96*cov_mcse,
                                          ymax = coverage+1.96*cov_mcse)) +
    ggplot2::facet_grid(n ~ dgm, labeller = ggplot2::label_both) +
    ggplot2::coord_flip() +
    ggplot2::labs(y = "coverage of nominal 95% region", x = NULL, colour = NULL) +
    ggplot2::theme_bw() + ggplot2::theme(legend.position = "bottom")
  ggplot2::ggsave(out_path("stan_vs_nostan.png"), g, width = 8, height = 5, dpi = 150)
  cat("\nWrote", out_path("stan_vs_nostan.png"), "and",
      out_path("stan_vs_nostan.csv"), "\n")
}
