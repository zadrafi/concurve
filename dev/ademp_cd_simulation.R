## =====================================================================
## ADEMP simulation study: evaluating confidence-distribution methods
##
## Structure follows Morris, White & Crowther (2019), "Using simulation
## studies to evaluate statistical methods", Statistics in Medicine
## 38:2074-2102: Aims, Data-generating mechanisms, Estimands, Methods,
## Performance measures -- declared BEFORE results, with a Monte Carlo
## SE attached to every performance estimate. A performance number
## without an MCSE is itself an estimate reported without uncertainty,
## which would be a strange look for this project in particular.
##
## Performance measures are computed twice: by hand from the Morris
## et al. formulas, and by rsimsum (Gasparini), and asserted equal.
##
## ---------------------------------------------------------------------
## THE ADEMP DECLARATION (frozen before the first run; changes after
## first results are amendments and must be logged as such)
##
## A  Aims. Estimate finite-sample calibration (coverage), bias,
##    precision, and interval width of four confidence-distribution
##    constructors for a population mean, under a correctly specified
##    and two misspecified error distributions; quantify the
##    undercoverage of first-order methods at small n.
##
## D  Data-generating mechanisms. Fully factorial:
##      error distribution: (i) standard normal;
##                          (ii) standardized lognormal (skewness ~6);
##                          (iii) standardized t3 (heavy tails);
##      all scaled to mean 0, SD 1 so the estimand is fixed;
##      n in {10, 30, 100}.
##
## E  Estimand. The population mean, theta = 0.
##
## M  Methods (each returns the CD median as point estimate and the
##    central 95% interval):
##      exact_t     exact-t CD. Identical to the Stan generalized
##                  fiducial distribution for the normal model
##                  (validated in stan_confidence_distribution.R);
##                  the analytic form is substituted for runtime.
##      signedroot  first-order signed-root likelihood-ratio CD under
##                  the normal working model.
##      wald        z-based Wald CD with the sample SD.
##      boot_pct    bootstrap percentile CD, B = 1000 (Efron's
##                  bootstrap-distribution-as-approximate-CD).
##
## P  Performance measures, each with MCSE (Morris et al., Table 6):
##      coverage of the 95% interval; bias of the point estimate;
##      empirical SE; mean interval width.
##    n_sim = 2000 per DGM cell, chosen so coverage MCSE <= 0.5
##    percentage points at true coverage 0.95
##    (sqrt(.95*.05/2000) = 0.0049).
##
## Seed policy: one master seed; cell-level substreams derived from it
## and recorded in the results object.
## =====================================================================

n_sim <- 2000
B     <- 1000
theta <- 0
master_seed <- 20260825

## Standardized error generators (mean 0, SD 1 by construction).
dgms <- list(
  normal    = function(n) rnorm(n),
  lognormal = function(n) (rlnorm(n) - exp(0.5)) / sqrt((exp(1) - 1) * exp(1)),
  t3        = function(n) rt(n, 3) / sqrt(3)
)

## Methods: each returns c(est, lower, upper).
methods <- list(
  exact_t = function(y) {
    n <- length(y); m <- mean(y); se <- sd(y) / sqrt(n)
    c(m, m + se * qt(c(.025, .975), n - 1), se)
  },
  signedroot = function(y) {
    n <- length(y); m <- mean(y)
    H <- function(t) {
      s2m <- vapply(t, function(v) mean((y - v)^2), numeric(1))
      pnorm(sign(t - m) * sqrt(pmax(0, n * log(s2m / mean((y - m)^2)))))
    }
    Q <- function(p) vapply(p, function(pp)
      uniroot(function(v) H(v) - pp,
              interval = m + c(-60, 60) * sd(y) / sqrt(n))$root, numeric(1))
    q <- Q(c(.5, .025, .975)); c(q, sd(y) / sqrt(n))
  },
  wald = function(y) {
    n <- length(y); m <- mean(y); se <- sd(y) / sqrt(n)
    c(m, m + se * qnorm(c(.025, .975)), se)
  },
  boot_pct = function(y) {
    n <- length(y)
    mb <- colMeans(matrix(sample(y, n * B, replace = TRUE), nrow = n))
    c(unname(quantile(mb, c(.5, .025, .975), type = 8)), sd(mb))
  }
)

## ---------------------------------------------------------------------
## Run
## ---------------------------------------------------------------------

cells <- expand.grid(dgm = names(dgms), n = c(10, 30, 100),
                     stringsAsFactors = FALSE)
res <- vector("list", nrow(cells) * n_sim * length(methods)); ri <- 0L

t0 <- Sys.time()
for (ci in seq_len(nrow(cells))) {
  gen <- dgms[[cells$dgm[ci]]]; n <- cells$n[ci]
  cell_seed <- master_seed + ci          # recorded substream
  set.seed(cell_seed)
  for (r in seq_len(n_sim)) {
    y <- gen(n)
    for (mth in names(methods)) {
      v <- methods[[mth]](y); ri <- ri + 1L
      res[[ri]] <- data.frame(dgm = cells$dgm[ci], n = n, seed = cell_seed,
                              rep = r, method = mth,
                              b = v[1], lo = v[2], hi = v[3], se = v[4])
    }
  }
}
res <- do.call(rbind, res)
cat(sprintf("simulation: %d estimates in %.1f s\n",
            nrow(res), as.numeric(difftime(Sys.time(), t0, units = "secs"))))

## ---------------------------------------------------------------------
## Performance measures with MCSE (hand-computed, Morris et al. Table 6)
## ---------------------------------------------------------------------

perf_hand <- do.call(rbind, lapply(split(res, list(res$dgm, res$n, res$method)),
  function(d) {
    ns  <- nrow(d)
    cov <- mean(d$lo <= theta & theta <= d$hi)
    ese <- sd(d$b)
    data.frame(
      dgm = d$dgm[1], n = d$n[1], method = d$method[1],
      bias      = mean(d$b) - theta,  bias_mcse  = ese / sqrt(ns),
      empse     = ese,                  empse_mcse = ese / sqrt(2 * (ns - 1)),
      coverage  = cov,                  cov_mcse   = sqrt(cov * (1 - cov) / ns),
      width     = mean(d$hi - d$lo),    width_mcse = sd(d$hi - d$lo) / sqrt(ns)
    )
  }))
rownames(perf_hand) <- NULL

## ---------------------------------------------------------------------
## Cross-validation against rsimsum
## ---------------------------------------------------------------------

cat("\n== Validation against rsimsum ==\n")
stopifnot(requireNamespace("rsimsum", quietly = TRUE))
ss <- rsimsum::simsum(
  data = res, estvarname = "b", se = "se", true = theta,
  methodvar = "method", by = c("dgm", "n"),
  ci.limits = c("lo", "hi"), x = TRUE
)
sm <- rsimsum::tidy(summary(ss))

chk <- merge(
  perf_hand,
  reshape(sm[sm$stat %in% c("bias", "empse", "cover"),
             c("dgm", "n", "method", "stat", "est", "mcse")],
          idvar = c("dgm", "n", "method"), timevar = "stat",
          direction = "wide"),
  by = c("dgm", "n", "method")
)
d_bias <- max(abs(chk$bias - chk$est.bias), abs(chk$bias_mcse - chk$mcse.bias))
d_emp  <- max(abs(chk$empse - chk$est.empse), abs(chk$empse_mcse - chk$mcse.empse))
d_cov  <- max(abs(chk$coverage - chk$est.cover), abs(chk$cov_mcse - chk$mcse.cover))
cat(sprintf("bias & MCSE     : %.2e\nempSE & MCSE    : %.2e\ncoverage & MCSE : %.2e\n",
            d_bias, d_emp, d_cov))
stopifnot(d_bias < 1e-10, d_emp < 1e-10, d_cov < 1e-10)

## ---------------------------------------------------------------------
## Results
## ---------------------------------------------------------------------

cat("\n== Coverage of nominal 95% intervals (MCSE ~ 0.005-0.011) ==\n")
tab <- reshape(perf_hand[, c("dgm", "n", "method", "coverage")],
               idvar = c("dgm", "n"), timevar = "method", direction = "wide")
names(tab) <- sub("coverage\\.", "", names(tab))
tab <- tab[order(tab$dgm, tab$n), c("dgm", "n", "exact_t", "signedroot", "wald", "boot_pct")]
print(tab, row.names = FALSE, digits = 3)

write.csv(res,       "ademp_estimates.csv",  row.names = FALSE)
write.csv(perf_hand, "ademp_performance.csv", row.names = FALSE)

if (requireNamespace("ggplot2", quietly = TRUE)) {
  z <- try(ggplot2::autoplot(ss, type = "zip"), silent = TRUE)
  if (!inherits(z, "try-error"))
    ggplot2::ggsave("ademp_zip.png", z + ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "bottom"), width = 10, height = 8, dpi = 150)

  pd <- perf_hand
  pd$n <- factor(pd$n)
  g <- ggplot2::ggplot(pd, ggplot2::aes(n, coverage, colour = method, group = method)) +
    ggplot2::geom_hline(yintercept = 0.95, linetype = 3) +
    ggplot2::geom_pointrange(ggplot2::aes(ymin = coverage - 1.96 * cov_mcse,
                                          ymax = coverage + 1.96 * cov_mcse),
                             position = ggplot2::position_dodge(width = .5), size = .3) +
    ggplot2::facet_wrap(~dgm) +
    ggplot2::labs(y = "coverage (95% MC interval)", colour = NULL) +
    ggplot2::theme_bw() + ggplot2::theme(legend.position = "bottom")
  ggplot2::ggsave("ademp_coverage.png", g, width = 9, height = 4, dpi = 150)
  cat("\nWrote ademp_zip.png, ademp_coverage.png, ademp_estimates.csv, ademp_performance.csv\n")
}
