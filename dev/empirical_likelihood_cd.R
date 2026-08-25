## =====================================================================
## Empirical likelihood as a nonparametric consonance constructor
##
## Owen (1988, Biometrika; 2001 book): maximise prod(p_i) subject to
## sum(p_i) = 1 and sum(p_i (x_i - theta)) = 0. The ratio R(theta)
## obeys a NONPARAMETRIC Wilks theorem, -2 log R -> chi^2_1, so
##
##     p(theta) = P(chi^2_1 > -2 log R(theta))
##
## is a consonance curve requiring no distributional model: shape
## driven by the data (asymmetric where the data are), transformation
## respecting, and Bartlett-correctable (DiCiccio, Hall & Romano 1991),
## which no other nonparametric method manages.
##
## The known costs, both demonstrated below rather than footnoted:
##   1. the CONVEX HULL constraint: R(theta) = 0 outside (min x, max x),
##      so the curve hits p = 0 in finite distance -- confidence regions
##      cannot extend past the data range, and at small n that is a
##      correctness problem, not a curiosity;
##   2. chi^2 calibration undercovers at small n; F(1, n-1) calibration
##      (Owen) is the cheap partial fix, bootstrap calibration the
##      expensive one.
##
## Verdict machinery: same ADEMP DGMs as ademp_cd_simulation.R so the
## coverage numbers are directly comparable with exact-t and bootstrap.
## =====================================================================

set.seed(1988)

## ---------------------------------------------------------------------
## 1. EL for a mean, by hand
## ---------------------------------------------------------------------

## Returns -2 log R(theta); +Inf outside the convex hull.
el_stat <- function(x, theta) {
  z <- x - theta; n <- length(x)
  if (min(z) >= 0 || max(z) <= 0) return(Inf)          # hull violated
  eps <- 1e-12
  lo <- (1 / n - 1) / max(z) + eps
  hi <- (1 / n - 1) / min(z) - eps
  lam <- uniroot(function(l) sum(z / (1 + l * z)), c(lo, hi),
                 tol = 1e-12)$root
  2 * sum(log1p(lam * z))
}

el_pvalue <- function(x, theta, calib = c("chisq", "F")) {
  calib <- match.arg(calib)
  s <- vapply(theta, function(t) el_stat(x, t), numeric(1))
  n <- length(x)
  if (calib == "chisq") pchisq(s, 1, lower.tail = FALSE)
  else pf(s * (n - 1) / n, 1, n - 1, lower.tail = FALSE)  # F calibration
}

## ---------------------------------------------------------------------
## 2. Validation
## ---------------------------------------------------------------------

cat("== Validation ==\n")
x <- rlnorm(25)

## (a) Against emplik::el.test at many theta.
stopifnot(requireNamespace("emplik", quietly = TRUE))
th_try <- seq(quantile(x, .1), quantile(x, .9), length.out = 25)
d1 <- max(abs(vapply(th_try, function(t) el_stat(x, t), 1) -
              vapply(th_try, function(t) emplik::el.test(x, mu = t)$`-2LLR`, 1)))
cat(sprintf("el_stat vs emplik::el.test                 : %.2e\n", d1))
stopifnot(d1 < 1e-6)

## (b) -2 log R = 0 at the sample mean (the NPMLE).
d2 <- el_stat(x, mean(x))
cat(sprintf("statistic at the sample mean               : %.2e\n", d2))
stopifnot(d2 < 1e-10)

## (c) Nonparametric Wilks. NOTE: the chi-square limit is asymptotic;
##     with 2000 replicates a KS test has power to detect the genuine
##     O(1/n) error at n = 200 (it did: p ~ 0.001 under lognormal), so
##     asserting KS non-rejection would be testing a hypothesis known
##     to be false. Instead: the type-I rate at the chi-square cutoff
##     must approach 0.05 as n grows.
for (nn in c(50, 200, 1000)) {
  rej <- mean(replicate(2000, el_stat(rlnorm(nn) - exp(.5), 0)) > qchisq(.95, 1))
  cat(sprintf("Wilks: P(-2logR > chisq cutoff), n = %4d  : %.3f\n", nn, rej))
  if (nn == 1000) stopifnot(abs(rej - 0.05) < 0.02)
}

## (d) Hull truncation is real: p-value exactly 0 just outside range.
d4 <- el_pvalue(x, max(x) + 1e-9)
cat(sprintf("p-value just beyond the data maximum       : %g\n", d4))
stopifnot(d4 == 0)

## ---------------------------------------------------------------------
## 3. The verdict simulation: same DGMs as the ADEMP study
## ---------------------------------------------------------------------
## Coverage by membership (statistic at the truth vs the cutoff), so no
## curve inversion is needed. n_sim = 2000; coverage MCSE ~ 0.005-0.011.

dgms <- list(
  normal    = function(n) rnorm(n),
  lognormal = function(n) (rlnorm(n) - exp(0.5)) / sqrt((exp(1) - 1) * exp(1)),
  t3        = function(n) rt(n, 3) / sqrt(3)
)
n_sim <- 2000

cat("\n== Coverage of nominal 95% regions (compare: ADEMP table) ==\n")
cat("       dgm   n  EL(chisq)  EL(F)   exact_t\n")
for (dg in names(dgms)) for (nn in c(10, 30, 100)) {
  set.seed(1988 + nn)
  hit <- t(replicate(n_sim, {
    yy <- dgms[[dg]](nn)
    s  <- el_stat(yy, 0)
    tt <- abs(mean(yy) / (sd(yy) / sqrt(nn))) <= qt(.975, nn - 1)
    c(chisq = s <= qchisq(.95, 1),
      F     = s * (nn - 1) / nn <= qf(.95, 1, nn - 1),
      t     = tt)
  }))
  cat(sprintf("%10s %4d   %6.3f   %6.3f   %6.3f\n", dg, nn,
              mean(hit[, "chisq"]), mean(hit[, "F"]), mean(hit[, "t"])))
}

## ---------------------------------------------------------------------
## 4. The picture: EL vs t consonance curves on skewed data
## ---------------------------------------------------------------------

if (requireNamespace("ggplot2", quietly = TRUE)) {
  xs <- rlnorm(20)                       # small skewed sample
  m <- mean(xs); se <- sd(xs) / sqrt(20)
  grid <- seq(m - 4.5 * se, m + 4.5 * se, length.out = 600)
  pd <- rbind(
    data.frame(theta = grid, p = el_pvalue(xs, grid, "F"),
               method = "Empirical likelihood (F-calibrated)"),
    data.frame(theta = grid,
               p = 2 * pt(-abs((grid - m) / se), 19),
               method = "Student t (parametric)")
  )
  g <- ggplot2::ggplot(pd, ggplot2::aes(theta, p, colour = method)) +
    ggplot2::geom_line(linewidth = .8) +
    ggplot2::geom_rug(data = data.frame(theta = xs), inherit.aes = FALSE,
                      ggplot2::aes(x = theta), alpha = .5) +
    ggplot2::geom_hline(yintercept = .05, linetype = 3) +
    ggplot2::labs(x = expression(theta~"(mean)"), y = "p-value", colour = NULL,
      subtitle = "Lognormal sample, n = 20: EL curve is asymmetric and dies at the data hull") +
    ggplot2::theme_bw() + ggplot2::theme(legend.position = "bottom")
  ggplot2::ggsave("el_consonance.png", g, width = 7.5, height = 4.4, dpi = 150)
  cat("\nWrote el_consonance.png\n")
}
