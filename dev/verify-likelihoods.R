# verify-likelihoods.R
# -----------------------------------------------------------------------------
# Standalone validation for the likelihood constructions in
# vignettes/likelihood-constructions.Rmd.
#
# Base R only -- no concurve, no NAMESPACE dependency. Each block rebuilds a
# likelihood and asserts that its maximum lands on the estimator the design
# defines. Run with:  Rscript verify-likelihoods.R
# -----------------------------------------------------------------------------

peak_of <- function(values, loglik) values[which.max(loglik)]

check <- function(label, got, want, tol) {
  ok <- abs(got - want) <= tol
  cat(sprintf("%-28s got=%.5f  want=%.5f  %s\n",
              label, got, want, if (ok) "PASS" else "FAIL"))
  if (!ok) stop(sprintf("Assertion failed: %s", label), call. = FALSE)
  invisible(TRUE)
}

# 1. Single proportion: exact binomial peak = x/n --------------------------------
x <- 8; n <- 20
p <- seq(1e-4, 1 - 1e-4, length.out = 4000)
loglik_binom <- x * log(p) + (n - x) * log(1 - p)
check("proportion (binomial)", peak_of(p, loglik_binom), x / n, 1e-2)

# 2. Odds ratio: exact conditional (noncentral hypergeometric) -------------------
# Peak = conditional MLE, which is close to but NOT ad/bc. We assert the
# conditional MLE solves the score equation E[A; psi] = a.
a <- 12; b <- 8; c <- 5; d <- 15
n1 <- a + b; n0 <- c + d; m1 <- a + c
klo <- max(0, m1 - n0); khi <- min(m1, n1); ks <- klo:khi

cond_loglik_or <- function(logpsi) {
  logterms <- lchoose(n1, ks) + lchoose(n0, m1 - ks) + ks * logpsi
  M <- max(logterms)
  a * logpsi - (M + log(sum(exp(logterms - M))))
}
logpsi <- seq(-3, 4, length.out = 20000)
loglik_or <- vapply(logpsi, cond_loglik_or, numeric(1))
psi_hat <- exp(peak_of(logpsi, loglik_or))

# noncentral hypergeometric mean at psi_hat should equal observed a
nchg_mean <- function(psi) {
  w <- lchoose(n1, ks) + lchoose(n0, m1 - ks) + ks * log(psi)
  w <- exp(w - max(w)); w <- w / sum(w)
  sum(ks * w)
}
check("odds ratio E[A;psi]=a", nchg_mean(psi_hat), a, 1e-2)
cat(sprintf("   (conditional MLE psi=%.3f vs naive ad/bc=%.3f -- expected to differ)\n",
            psi_hat, (a * d) / (b * c)))

# 3. Rate ratio: exact conditional binomial peak = sample RR ---------------------
a_e <- 30; T1 <- 1000; b_e <- 18; T0 <- 1200
cond_loglik_rr <- function(logtheta) {
  pi <- exp(logtheta) * T1 / (exp(logtheta) * T1 + T0)
  a_e * log(pi) + b_e * log(1 - pi)
}
logtheta <- seq(-2, 3, length.out = 20000)
loglik_rr <- vapply(logtheta, cond_loglik_rr, numeric(1))
check("rate ratio (conditional)",
      exp(peak_of(logtheta, loglik_rr)), (a_e / T1) / (b_e / T0), 1e-2)

# 4. Mean: profile likelihood peak = xbar ----------------------------------------
xs <- c(5.1, 4.8, 5.5, 4.9, 5.3, 5.0, 4.7, 5.2, 5.4, 4.6)
nx <- length(xs); xbar <- mean(xs); ss_min <- sum((xs - xbar)^2)
mu <- seq(4.4, 5.8, length.out = 4000)
loglik_mu <- -(nx / 2) * log(vapply(mu, function(m) sum((xs - m)^2), numeric(1)) / ss_min)
check("mean (profile likelihood)", peak_of(mu, loglik_mu), xbar, 1e-2)

# 5. Variance: chi-square likelihood peak = s^2 ----------------------------------
s2 <- var(xs); nu <- nx - 1
sig2 <- seq(0.02, 0.60, length.out = 4000)
loglik_var <- -(nu / 2) * log(sig2) - nu * s2 / (2 * sig2)
check("variance (chi-square)", peak_of(sig2, loglik_var), s2, 1e-2)

cat("\nAll likelihood constructions verified.\n")
