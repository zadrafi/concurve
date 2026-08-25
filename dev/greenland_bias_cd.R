## =====================================================================
## Greenland-type sensitivity (bias) analysis on confidence distributions
##
##   Tier 1  Deterministic external adjustment (Greenland & Lash):
##           fixed bias parameters -> a FAMILY of consonance curves.
##           These remain genuine confidence distributions, conditional
##           on the assumed bias values.
##
##   Tier 2  Probabilistic bias analysis / Monte-Carlo sensitivity
##           analysis (Greenland 2005; Lash, Fox & MacLehose):
##           priors on bias parameters, bias-corrected estimate plus
##           resampled random error.
##
##   Tier 3  Joint bias model in Stan (the "multiple-bias modelling"
##           of Greenland 2005, JRSS-A 168:267-306): misclassification
##           in the likelihood, confounding as an unidentified offset,
##           priors on all bias parameters, sampled jointly.
##
## HONESTY LABEL, printed by the script and to be kept in any write-up:
## Tier 1 curves are CDs given assumptions. Tiers 2-3 average over a
## bias prior; their coverage statements are prior-averaged, and because
## the bias parameters are NOT identified by the data, the prior's
## influence does NOT vanish as n grows. These are posterior-type
## objects wearing consonance clothing. Label them as such.
##
## Setting: unmatched case-control 2x2, odds ratio.
## Counts below are ILLUSTRATIVE, patterned after the resins-lung
## cancer example used in the multiple-bias literature. Verify against
## Greenland (2005) before quoting them as that study's data.
## =====================================================================

library(cmdstanr)
if (nzchar(Sys.getenv("CMDSTAN"))) set_cmdstan_path(Sys.getenv("CMDSTAN"))
set.seed(1965)

a <- 45; b <- 94    # exposed / unexposed cases
c0 <- 257; d <- 945 # exposed / unexposed controls
M1 <- a + b; M0 <- c0 + d

theta_hat <- log(a * d / (b * c0))
se_hat    <- sqrt(1/a + 1/b + 1/c0 + 1/d)

## Crude consonance function (Wald CD for the log OR).
H_crude <- function(th) pnorm((th - theta_hat) / se_hat)
p_fun   <- function(H) function(th) 2 * pmin(H(th), 1 - H(th))

cat(sprintf("Crude OR %.3f, 95%% CI [%.3f, %.3f]\n",
            exp(theta_hat), exp(theta_hat - 1.96 * se_hat),
            exp(theta_hat + 1.96 * se_hat)))


## ---------------------------------------------------------------------
## Tier 1 -- deterministic external adjustment
## ---------------------------------------------------------------------

## Unmeasured binary confounder Z with prevalence pz1 among exposed,
## pz0 among unexposed, and confounder-disease odds ratio or_cd.
## Bias factor (Greenland & Lash, Modern Epidemiology 3e, ch. 19):
bias_factor <- function(pz1, pz0, or_cd) {
  (pz1 * (or_cd - 1) + 1) / (pz0 * (or_cd - 1) + 1)
}

## The adjusted consonance curve is the crude curve translated by
## log(B): H_adj(theta) = H_crude(theta + log B). A shift, nothing more.
H_adjusted <- function(pz1, pz0, or_cd) {
  lb <- log(bias_factor(pz1, pz0, or_cd))
  function(th) H_crude(th + lb)
}

scenarios <- expand.grid(
  or_cd = c(2, 4, 8),
  pz1   = c(0.3, 0.5, 0.7),
  pz0   = c(0.1, 0.2)
)
scenarios$B <- with(scenarios, bias_factor(pz1, pz0, or_cd))
scenarios$or_adj <- exp(theta_hat) / scenarios$B
scenarios$lcl_adj <- exp(theta_hat - 1.96 * se_hat) / scenarios$B

cat("\n== Tier 1: worst rows of the scenario grid ==\n")
print(scenarios[order(-scenarios$B), ][1:5, ], row.names = FALSE, digits = 3)

## E-value (VanderWeele & Ding): the minimum joint confounder strength,
## on the OR scale (rare outcome), that could fully explain the estimate.
evalue <- function(rr) { rr <- max(rr, 1 / rr); rr + sqrt(rr * (rr - 1)) }
cat(sprintf("\nE-value for the estimate: %.2f;  for the lower 95%% limit: %.2f\n",
            evalue(exp(theta_hat)), evalue(exp(theta_hat - 1.96 * se_hat))))


## ---------------------------------------------------------------------
## Tier 2 -- probabilistic bias analysis (MCSA)
## ---------------------------------------------------------------------

## Bias priors. In Greenland's usage these come from validation studies
## or elicitation and must be DEFENDED, not defaulted. These are demo
## values.
##   Se, Sp   : nondifferential exposure misclassification
##   pz1, pz0 : confounder prevalences
##   or_cd    : confounder-disease OR
r_bias <- function(k) data.frame(
  se    = rbeta(k, 76, 4),                     # ~0.95 (0.89, 0.99)
  sp    = rbeta(k, 96, 4),                     # ~0.96 (0.91, 0.99)
  pz1   = rbeta(k, 5, 5),
  pz0   = rbeta(k, 2, 8),
  or_cd = rlnorm(k, log(3), 0.35)
)

## Closed-form misclassification correction of a 2x2 margin:
## observed exposed = Se*E + (1-Sp)*(N-E)  =>  E = (obs-(1-Sp)N)/(Se+Sp-1)
correct_counts <- function(obs, N, se, sp) (obs - (1 - sp) * N) / (se + sp - 1)

mcsa <- function(k = 5e4) {
  bp <- r_bias(k)
  A <- correct_counts(a,  M1, bp$se, bp$sp)
  C <- correct_counts(c0, M0, bp$se, bp$sp)
  ok <- A > 0 & A < M1 & C > 0 & C < M0        # impossible corrections
  th  <- log(A * (M0 - C) / ((M1 - A) * C)) - log(bias_factor(bp$pz1, bp$pz0, bp$or_cd))
  sev <- sqrt(1/A + 1/(M1 - A) + 1/C + 1/(M0 - C))
  th_tot <- th[ok] - rnorm(sum(ok)) * sev[ok]  # bias removed, random error restored
  list(draws = th_tot, prop_rejected = mean(!ok))
}
m2 <- mcsa()
H_mcsa <- ecdf(m2$draws)

cat(sprintf("\n== Tier 2: MCSA ==\nrejected (impossible correction): %.1f%%\n",
            100 * m2$prop_rejected))
cat(sprintf("median OR %.3f, 95%% simulation interval [%.3f, %.3f]\n",
            exp(median(m2$draws)),
            exp(quantile(m2$draws, .025)), exp(quantile(m2$draws, .975))))


## ---------------------------------------------------------------------
## Tier 3 -- joint bias model in Stan
## ---------------------------------------------------------------------

## Misclassification sits in the likelihood; the confounder acts as an
## unidentified offset on the log OR. All bias priors are passed as
## data so the same compiled model serves both the bias analysis and
## the no-bias validation run.
bias_code <- "
data {
  int a; int M1;                 // exposed cases, total cases
  int c; int M0;                 // exposed controls, total controls
  vector[2] pr_se; vector[2] pr_sp;       // beta priors
  vector[2] pr_pz1; vector[2] pr_pz0;     // beta priors
  vector[2] pr_orcd;                      // lognormal (mu, sd)
}
parameters {
  real<lower=0, upper=1> p1;     // true exposure prevalence, cases
  real<lower=0, upper=1> p0;     // true exposure prevalence, controls
  real<lower=0.5, upper=1> se;   // >0.5 keeps the correction identified in sign
  real<lower=0.5, upper=1> sp;
  real<lower=0, upper=1> pz1;
  real<lower=0, upper=1> pz0;
  real<lower=0> or_cd;
}
model {
  se    ~ beta(pr_se[1],  pr_se[2]);
  sp    ~ beta(pr_sp[1],  pr_sp[2]);
  pz1   ~ beta(pr_pz1[1], pr_pz1[2]);
  pz0   ~ beta(pr_pz0[1], pr_pz0[2]);
  or_cd ~ lognormal(pr_orcd[1], pr_orcd[2]);
  p1 ~ beta(0.5, 0.5);           // Jeffreys on the identified part
  p0 ~ beta(0.5, 0.5);
  a ~ binomial(M1, se * p1 + (1 - sp) * (1 - p1));
  c ~ binomial(M0, se * p0 + (1 - sp) * (1 - p0));
}
generated quantities {
  real theta_crude = logit(p1) - logit(p0);
  real theta_adj = theta_crude
    - log( (pz1 * (or_cd - 1) + 1) / (pz0 * (or_cd - 1) + 1) );
}
"
mod <- cmdstan_model(write_stan_file(bias_code))

sdat_bias <- list(a = a, M1 = M1, c = c0, M0 = M0,
                  pr_se = c(76, 4), pr_sp = c(96, 4),
                  pr_pz1 = c(5, 5), pr_pz0 = c(2, 8),
                  pr_orcd = c(log(3), 0.35))
fit3 <- mod$sample(data = sdat_bias, seed = 1965, chains = 4,
                   parallel_chains = 4, iter_warmup = 2000,
                   iter_sampling = 10000, refresh = 0, show_messages = FALSE)
th3 <- as.numeric(fit3$draws("theta_adj"))
H_stan <- ecdf(th3)

cat(sprintf("\n== Tier 3: Stan joint bias model ==\nmedian OR %.3f, 95%% interval [%.3f, %.3f]\n",
            exp(median(th3)), exp(quantile(th3, .025)), exp(quantile(th3, .975))))


## ---------------------------------------------------------------------
## Validation -- every moving part against an independent reference
## ---------------------------------------------------------------------

cat("\n== Validation ==\n")

## (a) Tier-1 shift identity: adjusting the curve IS translating it.
th_grid <- seq(theta_hat - 4 * se_hat, theta_hat + 4 * se_hat, length.out = 101)
Hs <- H_adjusted(0.5, 0.2, 4)
d1 <- max(abs(Hs(th_grid) - H_crude(th_grid + log(bias_factor(0.5, 0.2, 4)))))
cat(sprintf("Tier-1 shift identity                 : %.2e\n", d1)); stopifnot(d1 == 0)

## (b) E-value identity: by definition E solves B_max = RR where
##     B_max = E^2 / (2E - 1) (Ding & VanderWeele bounding factor).
E <- evalue(exp(theta_hat))
d2 <- abs(E^2 / (2 * E - 1) - exp(theta_hat))
cat(sprintf("E-value bounding-factor identity      : %.2e\n", d2)); stopifnot(d2 < 1e-10)

## (c) Misclassification round trip: misclassify expected counts, then
##     correct; must recover the truth exactly.
E_true <- 300; N <- 1000; se0 <- 0.9; sp0 <- 0.93
obs <- se0 * E_true + (1 - sp0) * (N - E_true)
d3 <- abs(correct_counts(obs, N, se0, sp0) - E_true)
cat(sprintf("misclassification round trip          : %.2e\n", d3)); stopifnot(d3 < 1e-9)

## (d) Stan model, bias switched off (priors concentrated at Se=Sp=1,
##     OR_cd=1): theta_adj must reproduce the crude Wald CD.
sdat_null <- list(a = a, M1 = M1, c = c0, M0 = M0,
                  pr_se = c(2e4, 1), pr_sp = c(2e4, 1),
                  pr_pz1 = c(5, 5), pr_pz0 = c(2, 8),
                  pr_orcd = c(0, 1e-3))
fit0 <- mod$sample(data = sdat_null, seed = 1965, chains = 4,
                   parallel_chains = 4, iter_warmup = 2000,
                   iter_sampling = 10000, refresh = 0, show_messages = FALSE)
th0 <- as.numeric(fit0$draws("theta_adj"))
q_stan <- quantile(th0, c(.025, .5, .975))
q_wald <- theta_hat + qnorm(c(.025, .5, .975)) * se_hat
cat("Stan (bias off) vs Wald CD quantiles  :\n")
print(round(rbind(stan = q_stan, wald = q_wald), 4))
stopifnot(max(abs(q_stan - q_wald)) < 0.03)     # large-sample + MC tolerance

## (e) Tier 2 vs Tier 3. Greenland's warning: MCSA approximates the
##     joint Bayesian answer, and the approximation can fail with
##     sparse data or data-prior conflict. Report, don't assert.
cat(sprintf("Tier 2 vs Tier 3 medians (log OR)     : %.3f vs %.3f\n",
            median(m2$draws), median(th3)))
cat(sprintf("Tier 2 vs Tier 3 interval widths      : %.3f vs %.3f\n",
            diff(quantile(m2$draws, c(.025, .975))), diff(quantile(th3, c(.025, .975)))))


## ---------------------------------------------------------------------
## Plot: the whole sensitivity analysis on one pair of axes
## ---------------------------------------------------------------------

if (requireNamespace("ggplot2", quietly = TRUE)) {
  og <- exp(seq(log(0.5), log(4), length.out = 400)); lg <- log(og)
  fam <- do.call(rbind, lapply(seq_len(nrow(scenarios)), function(i) {
    Hi <- H_adjusted(scenarios$pz1[i], scenarios$pz0[i], scenarios$or_cd[i])
    data.frame(or = og, p = p_fun(Hi)(lg), grp = paste0("s", i))
  }))
  main <- rbind(
    data.frame(or = og, p = p_fun(H_crude)(lg), curve = "Crude (conventional)"),
    data.frame(or = og, p = p_fun(H_mcsa)(lg),  curve = "Tier 2: MCSA"),
    data.frame(or = og, p = p_fun(H_stan)(lg),  curve = "Tier 3: Stan joint bias model")
  )
  p <- ggplot2::ggplot() +
    ggplot2::geom_line(data = fam, ggplot2::aes(or, p, group = grp),
                       colour = "grey70", linewidth = 0.3) +
    ggplot2::geom_line(data = main, ggplot2::aes(or, p, colour = curve),
                       linewidth = 0.8) +
    ggplot2::geom_vline(xintercept = 1, linetype = 3) +
    ggplot2::scale_x_log10() +
    ggplot2::labs(x = "Odds ratio", y = "p-value / consonance",
                  colour = NULL,
                  subtitle = "Grey: Tier-1 fixed-bias consonance curves (18 scenarios)") +
    ggplot2::theme_bw() + ggplot2::theme(legend.position = "bottom")
  ggplot2::ggsave("bias_consonance.png", p, width = 7.5, height = 5, dpi = 150)
  cat("\nWrote bias_consonance.png\n")
}

cat("\nREMINDER: Tier 1 = confidence statements conditional on assumed bias\n")
cat("values. Tiers 2-3 = prior-averaged; not CDs in the strict sense, and\n")
cat("the bias prior never washes out because nothing identifies it.\n")
