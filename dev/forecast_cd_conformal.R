## =====================================================================
## Confidence distributions meet modern forecasting
##
## The forecasting literature arrived at the CD destination decades
## ago under its own name: a good forecast is a PREDICTIVE DISTRIBUTION
## (Gneiting & Katzfuss 2014), judged by calibration (PIT uniformity)
## and sharpness (proper scores). A fan chart is a drapery plot for
## the future. This script wires the pieces built earlier in this
## project -- conformal predictive systems, the decision layer, the
## ADEMP habit -- into a forecasting pipeline, with the one new
## ingredient time series force on you:
##
##   Exchangeability is DEAD in time series, so plain conformal has no
##   guarantee. Adaptive Conformal Inference (Gibbs & Candes 2021,
##   NeurIPS) restores a long-run coverage guarantee under arbitrary
##   distribution shift by feeding back the miss indicator:
##       alpha_{t+1} = alpha_t + gamma * (alpha - err_t).
##
## Demonstration: retail-flavoured daily series whose noise regime
## SHIFTS halfway through evaluation, with a deliberately stale
## forecaster -- the realistic production failure. Three interval
## methods ride the same point forecasts:
##   model     Gaussian intervals, training residual SD (what most
##             pipelines ship)
##   split     static split conformal (valid if the world holds still)
##   aci       adaptive conformal on a trailing window
## =====================================================================

set.seed(1148)
suppressPackageStartupMessages(library(forecast))

## ---------------------------------------------------------------------
## 1. Data: trend + weekly seasonality + a variance regime shift
## ---------------------------------------------------------------------

T_all <- 1400; t_ax <- seq_len(T_all)
dow   <- factor(((t_ax - 1) %% 7) + 1)
sd_t  <- ifelse(t_ax <= 1000, 6, 15)              # shift at t = 1001
mu_t  <- 100 + 0.02 * t_ax + c(0, 4, 6, 5, 8, 14, 12)[as.integer(dow)]
y     <- mu_t + rnorm(T_all, 0, sd_t)

train <- 1:500; cal <- 501:700; eval_idx <- 701:1400

## Stale forecaster: fit once on train, never refit.
fit <- lm(y ~ t_ax + dow, data = data.frame(y = y, t_ax = t_ax, dow = dow),
          subset = train)
yhat  <- predict(fit, newdata = data.frame(t_ax = t_ax, dow = dow))
sd_tr <- sd(residuals(fit))

## ---------------------------------------------------------------------
## 2. Interval methods (alpha = 0.05)
## ---------------------------------------------------------------------

alpha <- 0.05

## (i) model-based Gaussian
lo_m <- yhat + qnorm(alpha / 2) * sd_tr
hi_m <- yhat + qnorm(1 - alpha / 2) * sd_tr

## (ii) static split conformal (finite-sample valid IF exchangeable)
s_cal <- abs(y[cal] - yhat[cal]); m <- length(s_cal)
q_spl <- sort(s_cal)[ceiling((m + 1) * (1 - alpha))]
lo_s <- yhat - q_spl; hi_s <- yhat + q_spl

## (iii) Adaptive Conformal Inference (Gibbs & Candes 2021)
aci <- function(y, yhat, idx, alpha = 0.05, gamma = 0.02, window = 200) {
  a_t <- alpha
  out <- matrix(NA_real_, length(idx), 3,
                dimnames = list(NULL, c("lo", "hi", "alpha_t")))
  for (k in seq_along(idx)) {
    t <- idx[k]
    s <- abs(y[(t - window):(t - 1)] - yhat[(t - window):(t - 1)])
    ## clamp: alpha_t can wander outside (0,1); quantile level must not
    q <- sort(s)[min(length(s), max(1, ceiling((length(s) + 1) *
                                               (1 - min(max(a_t, 1e-4), 1)))))]
    out[k, ] <- c(yhat[t] - q, yhat[t] + q, a_t)
    err <- as.numeric(y[t] < out[k, 1] | y[t] > out[k, 2])
    a_t <- a_t + gamma * (alpha - err)
  }
  out
}
A <- aci(y, yhat, eval_idx)

## ---------------------------------------------------------------------
## 3. Coverage by regime -- the demonstration
## ---------------------------------------------------------------------

pre  <- eval_idx <= 1000; post <- !pre
covr <- function(lo, hi, ix) mean(y[eval_idx][ix] >= lo[ix] & y[eval_idx][ix] <= hi[ix])
tab <- rbind(
  model = c(covr(lo_m[eval_idx], hi_m[eval_idx], pre),
            covr(lo_m[eval_idx], hi_m[eval_idx], post),
            covr(lo_m[eval_idx], hi_m[eval_idx], rep(TRUE, length(eval_idx)))),
  split = c(covr(lo_s[eval_idx], hi_s[eval_idx], pre),
            covr(lo_s[eval_idx], hi_s[eval_idx], post),
            covr(lo_s[eval_idx], hi_s[eval_idx], rep(TRUE, length(eval_idx)))),
  aci   = c(covr(A[, "lo"], A[, "hi"], pre),
            covr(A[, "lo"], A[, "hi"], post),
            covr(A[, "lo"], A[, "hi"], rep(TRUE, length(eval_idx)))))
colnames(tab) <- c("pre-shift", "post-shift", "overall")
cat("== Coverage of nominal 95% one-step intervals ==\n")
print(round(tab, 3))

## Gibbs-Candes guarantee: |mean(err) - alpha| <= (a_max+gamma)/(gamma*T)
err_aci <- 1 - tab["aci", "overall"]
bound   <- (max(A[, "alpha_t"]) + 0.02) / (0.02 * length(eval_idx))
cat(sprintf("\nACI long-run bound: |%.4f - 0.05| = %.4f  <=  %.4f  : %s\n",
            err_aci, abs(err_aci - alpha), bound, abs(err_aci - alpha) <= bound))
stopifnot(abs(err_aci - alpha) <= bound)

## ---------------------------------------------------------------------
## 4. Distributional evaluation: PIT and CRPS
## ---------------------------------------------------------------------

cat("\n== Validation of scoring machinery ==\n")

## (a) Hand-coded closed-form normal CRPS vs numeric quadrature of the
##     definition, and vs scoringRules.
crps_norm_hand <- function(yv, mu, sg) {
  z <- (yv - mu) / sg
  sg * (z * (2 * pnorm(z) - 1) + 2 * dnorm(z) - 1 / sqrt(pi))
}
crps_num <- integrate(function(x) (pnorm(x, 1, 2) - (x >= 2.5))^2,
                      -40, 40, rel.tol = 1e-12)$value
d1 <- abs(crps_norm_hand(2.5, 1, 2) - crps_num)
cat(sprintf("closed-form normal CRPS vs quadrature      : %.2e\n", d1))
stopifnot(d1 < 1e-9)
if (requireNamespace("scoringRules", quietly = TRUE)) {
  d2 <- abs(crps_norm_hand(2.5, 1, 2) - scoringRules::crps_norm(2.5, 1, 2))
  cat(sprintf("closed-form normal CRPS vs scoringRules    : %.2e\n", d2))
  stopifnot(d2 < 1e-12)
}

## (b) my one-step Gaussian intervals vs forecast::forecast on an ARIMA
##     fit -- checks the interval construction against the mainstream.
ar <- Arima(rnorm(300), order = c(1, 0, 0))
fc <- forecast(ar, h = 1, level = 95)
d3 <- max(abs(c(fc$lower, fc$upper) -
              (as.numeric(fc$mean) + qnorm(c(.025, .975)) * sqrt(ar$sigma2))))
cat(sprintf("one-step interval vs forecast::forecast    : %.2e\n", d3))
stopifnot(d3 < 1e-6)

## PIT for the two distributional forecasts (model-based normal; ACI's
## trailing-window conformal predictive system with signed residuals).
pit_model <- pnorm(y[eval_idx], yhat[eval_idx], sd_tr)
pit_cps <- vapply(seq_along(eval_idx), function(k) {
  t <- eval_idx[k]
  C <- sort(y[(t - 200):(t - 1)] - yhat[(t - 200):(t - 1)])
  (sum(C < (y[t] - yhat[t])) + runif(1)) / (length(C) + 1)
}, numeric(1))

## CRPS comparison (sample CRPS for the CPS via scoringRules).
crps_model <- mean(crps_norm_hand(y[eval_idx], yhat[eval_idx], sd_tr))
crps_cps <- if (requireNamespace("scoringRules", quietly = TRUE)) {
  mean(vapply(seq_along(eval_idx), function(k) {
    t <- eval_idx[k]
    scoringRules::crps_sample(y[t],
      yhat[t] + (y[(t - 200):(t - 1)] - yhat[(t - 200):(t - 1)]))
  }, numeric(1)))
} else NA
cat(sprintf("\nmean CRPS  model-based: %.3f   trailing CPS: %.3f\n",
            crps_model, crps_cps))
cat(sprintf("PIT dispersion (SD; uniform = 0.289)  model: %.3f   CPS: %.3f\n",
            sd(pit_model), sd(pit_cps)))

## ---------------------------------------------------------------------
## 5. The decision layer, forecasting edition: the newsvendor
## ---------------------------------------------------------------------
##
## Stocking decision with overage cost co = 1, underage cost cu = 4:
## the optimal action is the cu/(co+cu) = 80th percentile of the
## PREDICTIVE distribution -- the pinball/linlin result from the
## decision script, which is also the M5-Uncertainty loss. Verified by
## realised cost, not by theory.

cost <- function(stock, demand, co = 1, cu = 4)
  co * pmax(stock - demand, 0) + cu * pmax(demand - stock, 0)

qs <- c(.5, .65, .8, .9, .95)
realised <- vapply(qs, function(qq) {
  mean(vapply(seq_along(eval_idx), function(k) {
    t <- eval_idx[k]
    C <- sort(y[(t - 200):(t - 1)] - yhat[(t - 200):(t - 1)])
    stock <- yhat[t] + C[min(length(C), max(1, ceiling(qq * (length(C) + 1))))]
    cost(stock, y[t])
  }, numeric(1)))
}, numeric(1))
cat("\n== Newsvendor: mean realised cost by stocking quantile ==\n")
print(round(setNames(realised, paste0("Q", 100 * qs)), 3))
stopifnot(which.min(realised) == which(qs == 0.8))

## ---------------------------------------------------------------------
## 6. Plot: adaptive fan (consonance bands for the future) + PIT
## ---------------------------------------------------------------------

if (requireNamespace("ggplot2", quietly = TRUE)) {
  show <- eval_idx >= 940 & eval_idx <= 1100
  pd <- data.frame(t = eval_idx[show], y = y[eval_idx][show],
                   yhat = yhat[eval_idx][show],
                   lo_m = lo_m[eval_idx][show], hi_m = hi_m[eval_idx][show],
                   lo_a = A[show, "lo"], hi_a = A[show, "hi"])
  g <- ggplot2::ggplot(pd, ggplot2::aes(t)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lo_a, ymax = hi_a),
                         fill = "steelblue", alpha = .3) +
    ggplot2::geom_line(ggplot2::aes(y = lo_m), colour = "red", linetype = 2) +
    ggplot2::geom_line(ggplot2::aes(y = hi_m), colour = "red", linetype = 2) +
    ggplot2::geom_point(ggplot2::aes(y = y), size = .5) +
    ggplot2::geom_line(ggplot2::aes(y = yhat), linewidth = .3) +
    ggplot2::geom_vline(xintercept = 1000.5, linetype = 3) +
    ggplot2::labs(x = "t", y = "y",
                  subtitle = "Variance shift at dotted line. Blue: ACI band adapts; red dashed: model band does not.") +
    ggplot2::theme_bw()
  ggplot2::ggsave("forecast_aci.png", g, width = 8.5, height = 4.5, dpi = 150)

  ph <- rbind(data.frame(pit = pit_model, m = "model-based"),
              data.frame(pit = pit_cps,  m = "trailing conformal"))
  g2 <- ggplot2::ggplot(ph, ggplot2::aes(pit)) +
    ggplot2::geom_histogram(breaks = seq(0, 1, .1), fill = "grey60", colour = "white") +
    ggplot2::geom_hline(yintercept = length(eval_idx) / 10, linetype = 3) +
    ggplot2::facet_wrap(~m) + ggplot2::theme_bw() +
    ggplot2::labs(x = "PIT", subtitle = "Calibration check: flat = calibrated")
  ggplot2::ggsave("forecast_pit.png", g2, width = 7.5, height = 3.6, dpi = 150)
  cat("\nWrote forecast_aci.png, forecast_pit.png\n")
}
