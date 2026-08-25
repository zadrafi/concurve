## =====================================================================
## A decision layer on confidence distributions
##
## The mechanism: given a CD H(theta) and a loss L(theta, a), act by
## minimising the H-expected loss  integral L(theta, a) dH(theta) --
## using the CD exactly as a Bayesian would use a posterior, then
## checking (by simulation, not by faith) what frequentist risk the
## resulting rule actually has.
##
## Three-decision testing a la Rice: Rice (2010, Am Stat 64:345-349),
## Rice, Bonnett & Krakauer (2020, JRSS-A 183:411-430), and the Annual
## Review "Three-Decision Methods" -- following Tukey's "sensible
## formulation": on observing data, either assert theta > theta0,
## assert theta < theta0, or say nothing. No "accepting the null";
## abstention is an ACTION with a cost, not a conclusion.
##
## Section 5 is the memento mori: the false confidence theorem
## (Balch, Martin & Ferson 2019, Proc R Soc A) -- any additive
## distribution on a parameter (CD, fiducial, flat-prior posterior)
## assigns high belief to some false assertions, and decision rules
## built on those assertions fail badly. The CD-as-posterior trick is
## safe for the monotone decisions in sections 2-4 and NOT safe in
## general.
## =====================================================================

set.seed(1937)

## Data and its exact confidence distribution (normal mean, t-based;
## identical to the Stan GFD validated earlier in this project).
n <- 15
y <- rnorm(n, 0.4, 1)
ybar <- mean(y); se <- sd(y) / sqrt(n); df <- n - 1

H  <- function(t) pt((t - ybar) / se, df)           # CD
dH <- function(t) dt((t - ybar) / se, df) / se      # its density
Q  <- function(p) ybar + se * qt(p, df)

## H-expected loss of action a. Quadrature on the probability scale
## (integral of L(Q(p), a) dp) is far more stable than integrating
## L * dH over theta: the adaptive rule otherwise sees a near-zero
## integrand over most of a wide interval and misdiagnoses divergence.
exp_loss <- function(loss, a) {
  integrate(function(p) loss(Q(p), a), 0, 1,
            subdivisions = 2000L, rel.tol = 1e-9)$value
}
## Windowed theta-scale version, used only to expose divergence.
exp_loss_window <- function(loss, a, w) {
  integrate(function(t) loss(t, a) * dH(t), ybar - w * se, ybar + w * se,
            subdivisions = 2000L, rel.tol = 1e-8)$value
}


## ---------------------------------------------------------------------
## 1. Three-decision rule (sign of theta relative to theta0)
## ---------------------------------------------------------------------

## Loss: wrong-sign assertion costs 1, abstention costs cc, correct
## assertion costs 0. Expected losses under the CD:
##   assert +: H(theta0)      assert -: 1 - H(theta0)     abstain: cc
## For cc < 1/2 the rule is: assert the sign iff the one-sided CD tail
## at theta0 is below cc -- i.e. iff the TWO-sided p-value < 2*cc.
## Fisher's alpha = 0.05 test is this rule with abstention priced at
## cc = 0.025. The p-value function is sufficient for the whole family.
three_decision <- function(Hfun, theta0 = 0, cc = 0.025) {
  el <- c(pos = Hfun(theta0), neg = 1 - Hfun(theta0), abstain = cc)
  names(el)[which.min(el)]
}


## ---------------------------------------------------------------------
## 2. Point decisions under standard losses
## ---------------------------------------------------------------------

sq_loss  <- function(t, a) (t - a)^2
abs_loss <- function(t, a) abs(t - a)
## Asymmetric linear ("linlin"/newsvendor): overshooting costs co per
## unit, undershooting cu per unit. Minimiser = CD quantile cu/(co+cu).
linlin <- function(co, cu) function(t, a) co * pmax(a - t, 0) + cu * pmax(t - a, 0)

argmin_loss <- function(loss) {
  optimize(function(a) exp_loss(loss, a),
           interval = ybar + c(-8, 8) * se, tol = 1e-9)$minimum
}


## ---------------------------------------------------------------------
## 3. Validation
## ---------------------------------------------------------------------

cat("== Validation ==\n")

## (a) Three-decision rule coincides with the two-sided t-test at
##     alpha = 2*cc, decision by decision.
agree <- replicate(3000, {
  yy <- rnorm(10, runif(1, -1, 1))
  Hy <- function(t) pt((t - mean(yy)) / (sd(yy) / sqrt(10)), 9)
  dec <- three_decision(Hy, 0, cc = 0.025)
  pv  <- t.test(yy, mu = 0)$p.value
  (dec != "abstain") == (pv < 0.05)
})
cat(sprintf("three-decision == two-sided t-test at alpha=2c : %d/3000 agree\n",
            sum(agree)))
stopifnot(all(agree))

## (b) Squared loss chooses the CD mean; absolute loss the CD median.
d_sq  <- abs(argmin_loss(sq_loss)  - ybar)   # t CD mean = median = ybar
d_abs <- abs(argmin_loss(abs_loss) - Q(0.5))
cat(sprintf("squared-loss argmin vs CD mean                 : %.2e\n", d_sq))
cat(sprintf("absolute-loss argmin vs CD median              : %.2e\n", d_abs))
stopifnot(d_sq < 1e-6, d_abs < 1e-6)

## (c) Asymmetric linear loss chooses the cu/(co+cu) CD quantile: with
##     overshoot 4x as costly as undershoot, act at the 20th percentile.
d_ll <- abs(argmin_loss(linlin(4, 1)) - Q(1 / 5))
cat(sprintf("linlin(4,1) argmin vs CD 20%% quantile          : %.2e\n", d_ll))
stopifnot(d_ll < 1e-5)   # kink-smoothed objective: optimize() plateaus ~1e-6

## (d) Frequentist check of the three-decision rule: the wrong-sign
##     (type S) rate must not exceed cc, at any theta. The CD earns its
##     keep here: the Bayes-style rule under the exact CD inherits the
##     frequentist directional guarantee.
cat("\n== Directional (type S) error, cc = 0.025, n = 10 ==\n")
cat("theta   assert+  assert-  abstain  wrong-sign\n")
for (th in c(0, 0.25, 0.5, 1)) {
  dec <- replicate(4000, {
    yy <- rnorm(10, th)
    three_decision(function(t) pt((t - mean(yy)) / (sd(yy) / sqrt(10)), 9), 0)
  })
  wrong <- if (th > 0) mean(dec == "neg") else mean(dec %in% c("pos", "neg")) / 2
  cat(sprintf("%5.2f   %6.3f   %6.3f   %6.3f     %6.4f\n", th,
              mean(dec == "pos"), mean(dec == "neg"), mean(dec == "abstain"), wrong))
  stopifnot(wrong <= 0.025 + 3 * sqrt(0.025 * 0.975 / 4000))
}

## (e) A loss the CD cannot price: LINEX (exponential) loss has
##     INFINITE expected loss under any t-based CD, because the t has
##     no moment generating function. The quadrature quietly returns
##     ever-larger numbers as the integration window widens -- the
##     failure is silent unless you look.
linex <- function(aa) function(t, a) exp(aa * (t - a)) - aa * (t - a) - 1
cat("\n== LINEX under a t CD: divergence check ==\n")
## The t density decays polynomially (|t|^-(df+1)); exp(a*theta) always
## wins eventually, so the expected LINEX loss is +Inf for every a > 0.
## At small a the divergence hides beyond any window you would think to
## plot -- which is exactly why it is dangerous. Shown here with a
## steep-enough a that quadrature exposes it.
vals <- sapply(c(10, 20, 40, 60), function(w) exp_loss_window(linex(8), ybar, w))
for (i in seq_along(vals))
  cat(sprintf("integration half-width %2.0f se : expected loss %.3g\n",
              c(10, 20, 40, 60)[i], vals[i]))
stopifnot(all(diff(vals) > 0), vals[4] / vals[1] > 1e3)   # visibly diverging
cat("no finite answer exists (t has no mgf); with small a the same\n")
cat("divergence is numerically invisible. Match loss tail to CD tail.\n")


## ---------------------------------------------------------------------
## 4. Expected-loss curves (the decision-theoretic consonance plot)
## ---------------------------------------------------------------------

if (requireNamespace("ggplot2", quietly = TRUE)) {
  acts <- seq(ybar - 3 * se, ybar + 3 * se, length.out = 200)
  el <- rbind(
    data.frame(a = acts, el = vapply(acts, function(a) exp_loss(sq_loss, a), 1),
               loss = "squared (act at mean)"),
    data.frame(a = acts, el = vapply(acts, function(a) exp_loss(abs_loss, a), 1),
               loss = "absolute (act at median)"),
    data.frame(a = acts, el = vapply(acts, function(a) exp_loss(linlin(4, 1), a), 1),
               loss = "linlin 4:1 (act at Q20)")
  )
  g <- ggplot2::ggplot(el, ggplot2::aes(a, el, colour = loss)) +
    ggplot2::geom_line(linewidth = .8) +
    ggplot2::geom_vline(xintercept = c(ybar, Q(.2)), linetype = 3) +
    ggplot2::labs(x = "action a", y = "CD-expected loss", colour = NULL) +
    ggplot2::theme_bw() + ggplot2::theme(legend.position = "bottom")
  ggplot2::ggsave("cd_expected_loss.png", g, width = 7, height = 4.2, dpi = 150)
}


## ---------------------------------------------------------------------
## 5. Where it breaks: false confidence and probability dilution
## ---------------------------------------------------------------------

## Balch-Martin-Ferson, in one dimension. theta is a miss distance,
## and the TRUTH is theta = 0: collision course. We observe
## x ~ N(0, sigma) and, following the naive decision rule, declare
## "safe -- do not manoeuvre" whenever the CD probability of collision
## P_H(|theta| < delta) drops below 5%. Watch what worse data does.
delta <- 0.5
cat("\n== False confidence: P(declare 'safe') when collision is CERTAIN ==\n")
cat("sigma   mean CD-belief in collision   declared safe\n")
for (sg in c(0.25, 0.5, 1, 2, 4, 8)) {
  bel <- replicate(4000, {
    x <- rnorm(1, 0, sg)
    pnorm((delta - x) / sg) - pnorm((-delta - x) / sg)   # CD prob of |theta|<delta
  })
  cat(sprintf("%5.2f          %6.3f                    %6.3f\n",
              sg, mean(bel), mean(bel < 0.05)))
}
cat("\nWorse measurements -> smaller CD-belief in collision -> 'safe'.\n")
cat("The assertion {|theta| < delta} is non-monotone in theta; CD-based\n")
cat("expected-loss decisions are only licensed for the monotone family\n")
cat("(signs, quantiles, one-sided losses) validated above. Outside it,\n")
cat("use the sampling model directly or an inferential-model treatment.\n")
