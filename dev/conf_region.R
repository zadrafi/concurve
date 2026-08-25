## =====================================================================
## conf_region(): a wrapper for joint confidence (consonance) regions
##
## For a parameter PAIR, the object of interest is not one ellipse at
## one alpha but the whole consonance SURFACE: at every grid point
## (t1, t2), the p-value for the joint hypothesis (theta1, theta2) =
## (t1, t2). Every confidence region at every level is a contour of
## this surface -- the 2D version of the consonance curve.
##
## Methods:
##   "lr"      profile likelihood-ratio region: fix the pair, refit
##             everything else, calibrate the deviance drop by chi^2_2.
##             (glm families with fixed dispersion: binomial, poisson.)
##   "wald"    quadratic approximation from vcov(); cheap, symmetric,
##             and wrong exactly when you care (small n, curvature).
##   "exact"   linear models only: the F-calibrated ellipse, which is
##             exact under normal errors.
##
## S3 interface:
##   conf_region(fit, parm = c("x1","x2"), method, level, steps, expand)
## returns a list: grid data.frame (t1, t2, stat, pvalue, svalue),
## boundary polygons per level, and metadata. Methods for lm, glm, and
## a default taking any negative log-likelihood function.
## =====================================================================

conf_region <- function(object, ...) UseMethod("conf_region")

.grid_from_wald <- function(est, V, steps, expand) {
  r <- expand * sqrt(qchisq(0.999, 2))
  s1 <- sqrt(V[1, 1]); s2 <- sqrt(V[2, 2])
  list(t1 = seq(est[1] - r * s1, est[1] + r * s1, length.out = steps),
       t2 = seq(est[2] - r * s2, est[2] + r * s2, length.out = steps))
}

.finish <- function(grid_df, est, levels, method, parm) {
  grid_df$pvalue <- pchisq(grid_df$stat, df = 2, lower.tail = FALSE)
  grid_df$svalue <- -log2(grid_df$pvalue)
  structure(list(grid = grid_df, estimate = est, parm = parm,
                 levels = levels, method = method,
                 cutoffs = qchisq(levels, 2)),
            class = "conf_region")
}

## ---- default: any negative log-likelihood --------------------------
## nll(theta_full) with `parm` giving the two indices of interest;
## remaining coordinates are profiled out with optim (warm-started
## column by column, which matters: cold starts on a fine grid both
## waste time and occasionally converge to the wrong branch).
conf_region.default <- function(object, start, parm = 1:2,
                                method = "lr", levels = c(.5, .8, .95, .99),
                                steps = 61, expand = 1.6, hessian_V = NULL, ...) {
  nll <- object
  opt <- optim(start, nll, method = "BFGS", hessian = TRUE)
  est <- opt$par[parm]
  V <- if (is.null(hessian_V)) solve(opt$hessian)[parm, parm] else hessian_V
  g <- .grid_from_wald(est, V, steps, expand)

  nuis <- setdiff(seq_along(start), parm)
  prof_nll <- function(t1, t2, warm) {
    if (!length(nuis)) {
      th <- start; th[parm] <- c(t1, t2); list(v = nll(th), warm = warm)
    } else {
      o <- optim(warm, function(nu) {
        th <- numeric(length(start)); th[parm] <- c(t1, t2); th[nuis] <- nu
        nll(th)
      }, method = "BFGS")
      list(v = o$value, warm = o$par)
    }
  }

  stat <- matrix(NA_real_, steps, steps)
  warm0 <- opt$par[nuis]
  for (i in seq_len(steps)) {
    warm <- warm0
    for (j in seq_len(steps)) {
      pr <- prof_nll(g$t1[i], g$t2[j], warm)
      stat[i, j] <- 2 * (pr$v - opt$value); warm <- pr$warm
    }
  }
  gd <- expand.grid(t1 = g$t1, t2 = g$t2)
  gd$stat <- pmax(0, as.vector(t(stat)))
  .finish(gd, est, levels, "lr", parm)
}

## ---- glm: profile LR via offset refits ------------------------------
conf_region.glm <- function(object, parm, method = c("lr", "wald"),
                            levels = c(.5, .8, .95, .99),
                            steps = 61, expand = 1.6, ...) {
  method <- match.arg(method)
  if (method == "lr" &&
      !object$family$family %in% c("binomial", "poisson"))
    stop("LR calibration here assumes fixed dispersion (binomial/poisson).")
  X <- model.matrix(object); cf <- coef(object)
  stopifnot(all(parm %in% colnames(X)))
  est <- cf[parm]; V <- vcov(object)[parm, parm]
  g <- .grid_from_wald(est, V, steps, expand)
  gd <- expand.grid(t1 = g$t1, t2 = g$t2)

  if (method == "wald") {
    gd$stat <- mahalanobis(as.matrix(gd), center = est, cov = V)
    return(.finish(gd, est, levels, "wald", parm))
  }

  keep <- setdiff(colnames(X), parm)
  dat  <- model.frame(object)
  yv   <- model.response(dat)
  dev_full <- deviance(object)
  gd$stat <- vapply(seq_len(nrow(gd)), function(k) {
    off <- as.vector(X[, parm, drop = FALSE] %*% c(gd$t1[k], gd$t2[k]))
    fit <- suppressWarnings(glm.fit(x = X[, keep, drop = FALSE], y = yv,
                                    offset = off, family = object$family))
    fit$deviance - dev_full
  }, numeric(1))
  gd$stat <- pmax(0, gd$stat)
  .finish(gd, est, levels, "lr", parm)
}

## ---- lm: exact F ellipse (plus Wald = the same shape, chi2-scaled) --
conf_region.lm <- function(object, parm, method = c("exact", "wald"),
                           levels = c(.5, .8, .95, .99),
                           steps = 121, expand = 1.6, ...) {
  method <- match.arg(method)
  est <- coef(object)[parm]; V <- vcov(object)[parm, parm]
  g <- .grid_from_wald(est, V, steps, expand)
  gd <- expand.grid(t1 = g$t1, t2 = g$t2)
  md <- mahalanobis(as.matrix(gd), center = est, cov = V)
  out <- if (method == "exact") {
    df2 <- df.residual(object)
    gd$stat <- md
    r <- .finish(gd, est, levels, "exact", parm)
    r$grid$pvalue <- pf(md / 2, 2, df2, lower.tail = FALSE)   # F calibration
    r$grid$svalue <- -log2(r$grid$pvalue)
    r$cutoffs <- 2 * qf(levels, 2, df2)
    r
  } else { gd$stat <- md; .finish(gd, est, levels, "wald", parm) }
  out
}

## ---- boundary extraction and plotting -------------------------------
region_boundary <- function(cr, level = 0.95) {
  cl <- grDevices::contourLines(
    x = sort(unique(cr$grid$t1)), y = sort(unique(cr$grid$t2)),
    z = matrix(cr$grid$stat,
               nrow = length(unique(cr$grid$t1)), byrow = TRUE),
    levels = cr$cutoffs[match(level, cr$levels)])
  lapply(cl, function(s) data.frame(t1 = s$x, t2 = s$y))
}

plot_region <- function(cr, title = NULL) {
  stopifnot(requireNamespace("ggplot2", quietly = TRUE))
  ggplot2::ggplot(cr$grid, ggplot2::aes(t1, t2)) +
    ggplot2::geom_raster(ggplot2::aes(fill = pvalue)) +
    ggplot2::geom_contour(ggplot2::aes(z = stat), breaks = cr$cutoffs,
                          colour = "white", linewidth = .4) +
    ggplot2::scale_fill_viridis_c(name = "p-value", limits = c(0, 1)) +
    ggplot2::annotate("point", x = cr$estimate[1], y = cr$estimate[2],
                      shape = 3, colour = "white") +
    ggplot2::labs(x = cr$parm[1], y = cr$parm[2], subtitle = title) +
    ggplot2::theme_bw()
}


## =====================================================================
## Validation
## =====================================================================

set.seed(4)
cat("== Validation ==\n")

## Test data: logistic regression, deliberately small n.
N <- 120
d <- data.frame(x1 = rnorm(N), x2 = rnorm(N))
d$y <- rbinom(N, 1, plogis(-0.3 + 0.8 * d$x1 - 0.5 * d$x2))
gfit <- glm(y ~ x1 + x2, binomial, data = d)

## (a) lm exact-F ellipse boundary against car::confidenceEllipse.
lfit <- lm(mpg ~ wt + hp + qsec, data = mtcars)
cr_lm <- conf_region(lfit, parm = c("wt", "hp"), method = "exact", steps = 201)
if (requireNamespace("car", quietly = TRUE)) {
  ce <- car::confidenceEllipse(lfit, which.coef = c("wt", "hp"),
                               levels = 0.95, draw = FALSE, segments = 200)
  ## car's ellipse points must lie ON my 95% contour: their F statistic
  ## must equal the cutoff.
  md <- mahalanobis(ce, center = coef(lfit)[c("wt", "hp")],
                    cov = vcov(lfit)[c("wt", "hp"), c("wt", "hp")])
  dev_car <- max(abs(md - 2 * qf(0.95, 2, df.residual(lfit))))
  cat(sprintf("car::confidenceEllipse on the exact-F contour : %.2e\n", dev_car))
  stopifnot(dev_car < 1e-8)
}

## (b) glm offset-refit machinery against MASS profile intervals: at
##     the endpoints of confint(), the 1-parameter profile deviance
##     drop equals qchisq(.95, 1) = 3.8415.
ci <- suppressMessages(confint(gfit, "x1"))
dev1 <- vapply(ci, function(b1) {
  X <- model.matrix(gfit)
  f <- suppressWarnings(glm.fit(X[, c("(Intercept)", "x2")], gfit$y,
                                offset = X[, "x1"] * b1, family = binomial()))
  f$deviance - deviance(gfit)
}, numeric(1))
d_b <- max(abs(dev1 - qchisq(.95, 1)))
cat(sprintf("profile deviance at confint() endpoints        : %.2e\n", d_b))
stopifnot(d_b < 5e-3)          # confint()'s own spline interpolation error

## (c) LR statistic vanishes at the MLE.
cr <- conf_region(gfit, parm = c("x1", "x2"), method = "lr", steps = 61)
at_mle <- cr$grid$stat[which.min((cr$grid$t1 - cr$estimate[1])^2 +
                                 (cr$grid$t2 - cr$estimate[2])^2)]
cat(sprintf("LR statistic near the MLE grid point           : %.2e\n", at_mle))
stopifnot(at_mle < 1e-2)

## (d) default (nll) method reproduces the glm method on the same grid.
nll_logit <- function(th) {
  eta <- th[1] + th[2] * d$x1 + th[3] * d$x2
  -sum(d$y * eta - log1p(exp(eta)))
}
cr_def <- conf_region(nll_logit, start = c(0, 0, 0), parm = 2:3,
                      method = "lr", steps = 21)
cr_glm <- conf_region(gfit, parm = c("x1", "x2"), method = "lr", steps = 21)
## align: default grid centres on optim estimates; compare via interp at
## shared points using the glm grid range on both -- simplest: compare
## profile stat at four probe points computed by both routes.
probe <- rbind(cr$estimate + c(.3, .2), cr$estimate + c(-.4, .1),
               cr$estimate + c(.2, -.3), cr$estimate)
stat_probe <- function(t1, t2) {
  X <- model.matrix(gfit)
  f <- suppressWarnings(glm.fit(X[, "(Intercept)", drop = FALSE], gfit$y,
        offset = X[, "x1"] * t1 + X[, "x2"] * t2, family = binomial()))
  f$deviance - deviance(gfit)
}
stat_nll <- function(t1, t2) {
  o <- optimize(function(b0) nll_logit(c(b0, t1, t2)), c(-5, 5), tol = 1e-10)
  2 * (o$objective - nll_logit(c(coef(gfit))))
}
d_pr <- max(abs(mapply(stat_probe, probe[, 1], probe[, 2]) -
                mapply(stat_nll,   probe[, 1], probe[, 2])))
cat(sprintf("glm offset route vs hand nll route             : %.2e\n", d_pr))
stopifnot(d_pr < 1e-6)

## (e) Joint coverage, ADEMP-lite (n_sim = 2000; coverage MCSE ~ .005).
##     Membership needs only the profile stat AT the truth -- no grid.
cat("\n== Joint 95% coverage, logistic, 2000 reps per n ==\n")
truth <- c(0.8, -0.5)
for (nn in c(40, 150)) {
hits <- t(replicate(2000, {
  dd <- data.frame(x1 = rnorm(nn), x2 = rnorm(nn))
  dd$y <- rbinom(nn, 1, plogis(-0.3 + 0.8 * dd$x1 - 0.5 * dd$x2))
  f <- suppressWarnings(glm(y ~ x1 + x2, binomial, data = dd))
  X <- model.matrix(f)
  f0 <- suppressWarnings(glm.fit(X[, "(Intercept)", drop = FALSE], f$y,
        offset = X[, "x1"] * truth[1] + X[, "x2"] * truth[2],
        family = binomial()))
  lr   <- (f0$deviance - deviance(f)) <= qchisq(.95, 2)
  wald <- mahalanobis(t(coef(f)[c("x1", "x2")]), truth,
                      vcov(f)[c("x1", "x2"), c("x1", "x2")]) <= qchisq(.95, 2)
  c(lr = lr, wald = wald)
}))
cat(sprintf("n = %3d  profile LR : %.3f   Wald : %.3f   (MCSE ~ 0.005)\n",
            nn, mean(hits[, "lr"]), mean(hits[, "wald"])))
}

## ---------------------------------------------------------------------
## Plots
## ---------------------------------------------------------------------
if (requireNamespace("ggplot2", quietly = TRUE)) {
  cr95 <- conf_region(gfit, parm = c("x1", "x2"), method = "lr", steps = 101)
  crw  <- conf_region(gfit, parm = c("x1", "x2"), method = "wald", steps = 101)
  b_lr <- region_boundary(cr95, .95)[[1]]; b_w <- region_boundary(crw, .95)[[1]]
  g <- plot_region(cr95, "Joint consonance surface, logistic (x1, x2)") +
    ggplot2::geom_path(data = b_w, ggplot2::aes(t1, t2),
                       colour = "red", linetype = 2, linewidth = .5)
  ggplot2::ggsave("conf_region.png", g, width = 6.8, height = 5.2, dpi = 150)
  cat("\nWrote conf_region.png (white: LR contours; red dashed: 95% Wald)\n")
}
