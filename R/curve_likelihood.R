# curve_likelihood.R -----------------------------------------------------------
# Native likelihood-function constructors for concurve.
#
# curve_lik() repackages an object from the ProfileLikelihood package; the
# functions here construct likelihood functions directly, with no external
# dependency, and emit the same object contract so ggcurve(), curve_table(),
# and plot_compare() work unchanged:
#   [[1]] "Intervals Dataframe": values, likelihood, loglikelihood, support,
#         deviancestat  (class c("data.frame", "concurve"))
#
# deviancestat is D = -2 log(L / Lmax), i.e. the likelihood-ratio statistic on
# the chi-squared(1) scale, so the 1/6.83 relative-likelihood cutoff sits at
# qchisq(0.95, 1) = 3.84. plot_compare() labels that axis "2ln(MLR)". Every
# constructor must use this scale; curve_rev() derives it as zscore^2.
#   [[2]] "Intervals Table" (when table = TRUE), list classed "concurve"

#' Package A Log-Likelihood Grid As A concurve Likelihood Object
#'
#' Takes a grid of parameter values and the corresponding (profile)
#' log-likelihood and returns the same object structure as [curve_lik()],
#' so all of \pkg{concurve}'s plotting and tabling functions work on it.
#' This is the bridge between any likelihood-producing tool
#' (\code{optim()}, \pkg{maxLik}, \pkg{bbmle}, \pkg{ProfileLikelihood},
#' hand-coded exact likelihoods) and \pkg{concurve}.
#'
#' @param values A numeric vector of parameter values (the grid).
#' @param loglik A numeric vector of log-likelihood values evaluated at
#' \code{values}. Any additive constant is allowed; the function
#' normalizes internally so the maximum relative likelihood is 1.
#' @param table Indicates whether or not a table output with some relevant
#' statistics should be generated. The default is TRUE and generates a
#' table which is included in the list object.
#'
#' @return A list with 2 items where the dataframe of values is in the
#' first object, and the table for the values in the second if
#' \code{table = TRUE}. The dataframe holds \code{values},
#' \code{likelihood}, \code{loglikelihood} (relative, maximum 0),
#' \code{support} (relative likelihood in (0, 1]), and
#' \code{deviancestat}, the likelihood-ratio statistic
#' \eqn{D = -2\log(L/\hat{L})} on the \eqn{\chi^2_1} scale, so that the
#' \eqn{1/6.83} support cutoff corresponds to \eqn{D = 3.84}.
#'
#' @examples
#' # exact binomial likelihood for 8 successes in 20 trials
#' p <- seq(0.001, 0.999, length.out = 2000)
#' lik <- as_curve_lik(p, 8 * log(p) + 12 * log(1 - p))
#' ggcurve(lik[[1]], type = "l1")
#'
#' @seealso [curve_lik()] [curve_lik_glm()] [curve_lik_exact()]
#' @export
as_curve_lik <- function(values, loglik, table = TRUE) {
  if (!is.numeric(values) || !is.numeric(loglik)) {
    stop("Error: 'values' and 'loglik' must be numeric vectors")
  }
  if (length(values) != length(loglik)) {
    stop("Error: 'values' and 'loglik' must have the same length")
  }
  ok <- is.finite(values) & is.finite(loglik)
  if (sum(ok) < 10) {
    stop("Error: fewer than 10 finite (values, loglik) pairs")
  }
  values <- values[ok]
  loglik <- loglik[ok]
  o <- order(values)
  values <- values[o]
  loglik <- loglik[o]

  loglikelihood <- loglik - max(loglik) # relative log-likelihood, max = 0
  support <- exp(loglikelihood) # relative likelihood in (0, 1]

  likfunction <- data.frame(
    values        = values,
    likelihood    = support,
    loglikelihood = loglikelihood,
    support       = support,
    # D = -2 log(L / Lmax), on the chi-squared(1) scale
    deviancestat  = -2 * loglikelihood
  )
  class(likfunction) <- c("data.frame", "concurve")

  if (isTRUE(table)) {
    levels <- c(0.03, 0.05, 0.12, 0.14)
    df_subintervals <- curve_table(likfunction, levels, type = "l", format = "data.frame")
    class(df_subintervals) <- c("data.frame", "concurve")
    dataframes <- list(likfunction, df_subintervals)
    names(dataframes) <- c("Intervals Dataframe", "Intervals Table")
    class(dataframes) <- "concurve"
    return(dataframes)
  }
  list(likfunction)
}

#' Profile Likelihood Function For A Model Coefficient
#'
#' Computes the profile likelihood function for a single coefficient in a
#' model fitted by \code{lm()} or \code{glm()}, by refitting the model
#' with the coefficient constrained (via an offset) at each point of a
#' grid and recording the maximized log-likelihood. This is the direct
#' likelihood analogue of [curve_gen()] and requires no external
#' packages. The nuisance parameters are genuinely profiled out --
#' maximized over at every grid point -- not fixed at their MLEs.
#'
#' @param model A fitted \code{lm} or \code{glm} object.
#' @param var The name of the coefficient of interest, as a character
#' string (e.g. \code{"dose"}).
#' @param range The number of standard errors on either side of the
#' estimate that the grid should cover. Defaults to 5, which reaches
#' relative likelihoods well below 1/1000.
#' @param steps Number of grid points at which the profile is evaluated.
#' Defaults to 200; each point costs one constrained model refit.
#' @param table Indicates whether or not a table output with some relevant
#' statistics should be generated. The default is TRUE.
#'
#' @return A list with 2 items where the dataframe of values is in the
#' first object, and the table for the values in the second if
#' \code{table = TRUE}.
#'
#' @details The constrained fits use the offset trick: to fix the
#' coefficient of \eqn{x_j} at \eqn{b}, the term \eqn{b x_j} is moved
#' into an offset and the model is refit without \eqn{x_j}. For
#' \code{lm} objects the residual variance is profiled out exactly, so
#' the curve is the likelihood-ratio profile; its 1/6.83 interval is
#' slightly narrower than the t-based \code{confint.lm()} interval in
#' small samples. For \code{glm} objects the profile deviance agrees with
#' \code{confint()}'s profile-likelihood intervals: the relative
#' likelihood cutoff \eqn{\exp(-\chi^2_{1,0.95}/2) = 1/6.83} reproduces
#' the 95\% profile CI up to the grid resolution.
#'
#' For families with a free dispersion parameter (\code{gaussian},
#' \code{Gamma}, \code{inverse.gaussian}, and the \code{quasi} families)
#' the profile deviance is divided by the dispersion estimated from the
#' full model, \code{summary(model)$dispersion}, exactly as
#' \code{confint()} does for such models. For \code{binomial} and
#' \code{poisson} the dispersion is fixed at 1.
#'
#' @references
#' Venzon DJ, Moolgavkar SH. A method for computing profile-likelihood-
#' based confidence intervals. J R Stat Soc Ser C Appl Stat.
#' 1988;37(1):87-94.
#'
#' Cole SR, Chu H, Greenland S. Maximum likelihood, profile likelihood,
#' and penalized likelihood: a primer. Am J Epidemiol.
#' 2014;179(2):252-260.
#'
#' @examples
#' \dontrun{
#' mod <- glm(am ~ mpg, family = binomial, data = mtcars)
#' lik <- curve_lik_glm(mod, "mpg")
#' ggcurve(lik[[1]], type = "l1")
#' curve_support(lik[[1]])
#' }
#'
#' @seealso [as_curve_lik()] [curve_lik_exact()] [curve_support()]
#' @export
curve_lik_glm <- function(model, var, range = 5, steps = 200, table = TRUE) {
  if (!inherits(model, c("lm", "glm"))) {
    stop("Error: 'model' must be an object fitted by lm() or glm()")
  }
  if (!is.character(var) || length(var) != 1L) {
    stop("Error: 'var' must be a single character string")
  }
  cf <- stats::coef(model)
  if (!var %in% names(cf)) {
    stop(sprintf("Error: '%s' is not a coefficient of the model", var))
  }
  est <- cf[[var]]
  se <- sqrt(diag(stats::vcov(model)))[[var]]

  X <- stats::model.matrix(model)
  xj <- X[, var]
  keep <- setdiff(colnames(X), var)
  y <- stats::model.response(stats::model.frame(model))

  grid <- seq(est - range * se, est + range * se, length.out = steps)

  is_glm <- inherits(model, "glm")
  fam <- if (is_glm) stats::family(model) else stats::gaussian()
  w <- stats::weights(model)
  if (is.null(w)) w <- rep(1, NROW(X))
  base_offset <- stats::model.offset(stats::model.frame(model))
  if (is.null(base_offset)) base_offset <- rep(0, NROW(X))

  Xr <- X[, keep, drop = FALSE]

  # Dispersion: fixed at 1 for binomial/poisson, otherwise the estimate from
  # the full model (what stats:::profile.glm / confint() use). Without this,
  # Gamma / gaussian / quasi profiles are off by a factor of phi.
  phi <- if (is_glm && !fam$family %in% c("binomial", "poisson")) {
    summary(model)$dispersion
  } else {
    1
  }

  # starting values for the constrained refits: the full model's fitted means
  mu_start <- if (is_glm) stats::fitted(model) else NULL

  prof_one <- function(b) {
    off <- base_offset + b * xj
    if (ncol(Xr) == 0L) {
      # no nuisance regressors: log-likelihood at fixed linear predictor
      eta <- off
      mu <- fam$linkinv(eta)
      if (is_glm) {
        dev <- sum(fam$dev.resids(y, mu, w))
        return(-dev / (2 * phi)) # scaled log-likelihood up to a constant
      }
      return(-0.5 * length(y) * log(mean((y - mu)^2)))
    }
    fit <- tryCatch(
      suppressWarnings(
        stats::glm.fit(Xr, y,
          weights = w, offset = off, family = fam,
          mustart = mu_start, intercept = FALSE
        )
      ),
      error = function(e) NULL
    )
    # a constrained fit can be infeasible far in the tails (e.g. an inverse
    # link driven to a negative mean); record NA and drop the point below
    if (is.null(fit) || !isTRUE(fit$converged)) {
      return(NA_real_)
    }
    if (is_glm) {
      -fit$deviance / (2 * phi) # scaled log-likelihood up to a constant
    } else {
      # gaussian lm: profile out sigma^2 as well
      -0.5 * length(y) * log(fit$deviance / length(y))
    }
  }

  loglik <- vapply(grid, prof_one, numeric(1))
  bad <- is.na(loglik)
  if (all(bad)) {
    stop("Error: no constrained refit converged; check the model or reduce 'range'")
  }
  if (any(bad)) {
    message(
      sum(bad), " grid point(s) where the constrained refit did not converge ",
      "were dropped; reduce 'range' to avoid this."
    )
    grid <- grid[!bad]
    loglik <- loglik[!bad]
  }
  as_curve_lik(grid, loglik, table = table)
}

#' Exact Likelihood Functions For Common Designs
#'
#' Constructs the likelihood function implied by the exact sampling model
#' of several common designs, with any nuisance parameters removed by
#' conditioning or profiling rather than by normal approximation. Returns
#' the same object structure as [curve_lik()].
#'
#' @param type The design:
#' \describe{
#'   \item{\code{"prop"}}{A binomial proportion. Supply \code{x}
#'   (successes) and \code{n} (trials). The likelihood is exact binomial;
#'   the parameter is the risk \eqn{p}.}
#'   \item{\code{"or"}}{A 2x2 table odds ratio. Supply cell counts
#'   \code{a}, \code{b}, \code{c}, \code{d} (exposed cases, exposed
#'   non-cases, unexposed cases, unexposed non-cases). The likelihood is
#'   the exact conditional (noncentral hypergeometric) likelihood on the
#'   log-odds-ratio scale; its maximum is the conditional MLE, which
#'   deliberately differs from \eqn{ad/bc}.}
#'   \item{\code{"rr"}}{A rate ratio from two Poisson counts. Supply
#'   \code{a} (exposed events), \code{t1} (exposed person-time),
#'   \code{b} (unexposed events), \code{t0} (unexposed person-time). The
#'   likelihood is the exact conditional binomial likelihood on the
#'   log-rate-ratio scale.}
#'   \item{\code{"mean"}}{A normal mean. Supply the data vector
#'   \code{data}; the variance is profiled out.}
#'   \item{\code{"var"}}{A normal variance. Supply \code{data}; the
#'   parameter is \eqn{\sigma^2}.}
#'   \item{\code{"corr"}}{A bivariate-normal correlation. Supply
#'   \code{data} as a two-column matrix or data frame; the four nuisance
#'   parameters are profiled out numerically.}
#' }
#' @param x,n Successes and trials for \code{type = "prop"}.
#' @param a,b,c,d Cell counts for \code{type = "or"}; for
#' \code{type = "rr"}, \code{a} and \code{b} are the two event counts.
#' @param t1,t0 Person-time denominators for \code{type = "rr"}.
#' @param data Data for \code{type = "mean"}, \code{"var"} (numeric
#' vector), or \code{"corr"} (two columns).
#' @param steps Number of grid points. Defaults to 1000.
#' @param table Indicates whether or not a table output should be
#' generated. The default is TRUE.
#'
#' @return A list with 2 items where the dataframe of values is in the
#' first object, and the table for the values in the second if
#' \code{table = TRUE}.
#'
#' @references
#' Royall R. Statistical Evidence: A Likelihood Paradigm. Chapman &
#' Hall/CRC; 1997.
#'
#' Cox DR, Hinkley DV. Theoretical Statistics. Chapman & Hall; 1974.
#'
#' @examples
#' # exact conditional odds-ratio likelihood for a 2x2 table
#' lik <- curve_lik_exact(type = "or", a = 12, b = 8, c = 5, d = 15)
#' ggcurve(lik[[1]], type = "l1", nullvalue = 0)
#'
#' @seealso [as_curve_lik()] [curve_lik_glm()] [curve_support()]
#' @export
curve_lik_exact <- function(type = c("prop", "or", "rr", "mean", "var", "corr"),
                            x = NULL, n = NULL,
                            a = NULL, b = NULL, c = NULL, d = NULL,
                            t1 = NULL, t0 = NULL,
                            data = NULL, steps = 1000, table = TRUE) {
  type <- match.arg(type)

  out <- switch(type,
    prop = {
      if (is.null(x) || is.null(n)) stop("Error: 'prop' requires x and n")
      if (x < 0 || x > n) stop("Error: x must lie in [0, n]")
      p <- seq(1e-4, 1 - 1e-4, length.out = steps)
      # handle boundaries: 0*log(0) = 0
      ll <- ifelse(rep(x > 0, steps), x * log(p), 0) +
        ifelse(rep(x < n, steps), (n - x) * log(1 - p), 0)
      list(values = p, loglik = ll)
    },
    or = {
      if (any(vapply(list(a, b, c, d), is.null, logical(1)))) {
        stop("Error: 'or' requires cell counts a, b, c, and d")
      }
      n1 <- a + b
      n0 <- c + d
      m1 <- a + c
      klo <- max(0, m1 - n0)
      khi <- min(m1, n1)
      ks <- klo:khi
      logwt <- lchoose(n1, ks) + lchoose(n0, m1 - ks)
      grid <- seq(-4, 4, length.out = steps) + log((a + 0.5) * (d + 0.5) / ((b + 0.5) * (c + 0.5)))
      ll <- vapply(grid, function(lp) {
        lt <- logwt + ks * lp
        M <- max(lt)
        a * lp - (M + log(sum(exp(lt - M))))
      }, numeric(1))
      list(values = grid, loglik = ll)
    },
    rr = {
      if (any(vapply(list(a, b, t1, t0), is.null, logical(1)))) {
        stop("Error: 'rr' requires a, b, t1, and t0")
      }
      grid <- seq(-4, 4, length.out = steps) +
        log(((a + 0.5) / t1) / ((b + 0.5) / t0))
      ll <- vapply(grid, function(lt) {
        pi <- exp(lt) * t1 / (exp(lt) * t1 + t0)
        a * log(pi) + b * log(1 - pi)
      }, numeric(1))
      list(values = grid, loglik = ll)
    },
    mean = {
      if (is.null(data) || !is.numeric(data)) stop("Error: 'mean' requires a numeric 'data' vector")
      nn <- length(data)
      xbar <- mean(data)
      s <- stats::sd(data)
      grid <- seq(xbar - 5 * s / sqrt(nn), xbar + 5 * s / sqrt(nn), length.out = steps)
      ll <- vapply(grid, function(m) {
        -(nn / 2) * log(mean((data - m)^2))
      }, numeric(1))
      list(values = grid, loglik = ll)
    },
    var = {
      if (is.null(data) || !is.numeric(data)) stop("Error: 'var' requires a numeric 'data' vector")
      nn <- length(data)
      s2 <- stats::var(data)
      nu <- nn - 1
      grid <- seq(s2 * nu / stats::qchisq(0.9999, nu), s2 * nu / stats::qchisq(1e-4, nu), length.out = steps)
      ll <- -(nu / 2) * log(grid) - nu * s2 / (2 * grid)
      list(values = grid, loglik = ll)
    },
    corr = {
      if (is.null(data) || NCOL(data) != 2) stop("Error: 'corr' requires two-column 'data'")
      u <- data[, 1]
      v <- data[, 2]
      nn <- length(u)
      grid <- seq(-0.99, 0.99, length.out = steps)
      start <- c(mean(u), mean(v), log(stats::sd(u)), log(stats::sd(v)))
      ll <- vapply(grid, function(r) {
        neg <- function(par) {
          du <- (u - par[1]) / exp(par[3])
          dv <- (v - par[2]) / exp(par[4])
          quad <- (du^2 - 2 * r * du * dv + dv^2) / (1 - r^2)
          -(-nn * (par[3] + par[4]) - 0.5 * nn * log(1 - r^2) - 0.5 * sum(quad))
        }
        -stats::optim(start, neg, method = "BFGS")$value
      }, numeric(1))
      list(values = grid, loglik = ll)
    }
  )

  as_curve_lik(out$values, out$loglik, table = table)
}

#' Support (Likelihood) Intervals From A Likelihood Function
#'
#' Computes likelihood/support intervals -- the set of parameter values
#' whose relative likelihood exceeds \eqn{1/k} -- from any \pkg{concurve}
#' likelihood dataframe, using monotone interpolation on each shoulder of
#' the function rather than snapping to grid points.
#'
#' The conventional cutoffs are \eqn{1/6.83} (which corresponds to the
#' 95\% profile-likelihood interval via
#' \eqn{\exp(-\chi^2_{1,0.95}/2)}), and Royall's \eqn{1/8} and
#' \eqn{1/32} benchmarks for "fairly strong" and "strong" evidence.
#'
#' @param data The likelihood dataframe produced by [curve_lik()],
#' [as_curve_lik()], [curve_lik_glm()], or [curve_lik_exact()].
#' @param cutoffs A numeric vector of \eqn{k} values. The default
#' \code{c(6.83, 8, 32)}.
#'
#' @return A data frame with one row per cutoff: \code{k}, the implied
#' relative-likelihood threshold, the interval limits, and the MLE.
#'
#' @references
#' Royall R. Statistical Evidence: A Likelihood Paradigm. Chapman &
#' Hall/CRC; 1997.
#'
#' @examples
#' p <- seq(0.001, 0.999, length.out = 2000)
#' lik <- as_curve_lik(p, 8 * log(p) + 12 * log(1 - p))
#' curve_support(lik[[1]])
#'
#' @seealso [as_curve_lik()] [curve_lik_glm()] [curve_lik_exact()]
#' @export
curve_support <- function(data, cutoffs = c(6.83, 8, 32)) {
  if (!methods::is(data, "concurve") || is.null(data$support)) {
    stop("Error: 'data' must be a likelihood dataframe from 'concurve'.")
  }
  if (!is.numeric(cutoffs) || any(cutoffs <= 1)) {
    stop("Error: 'cutoffs' must be numeric values greater than 1")
  }

  v <- data$values
  s <- data$support
  imax <- which.max(s)

  # refine the MLE off the grid by quadratic interpolation around the peak
  mle <- v[imax]
  if (imax > 1L && imax < length(v)) {
    l0 <- data$loglikelihood[(imax - 1L):(imax + 1L)]
    v0 <- v[(imax - 1L):(imax + 1L)]
    denom <- (l0[1] - 2 * l0[2] + l0[3])
    if (is.finite(denom) && denom < 0) {
      mle <- v0[2] - 0.5 * (v0[3] - v0[1]) / 2 * (l0[3] - l0[1]) / denom
    }
  }

  interp_cross <- function(idx_side, thresh, boundary) {
    vv <- v[idx_side]
    ss <- s[idx_side]
    if (all(ss >= thresh)) {
      return(boundary)
    }
    o <- order(ss)
    stats::approx(ss[o], vv[o], xout = thresh, ties = "ordered")$y
  }

  res <- lapply(cutoffs, function(k) {
    thresh <- 1 / k
    lower <- interp_cross(seq_len(imax), thresh, boundary = v[1])
    upper <- interp_cross(seq(imax, length(v)), thresh, boundary = v[length(v)])
    data.frame(
      k = k, support.level = thresh,
      lower.limit = lower, upper.limit = upper, mle = mle
    )
  })
  out <- do.call(rbind, res)
  class(out) <- c("data.frame", "concurve")
  out
}
