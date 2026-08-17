#' Analytic Consonance Functions From Summary Statistics
#'
#' Computes consonance (confidence) intervals at every level directly from
#' closed-form quantile functions rather than by numerically inverting
#' \code{confint()} thousands of times. This is exact for the stated
#' sampling model, requires no fitted model object, produces no profiling
#' messages, and is typically several orders of magnitude faster than the
#' inversion-based functions. The output object is identical in structure
#' to that of [curve_gen()], so [ggcurve()], [curve_compare()],
#' [plot_compare()], and [curve_table()] all work on it unchanged.
#'
#' The analytic approach follows the confidence-distribution framework
#' reviewed by Xie & Singh (2013) and implemented for several estimate
#' types by Infanger & Schmidt-Trucksäss (2019) in the
#' \pkg{pvaluefunctions} package: for each interval level the limits are
#' read off the quantile function of the estimator's sampling
#' distribution.
#'
#' @param estimate The point estimate. For \code{dist = "corr"} this is the
#' sample correlation coefficient; for \code{dist = "var"} the sample
#' variance; for \code{dist = "prop"} the number of successes (a count,
#' together with \code{n}).
#' @param se The standard error of the estimate. Required for
#' \code{dist = "z"} and \code{dist = "t"}. Ignored otherwise.
#' @param df Degrees of freedom. Required for \code{dist = "t"}.
#' @param n The sample size. Required for \code{dist = "corr"},
#' \code{dist = "var"}, and \code{dist = "prop"}.
#' @param dist The sampling distribution used to construct the intervals:
#' \describe{
#'   \item{\code{"z"}}{Normal (Wald) intervals, \code{estimate} and \code{se}.}
#'   \item{\code{"t"}}{Student-t intervals, \code{estimate}, \code{se}, and \code{df}.}
#'   \item{\code{"corr"}}{Pearson correlation via the Fisher z-transformation,
#'   \code{estimate} (r) and \code{n}; the standard error on the z scale is
#'   \code{1/sqrt(n - 3)}.}
#'   \item{\code{"var"}}{A normal-model variance via the chi-squared pivot
#'   \code{(n - 1) s^2 / sigma^2}, \code{estimate} (\code{s^2}) and \code{n}.}
#'   \item{\code{"prop"}}{A binomial proportion via the Wilson score
#'   interval, \code{estimate} (number of successes) and \code{n}.}
#' }
#' @param log Indicates whether the estimate is on the log scale and the
#' limits should be exponentiated (as when supplying a log risk ratio, log
#' odds ratio, or log hazard ratio with its standard error). Defaults to
#' \code{FALSE}. Only available for \code{dist = "z"} and \code{dist = "t"}.
#' @param penalty An input to specify whether the intervals should be
#' corrected for multiple comparisons. The default is NULL, so there is no
#' correction. Other options include "bonferroni" and "sidak".
#' @param m Indicates how many comparisons are being done and the number
#' that should be used to correct for multiple comparisons. The default is
#' NULL.
#' @param steps Indicates how many consonance intervals are to be
#' calculated at various levels. By default, it is set to 1000. Because
#' the limits are computed analytically, large values are cheap.
#' @param table Indicates whether or not a table output with some relevant
#' statistics should be generated. The default is TRUE and generates a
#' table which is included in the list object.
#'
#' @return A list with 3 items where the dataframe of values is in the
#' first object, the values needed to calculate the density function in
#' the second, and the table for the values in the third if
#' \code{table = TRUE}.
#'
#' @references
#' Xie M, Singh K. Confidence distribution, the frequentist distribution
#' estimator of a parameter: a review. Int Stat Rev. 2013;81(1):3-39.
#'
#' Infanger D, Schmidt-Trucksäss A. P value functions: An underused method
#' to present research results and to promote quantitative reasoning.
#' Stat Med. 2019;38(21):4189-4197.
#'
#' Rafi Z, Greenland S. Semantic and cognitive tools to aid statistical
#' science: replace confidence and significance by compatibility and
#' surprise. BMC Med Res Methodol. 2020;20(1):244.
#'
#' @examples
#' # From a log hazard ratio and its SE, on the ratio scale:
#' hr <- curve_analytic(estimate = log(0.80), se = 0.16, dist = "z", log = TRUE)
#' ggcurve(hr[[1]], measure = "ratio", nullvalue = 1)
#'
#' # A correlation from r and n:
#' rho <- curve_analytic(estimate = 0.45, n = 40, dist = "corr")
#' ggcurve(rho[[1]])
#'
#' @seealso [curve_gen()] [curve_corr()] [curve_region()]
#' @export
curve_analytic <- function(estimate, se = NULL, df = NULL, n = NULL,
                           dist = c("z", "t", "corr", "var", "prop"),
                           log = FALSE, penalty = NULL, m = NULL,
                           steps = 1000, table = TRUE) {
  dist <- match.arg(dist)

  if (!is.numeric(estimate) || length(estimate) != 1L) {
    stop("Error: 'estimate' must be a single numeric value")
  }
  if (!is.numeric(steps) || steps < 10) {
    stop("Error: 'steps' must be a numeric value of at least 10")
  }
  if (isTRUE(log) && !dist %in% c("z", "t")) {
    stop("Error: 'log = TRUE' is only available for dist = \"z\" or \"t\"")
  }

  # required inputs per distribution -----------------------------------------
  switch(dist,
    z = {
      if (is.null(se)) stop("Error: 'se' is required for dist = \"z\"")
    },
    t = {
      if (is.null(se)) stop("Error: 'se' is required for dist = \"t\"")
      if (is.null(df)) stop("Error: 'df' is required for dist = \"t\"")
    },
    corr = {
      if (is.null(n)) stop("Error: 'n' is required for dist = \"corr\"")
      if (n < 4) stop("Error: 'n' must be at least 4 for dist = \"corr\"")
      if (abs(estimate) >= 1) stop("Error: a correlation must lie in (-1, 1)")
    },
    var = {
      if (is.null(n)) stop("Error: 'n' is required for dist = \"var\"")
      if (estimate <= 0) stop("Error: a variance must be positive")
    },
    prop = {
      if (is.null(n)) stop("Error: 'n' is required for dist = \"prop\"")
      if (estimate < 0 || estimate > n) {
        stop("Error: for dist = \"prop\", 'estimate' is the success count and must lie in [0, n]")
      }
    }
  )

  intrvls <- (1:(steps - 1)) / steps

  # multiple-comparison adjustment (mirrors curve_gen) ------------------------
  adj_levels <- if (is.null(penalty) && is.null(m)) {
    intrvls
  } else if (identical(penalty, "bonferroni") && !is.null(m) && m > 1) {
    1 - ((1 - intrvls) / m)
  } else if (identical(penalty, "sidak") && !is.null(m) && m > 1) {
    intrvls^(1 / m)
  } else {
    stop(
      "Error: 'penalty' must be NULL, \"bonferroni\", or \"sidak\", ",
      "and 'm' must be greater than 1"
    )
  }

  alpha_half <- (1 - adj_levels) / 2 # lower tail probability at each level

  limits <- switch(dist,
    z = {
      q <- stats::qnorm(1 - alpha_half)
      cbind(estimate - q * se, estimate + q * se)
    },
    t = {
      q <- stats::qt(1 - alpha_half, df = df)
      cbind(estimate - q * se, estimate + q * se)
    },
    corr = {
      zse <- 1 / sqrt(n - 3)
      q <- stats::qnorm(1 - alpha_half)
      cbind(
        tanh(atanh(estimate) - q * zse),
        tanh(atanh(estimate) + q * zse)
      )
    },
    var = {
      nu <- n - 1
      cbind(
        nu * estimate / stats::qchisq(1 - alpha_half, df = nu),
        nu * estimate / stats::qchisq(alpha_half, df = nu)
      )
    },
    prop = {
      # Wilson score limits at each level (Wilson 1927)
      phat <- estimate / n
      z <- stats::qnorm(1 - alpha_half)
      center <- (phat + z^2 / (2 * n)) / (1 + z^2 / n)
      halfwidth <- (z / (1 + z^2 / n)) * sqrt(phat * (1 - phat) / n + z^2 / (4 * n^2))
      cbind(center - halfwidth, center + halfwidth)
    }
  )

  df_out <- data.frame(lower.limit = limits[, 1], upper.limit = limits[, 2])

  if (isTRUE(log)) {
    df_out <- exp(df_out)
  }

  df_out$intrvl.width <- abs(df_out$upper.limit - df_out$lower.limit)
  df_out$intrvl.level <- intrvls
  df_out$cdf <- (abs(df_out$intrvl.level / 2)) + 0.5
  df_out$pvalue <- 1 - intrvls
  df_out$svalue <- -log2(df_out$pvalue)
  df_out <- utils::head(df_out, -1)
  class(df_out) <- c("data.frame", "concurve")

  densdf <- data.frame(x = c(df_out$lower.limit, df_out$upper.limit))
  densdf <- utils::head(densdf, -1)
  class(densdf) <- c("data.frame", "concurve")

  if (isTRUE(table)) {
    levels <- c(0.25, 0.50, 0.75, 0.80, 0.85, 0.90, 0.95, 0.975, 0.99)
    df_subintervals <- curve_table(df_out, levels, type = "c", format = "data.frame")
    class(df_subintervals) <- c("data.frame", "concurve")
    dataframes <- list(df_out, densdf, df_subintervals)
    names(dataframes) <- c("Intervals Dataframe", "Intervals Density", "Intervals Table")
    class(dataframes) <- "concurve"
    return(dataframes)
  }

  list(df_out, densdf)
}

#' Confidence In A Parameter Region
#'
#' Computes the confidence-distribution probability that the parameter
#' lies below, above, or inside a specified region, from any \code{concurve}
#' intervals dataframe -- whether produced analytically by
#' [curve_analytic()] or numerically by [curve_gen()], [curve_boot()],
#' [curve_meta()], and the other interval functions. Also reports the
#' counternull value (Rosenthal & Rubin 1994) for a supplied null value.
#'
#' The confidence distribution \eqn{H(\theta)} is reconstructed from the
#' stored interval limits: each two-sided level \eqn{1 - \alpha} places its
#' lower limit at \eqn{H^{-1}(\alpha/2)} and its upper limit at
#' \eqn{H^{-1}(1 - \alpha/2)}, so the pairs (limit, quantile) trace out the
#' whole distribution and intermediate values are obtained by monotone
#' interpolation. Region probabilities are then differences of
#' \eqn{H}. This is the "confidence in a treatment effect" analysis of
#' Marschner (2024): confidence in benefit is \eqn{H(\theta_0)} (or its
#' complement, depending on direction), confidence in equivalence is
#' \eqn{H(b) - H(a)} for an equivalence region \eqn{(a, b)}.
#'
#' @param data The intervals dataframe produced by one of the interval
#' functions (e.g. \code{x[[1]]}).
#' @param lower The lower bound of the region of interest. Use \code{-Inf}
#' for "everything below \code{upper}".
#' @param upper The upper bound of the region of interest. Use \code{Inf}
#' for "everything above \code{lower}".
#' @param nullvalue Optional null value for which the two-sided P-value,
#' S-value, and counternull are reported.
#'
#' @return A data frame with one row containing the region, the
#' confidence-distribution probability of the region, and (if
#' \code{nullvalue} was supplied) the two-sided P-value, S-value, and
#' counternull.
#'
#' @references
#' Marschner IC. Confidence distributions for treatment effects in
#' clinical trials: posteriors without priors. Stat Med.
#' 2024;43(6):1271-1289.
#'
#' Rosenthal R, Rubin DB. The counternull value of an effect size: a new
#' statistic. Psychol Sci. 1994;5(6):329-334.
#'
#' @examples
#' hr <- curve_analytic(estimate = log(0.80), se = 0.16, dist = "z", log = TRUE)
#' # Confidence that the hazard ratio shows any benefit (HR < 1):
#' curve_region(hr[[1]], lower = 0, upper = 1, nullvalue = 1)
#' # Confidence in equivalence, HR within (0.9, 1.1):
#' curve_region(hr[[1]], lower = 0.9, upper = 1.1)
#'
#' @seealso [curve_analytic()] [curve_gen()]
#' @export
curve_region <- function(data, lower = -Inf, upper = Inf, nullvalue = NULL) {
  if (!methods::is(data, "concurve") || is.null(data$lower.limit)) {
    stop("Error: 'data' must be an intervals dataframe from 'concurve'.")
  }
  if (!is.numeric(lower) || !is.numeric(upper) || lower >= upper) {
    stop("Error: 'lower' must be less than 'upper'.")
  }

  # Reconstruct the confidence distribution from stored limits --------------
  alpha_half <- (1 - data$intrvl.level) / 2
  theta <- c(data$lower.limit, data$upper.limit)
  hvals <- c(alpha_half, 1 - alpha_half)
  o <- order(theta)
  theta <- theta[o]
  hvals <- hvals[o]

  # collapse exact ties in theta (e.g. the level-0 interval) and enforce
  # monotone H, then interpolate with a monotone Hyman spline, which is far
  # more accurate than linear interpolation where H is curved
  keep <- !duplicated(theta)
  theta <- theta[keep]
  hvals <- cummax(hvals[keep])

  H_spline <- stats::splinefun(theta, hvals, method = "hyman")

  # Beyond the smallest/largest stored limits only the tail mass
  # (at most (1 - max level)/2) is unaccounted for. Extrapolate it on the
  # probit scale, which is exact when the tail of the confidence
  # distribution is asymptotically normal and clamped to the known bounds
  # otherwise.
  k <- min(10L, length(theta) - 1L)
  z_lo <- stats::qnorm(hvals[c(1L, 1L + k)])
  z_hi <- stats::qnorm(hvals[c(length(hvals) - k, length(hvals))])
  slope_lo <- (z_lo[2] - z_lo[1]) / (theta[1L + k] - theta[1L])
  slope_hi <- (z_hi[2] - z_hi[1]) / (theta[length(theta)] - theta[length(theta) - k])

  H_at <- function(q) {
    if (is.infinite(q)) {
      return(if (q < 0) 0 else 1)
    }
    if (q <= theta[1]) {
      z <- z_lo[1] + slope_lo * (q - theta[1])
      return(min(stats::pnorm(z), hvals[1]))
    }
    if (q >= theta[length(theta)]) {
      z <- z_hi[2] + slope_hi * (q - theta[length(theta)])
      return(max(stats::pnorm(z), hvals[length(hvals)]))
    }
    min(max(H_spline(q), 0), 1)
  }

  p_region <- H_at(upper) - H_at(lower)

  out <- data.frame(
    lower = lower,
    upper = upper,
    conf.region = p_region
  )

  if (!is.null(nullvalue)) {
    h0 <- H_at(nullvalue)
    pval <- 2 * min(h0, 1 - h0)
    # counternull: the value with the same two-sided P-value on the other
    # side of the point estimate, H^{-1}(1 - H(null))
    hu <- !duplicated(hvals)
    Hinv <- stats::splinefun(hvals[hu], theta[hu], method = "hyman")
    out$pvalue <- pval
    out$svalue <- -log2(pval)
    out$counternull <- Hinv(min(max(1 - h0, min(hvals)), max(hvals)))
  }

  class(out) <- c("data.frame", "concurve")
  out
}
