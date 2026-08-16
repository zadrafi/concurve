# =============================================================================
# ADVANCED PLOTTING FUNCTIONS FOR LIKELIHOOD OBJECTS
# =============================================================================

#' Plot likelihood function
#'
#' @description Plot a likelihood function showing relative likelihood or deviance,
#'   with optional confidence intervals and reference lines.
#'
#' @param x A \code{likelihood_function} object
#' @param parameter Name of the parameter to plot. If \code{NULL}, plots the first parameter
#'   and displays a message.
#' @param type Type of plot: \code{"likelihood"} (relative likelihood), \code{"deviance"}
#'   (profile deviance), or \code{"both"} (two-panel plot)
#' @param n_points Number of parameter values at which to evaluate the profile likelihood
#' @param interval A length-2 numeric vector specifying the range of parameter values
#'   to plot. If \code{NULL}, automatically determined as ±5 standard errors from the MLE.
#' @param add_ci Logical; if \code{TRUE}, adds confidence interval bounds to the plot
#' @param ci_level Confidence level for the intervals (default: 0.95)
#' @param add_mle Logical; if \code{TRUE}, adds a vertical line at the MLE
#' @param relative Logical; if \code{TRUE}, plots relative likelihood; if \code{FALSE},
#'   plots absolute likelihood. Only applies to likelihood plots.
#' @param main Main title for the plot. If \code{NULL}, automatically generated.
#' @param xlab X-axis label. If \code{NULL}, uses the parameter name.
#' @param ylab Y-axis label. If \code{NULL}, automatically determined by plot type.
#' @param col Color for the main likelihood/deviance curve
#' @param lwd Line width for the main curve
#' @param ... Additional arguments passed to \code{\link{plot}}
#'
#' @return Invisibly returns the profile likelihood data frame
#'
#' @details Creates publication-quality plots of likelihood and deviance functions.
#'   Includes optional confidence intervals and reference lines. For two-panel plots,
#'   the layout is automatically managed.
#'
#' @seealso \code{\link{ggplot_likelihood}} for ggplot2 version,
#'   \code{\link{plotly_likelihood}} for interactive plots
#'
#' @export
plot.likelihood_function <- function(x,
                                     parameter = NULL,
                                     type = c("likelihood", "deviance", "both"),
                                     n_points = 200,
                                     interval = NULL,
                                     add_ci = TRUE,
                                     ci_level = 0.95,
                                     add_mle = TRUE,
                                     relative = TRUE,
                                     main = NULL,
                                     xlab = NULL,
                                     ylab = NULL,
                                     col = "black",
                                     lwd = 2,
                                     ...) {
  type <- match.arg(type)

  # Select parameter
  if (is.null(parameter)) {
    parameter <- x$param_names[1]
    message("Plotting first parameter: ", parameter)
  }

  if (!parameter %in% x$param_names) {
    stop("Parameter '", parameter, "' not found in model", call. = FALSE)
  }

  param_idx <- which(x$param_names == parameter)
  mle_val <- x$mle[param_idx]

  # Determine plotting interval
  if (is.null(interval)) {
    se <- sqrt(diag(vcov(x)))[param_idx]
    interval <- c(mle_val - 5 * se, mle_val + 5 * se)
  }

  # Generate values
  param_values <- seq(interval[1], interval[2], length.out = n_points)

  # Compute profile likelihood
  profile_data <- x$profile(parameter, param_values)

  # Setup labels
  if (is.null(xlab)) xlab <- parameter
  if (is.null(main)) main <- sprintf("Likelihood Function: %s", parameter)

  # Plot based on type
  if (type == "both") {
    # Two-panel plot
    par(mfrow = c(2, 1), mar = c(4, 4, 3, 2))

    # Top panel: Likelihood
    .plot_likelihood_panel(
      profile_data, param_values, mle_val,
      relative, add_ci, ci_level, x, parameter,
      xlab, "Relative Likelihood", main, col, lwd, ...
    )

    # Bottom panel: Deviance
    .plot_deviance_panel(
      profile_data, param_values, mle_val,
      add_ci, ci_level, x, parameter,
      xlab, "Deviance", NULL, col, lwd, ...
    )

    par(mfrow = c(1, 1))
  } else if (type == "likelihood") {
    .plot_likelihood_panel(
      profile_data, param_values, mle_val,
      relative, add_ci, ci_level, x, parameter,
      xlab, ylab %||% "Relative Likelihood",
      main, col, lwd, ...
    )
  } else {
    .plot_deviance_panel(
      profile_data, param_values, mle_val,
      add_ci, ci_level, x, parameter,
      xlab, ylab %||% "Deviance",
      main, col, lwd, ...
    )
  }

  invisible(profile_data)
}


# Helper: Plot likelihood panel
.plot_likelihood_panel <- function(profile_data, param_values, mle_val,
                                   relative, add_ci, ci_level, x, parameter,
                                   xlab, ylab, main, col, lwd, ...) {
  if (relative) {
    # Relative likelihood
    lik_vals <- exp(profile_data$loglik - max(profile_data$loglik))
    if (is.null(ylab)) ylab <- "Relative Likelihood"
  } else {
    # Absolute likelihood
    lik_vals <- exp(profile_data$loglik)
    if (is.null(ylab)) ylab <- "Likelihood"
  }

  plot(param_values, lik_vals,
    type = "l", lwd = lwd, col = col,
    xlab = xlab, ylab = ylab, main = main,
    las = 1, ...
  )

  # Add MLE line
  abline(v = mle_val, col = "red", lty = 2, lwd = 1.5)

  # Add confidence interval
  if (add_ci) {
    ci <- x$confint(parameter, level = ci_level)
    abline(v = ci, col = "blue", lty = 2, lwd = 1.5)

    # Add horizontal line at CI cutoff
    ci_cutoff <- exp(-qchisq(ci_level, 1) / 2)
    abline(h = ci_cutoff, col = "gray60", lty = 3, lwd = 1)

    # Add legend
    legend("topright",
      legend = c("MLE", sprintf("%d%% CI", round(ci_level * 100))),
      col = c("red", "blue"), lty = 2, lwd = 1.5, bty = "n"
    )
  }

  # Add grid
  grid(col = "gray90", lty = 1)
}


# Helper: Plot deviance panel
.plot_deviance_panel <- function(profile_data, param_values, mle_val,
                                 add_ci, ci_level, x, parameter,
                                 xlab, ylab, main, col, lwd, ...) {
  plot(param_values, profile_data$deviance,
    type = "l", lwd = lwd, col = col,
    xlab = xlab, ylab = ylab, main = main,
    las = 1, ...
  )

  # Add reference lines
  abline(h = 0, col = "gray40", lty = 1, lwd = 1)
  abline(v = mle_val, col = "red", lty = 2, lwd = 1.5)

  # Add confidence interval
  if (add_ci) {
    ci <- x$confint(parameter, level = ci_level)
    abline(v = ci, col = "blue", lty = 2, lwd = 1.5)

    # Add horizontal line at chi-square critical value
    chi2_crit <- qchisq(ci_level, df = 1)
    abline(h = chi2_crit, col = "gray60", lty = 3, lwd = 1)

    legend("topright",
      legend = c("MLE", sprintf("%d%% CI", round(ci_level * 100))),
      col = c("red", "blue"), lty = 2, lwd = 1.5, bty = "n"
    )
  }

  grid(col = "gray90", lty = 1)
}


# Null coalescing operator
`%||%` <- function(a, b) if (is.null(a)) b else a


# =============================================================================
# PLOT ALL PARAMETERS AT ONCE
# =============================================================================

#' Plot likelihood for all parameters
#'
#' @param x A \code{likelihood_function} object
#' @param type Type of plot: \code{"likelihood"} or \code{"deviance"}
#' @param ci_level Confidence level for intervals (default: 0.95)
#' @param n_points Number of parameter values for profile likelihood evaluation
#' @param ncol Number of columns in the plot grid. If \code{NULL}, automatically
#'   determined as \code{ceiling(sqrt(n_parameters))}
#' @param ... Additional arguments passed to \code{\link[=plot.likelihood_function]{plot}}
#'
#' @return Invisibly returns \code{NULL}
#'
#' @details Creates a multi-panel plot with one panel per parameter. Useful for
#'   visualizing all likelihood functions simultaneously for comparison.
#'
#' @export
plot_all_parameters <- function(x,
                                type = c("likelihood", "deviance"),
                                ci_level = 0.95,
                                n_points = 100,
                                ncol = NULL,
                                ...) {
  type <- match.arg(type)

  n_params <- length(x$param_names)

  # Determine grid layout
  if (is.null(ncol)) {
    ncol <- ceiling(sqrt(n_params))
  }
  nrow <- ceiling(n_params / ncol)

  par(mfrow = c(nrow, ncol), mar = c(4, 4, 2, 1))

  for (param in x$param_names) {
    plot(x,
      parameter = param,
      type = type,
      ci_level = ci_level,
      n_points = n_points,
      main = param,
      ...
    )
  }

  par(mfrow = c(1, 1))

  invisible(NULL)
}


# =============================================================================
# COMPARE MULTIPLE CONFIDENCE LEVELS
# =============================================================================

#' Plot likelihood with multiple confidence levels
#'
#' @param x A \code{likelihood_function} object
#' @param parameter Name of the parameter to plot. If \code{NULL}, uses the first parameter.
#' @param ci_levels Numeric vector of confidence levels to display (default: 50%, 68%, 90%, 95%, 99%)
#' @param n_points Number of parameter values for profile likelihood evaluation
#' @param colors Character vector of colors for each confidence level. If \code{NULL},
#'   automatically generated as a gradient from blue to red.
#' @param ... Additional arguments passed to \code{\link{plot}}
#'
#' @return Invisibly returns \code{NULL}
#'
#' @details Plots a single likelihood function with colored interval bands at multiple
#'   confidence levels, allowing visual comparison of interval widths and how confidence
#'   intervals change with level.
#'
#' @export
plot_ci_levels <- function(x,
                           parameter = NULL,
                           ci_levels = c(0.50, 0.68, 0.90, 0.95, 0.99),
                           n_points = 200,
                           colors = NULL,
                           ...) {
  if (is.null(parameter)) {
    parameter <- x$param_names[1]
  }

  param_idx <- which(x$param_names == parameter)
  mle_val <- x$mle[param_idx]

  # Determine interval
  se <- sqrt(diag(vcov(x)))[param_idx]
  interval <- c(mle_val - 5 * se, mle_val + 5 * se)
  param_values <- seq(interval[1], interval[2], length.out = n_points)

  # Profile
  profile_data <- x$profile(parameter, param_values)
  rel_lik <- exp(profile_data$loglik - max(profile_data$loglik))

  # Colors
  if (is.null(colors)) {
    colors <- colorRampPalette(c("blue", "red"))(length(ci_levels))
  }

  # Plot
  plot(param_values, rel_lik,
    type = "l", lwd = 3,
    xlab = parameter, ylab = "Relative Likelihood",
    main = sprintf("Multiple Confidence Levels: %s", parameter),
    las = 1, ...
  )

  abline(v = mle_val, col = "black", lty = 1, lwd = 2)

  # Add each CI level
  for (i in seq_along(ci_levels)) {
    ci <- x$confint(parameter, level = ci_levels[i])
    abline(v = ci, col = colors[i], lty = 2, lwd = 1.5)

    # Add horizontal cutoff line
    cutoff <- exp(-qchisq(ci_levels[i], 1) / 2)
    segments(ci[1], cutoff, ci[2], cutoff,
      col = colors[i], lwd = 2
    )
  }

  # Legend
  legend("topright",
    legend = c("MLE", sprintf("%d%%", round(ci_levels * 100))),
    col = c("black", colors),
    lty = c(1, rep(2, length(ci_levels))),
    lwd = c(2, rep(1.5, length(ci_levels))),
    bty = "n"
  )

  grid(col = "gray90")

  invisible(NULL)
}


# =============================================================================
# COMPARE PROFILE VS WALD INTERVALS
# =============================================================================

#' Compare profile and Wald confidence intervals
#'
#' @param x A \code{likelihood_function} object
#' @param parameter Name of the parameter to plot. If \code{NULL}, uses the first parameter.
#' @param ci_level Confidence level (default: 0.95)
#' @param n_points Number of parameter values for profile likelihood evaluation
#' @param ... Additional arguments passed to \code{\link{plot}}
#'
#' @return Invisibly returns a list with elements \code{profile} and \code{wald}
#'   containing the respective confidence interval bounds
#'
#' @details Plots the profile likelihood with both profile-based and Wald (normal approximation)
#'   confidence interval bounds overlaid for comparison. Differences between the two methods
#'   indicate departures from normality or asymmetry in the likelihood.
#'
#' @export
plot_profile_vs_wald <- function(x,
                                 parameter = NULL,
                                 ci_level = 0.95,
                                 n_points = 200,
                                 ...) {
  if (is.null(parameter)) {
    parameter <- x$param_names[1]
  }

  param_idx <- which(x$param_names == parameter)
  mle_val <- x$mle[param_idx]
  se <- sqrt(diag(vcov(x)))[param_idx]

  # Intervals
  interval <- c(mle_val - 5 * se, mle_val + 5 * se)
  param_values <- seq(interval[1], interval[2], length.out = n_points)

  # Profile
  profile_data <- x$profile(parameter, param_values)
  rel_lik <- exp(profile_data$loglik - max(profile_data$loglik))

  # Confidence intervals
  ci_profile <- x$confint(parameter, level = ci_level)
  ci_wald <- c(
    mle_val - qnorm(1 - (1 - ci_level) / 2) * se,
    mle_val + qnorm(1 - (1 - ci_level) / 2) * se
  )

  # Plot
  plot(param_values, rel_lik,
    type = "l", lwd = 3,
    xlab = parameter, ylab = "Relative Likelihood",
    main = sprintf("Profile vs Wald CIs: %s", parameter),
    las = 1, ...
  )

  abline(v = mle_val, col = "black", lty = 1, lwd = 2)

  # Profile CI
  abline(v = ci_profile, col = "blue", lty = 2, lwd = 2)

  # Wald CI
  abline(v = ci_wald, col = "red", lty = 2, lwd = 2)

  # Add horizontal cutoff
  cutoff <- exp(-qchisq(ci_level, 1) / 2)
  abline(h = cutoff, col = "gray60", lty = 3)

  # Legend with interval widths
  profile_width <- diff(ci_profile)
  wald_width <- diff(ci_wald)

  legend("topright",
    legend = c(
      "MLE",
      sprintf("Profile (width: %.3f)", profile_width),
      sprintf("Wald (width: %.3f)", wald_width)
    ),
    col = c("black", "blue", "red"),
    lty = c(1, 2, 2),
    lwd = c(2, 2, 2),
    bty = "n"
  )

  grid(col = "gray90")

  invisible(list(profile = ci_profile, wald = ci_wald))
}


# =============================================================================
# GGPLOT2 VERSIONS (publication quality)
# =============================================================================

#' Plot likelihood using ggplot2
#'
#' @param x A \code{likelihood_function} object
#' @param parameter Name of the parameter to plot. If \code{NULL}, uses the first parameter.
#' @param type Type of plot: \code{"likelihood"}, \code{"deviance"}, or \code{"both"}
#' @param ci_level Confidence level (default: 0.95)
#' @param n_points Number of parameter values for profile likelihood evaluation
#' @param theme ggplot2 theme to apply. Options: \code{"minimal"} (default), \code{"classic"},
#'   \code{"bw"}, or any other ggplot2 theme object.
#'
#' @return A ggplot object (or patchwork composition for \code{type = "both"})
#'
#' @details Creates publication-quality plots using ggplot2 with subtitle showing
#'   the confidence interval. For \code{type = "both"}, requires the patchwork package
#'   for combining plots vertically. If patchwork is not installed, only the likelihood
#'   plot is returned with a message.
#'
#' @seealso \code{\link[=plot.likelihood_function]{plot}} for base graphics version,
#'   \code{\link{plotly_likelihood}} for interactive plots
#'
#' @export
ggplot_likelihood <- function(x,
                              parameter = NULL,
                              type = c("likelihood", "deviance", "both"),
                              ci_level = 0.95,
                              n_points = 200,
                              theme = "minimal") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' required for this function", call. = FALSE)
  }

  type <- match.arg(type)

  if (is.null(parameter)) {
    parameter <- x$param_names[1]
  }

  param_idx <- which(x$param_names == parameter)
  mle_val <- x$mle[param_idx]
  se <- sqrt(diag(vcov(x)))[param_idx]

  # Generate data
  interval <- c(mle_val - 5 * se, mle_val + 5 * se)
  param_values <- seq(interval[1], interval[2], length.out = n_points)
  profile_data <- x$profile(parameter, param_values)

  # Add relative likelihood
  profile_data$rel_likelihood <- exp(profile_data$loglik - max(profile_data$loglik))

  # Get CI
  ci <- x$confint(parameter, level = ci_level)
  ci_cutoff <- exp(-qchisq(ci_level, 1) / 2)
  chi2_crit <- qchisq(ci_level, df = 1)

  if (type == "likelihood" || type == "both") {
    p_lik <- ggplot2::ggplot(profile_data, ggplot2::aes(x = value, y = rel_likelihood)) +
      ggplot2::geom_line(linewidth = 1.2, color = "black") +
      ggplot2::geom_vline(
        xintercept = mle_val, linetype = "dashed",
        color = "red", linewidth = 1
      ) +
      ggplot2::geom_vline(
        xintercept = ci, linetype = "dashed",
        color = "blue", linewidth = 0.8
      ) +
      ggplot2::geom_hline(
        yintercept = ci_cutoff, linetype = "dotted",
        color = "gray50", linewidth = 0.8
      ) +
      ggplot2::labs(
        x = parameter,
        y = "Relative Likelihood",
        title = sprintf("Likelihood Function: %s", parameter),
        subtitle = sprintf(
          "%d%% CI: [%.3f, %.3f]",
          round(ci_level * 100), ci[1], ci[2]
        )
      ) +
      ggplot2::annotate("text",
        x = mle_val, y = 1.05, label = "MLE",
        color = "red", size = 4
      )
  }

  if (type == "deviance" || type == "both") {
    p_dev <- ggplot2::ggplot(profile_data, ggplot2::aes(x = value, y = deviance)) +
      ggplot2::geom_line(linewidth = 1.2, color = "black") +
      ggplot2::geom_vline(
        xintercept = mle_val, linetype = "dashed",
        color = "red", linewidth = 1
      ) +
      ggplot2::geom_vline(
        xintercept = ci, linetype = "dashed",
        color = "blue", linewidth = 0.8
      ) +
      ggplot2::geom_hline(
        yintercept = 0, linetype = "solid",
        color = "gray40", linewidth = 0.8
      ) +
      ggplot2::geom_hline(
        yintercept = chi2_crit, linetype = "dotted",
        color = "gray50", linewidth = 0.8
      ) +
      ggplot2::labs(
        x = parameter,
        y = "Profile Deviance",
        title = sprintf("Profile Deviance: %s", parameter),
        subtitle = sprintf(
          "%d%% CI: [%.3f, %.3f]",
          round(ci_level * 100), ci[1], ci[2]
        )
      )
  }

  # Apply theme
  apply_theme <- function(p) {
    p <- p + switch(theme,
      "minimal" = ggplot2::theme_minimal(),
      "classic" = ggplot2::theme_classic(),
      "bw" = ggplot2::theme_bw(),
      ggplot2::theme_minimal()
    )
    p + ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 11),
      axis.title = ggplot2::element_text(size = 12),
      axis.text = ggplot2::element_text(size = 10)
    )
  }

  if (type == "both") {
    if (!requireNamespace("patchwork", quietly = TRUE)) {
      message("Install 'patchwork' for combined plots. Showing likelihood only.")
      return(apply_theme(p_lik))
    }
    return(apply_theme(p_lik) / apply_theme(p_dev))
  } else if (type == "likelihood") {
    return(apply_theme(p_lik))
  } else {
    return(apply_theme(p_dev))
  }
}


# =============================================================================
# INTERACTIVE PLOTS (plotly)
# =============================================================================

#' Interactive likelihood plot
#'
#' @param x A \code{likelihood_function} object
#' @param parameter Name of the parameter to plot. If \code{NULL}, uses the first parameter.
#' @param ci_level Confidence level (default: 0.95)
#' @param n_points Number of parameter values for profile likelihood evaluation
#'
#' @return A plotly object with interactive hover tooltips
#'
#' @details Creates an interactive plot using plotly, allowing users to hover over
#'   the likelihood curve to see exact values. Includes confidence interval bounds
#'   and MLE reference line.
#'
#' @seealso \code{\link[=plot.likelihood_function]{plot}} for base graphics version,
#'   \code{\link{ggplot_likelihood}} for ggplot2 version
#'
#' @export
plotly_likelihood <- function(x,
                              parameter = NULL,
                              ci_level = 0.95,
                              n_points = 200) {
  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("Package 'plotly' required for this function", call. = FALSE)
  }

  if (is.null(parameter)) {
    parameter <- x$param_names[1]
  }

  param_idx <- which(x$param_names == parameter)
  mle_val <- x$mle[param_idx]
  se <- sqrt(diag(vcov(x)))[param_idx]

  # Generate data
  interval <- c(mle_val - 5 * se, mle_val + 5 * se)
  param_values <- seq(interval[1], interval[2], length.out = n_points)
  profile_data <- x$profile(parameter, param_values)
  profile_data$rel_likelihood <- exp(profile_data$loglik - max(profile_data$loglik))

  # Get CI
  ci <- x$confint(parameter, level = ci_level)

  # Create plotly
  p <- plotly::plot_ly() %>%
    plotly::add_trace(
      data = profile_data,
      x = ~value,
      y = ~rel_likelihood,
      type = "scatter",
      mode = "lines",
      name = "Likelihood",
      line = list(color = "black", width = 2),
      hovertemplate = paste(
        "<b>%{x:.4f}</b><br>",
        "Rel. Likelihood: %{y:.4f}<br>",
        "<extra></extra>"
      )
    ) %>%
    plotly::add_trace(
      x = c(mle_val, mle_val),
      y = c(0, 1),
      type = "scatter",
      mode = "lines",
      name = "MLE",
      line = list(color = "red", width = 2, dash = "dash"),
      hoverinfo = "skip"
    ) %>%
    plotly::add_trace(
      x = c(ci[1], ci[1]),
      y = c(0, 1),
      type = "scatter",
      mode = "lines",
      name = sprintf("%d%% CI", round(ci_level * 100)),
      line = list(color = "blue", width = 1.5, dash = "dash"),
      hoverinfo = "skip",
      showlegend = TRUE
    ) %>%
    plotly::add_trace(
      x = c(ci[2], ci[2]),
      y = c(0, 1),
      type = "scatter",
      mode = "lines",
      name = sprintf("%d%% CI", round(ci_level * 100)),
      line = list(color = "blue", width = 1.5, dash = "dash"),
      hoverinfo = "skip",
      showlegend = FALSE
    ) %>%
    plotly::layout(
      title = sprintf("Interactive Likelihood: %s", parameter),
      xaxis = list(title = parameter),
      yaxis = list(title = "Relative Likelihood"),
      hovermode = "x unified"
    )

  p
}

# Declare global variables used in aes() to suppress R CMD check notes
utils::globalVariables(c("rel_likelihood", "deviance", "value"))
