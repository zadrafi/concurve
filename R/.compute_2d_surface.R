# =============================================================================
# 2D LIKELIHOOD SURFACE PLOTS (CONTOURS, HEATMAPS, 3D)
# =============================================================================

#' Compute 2D likelihood surface
#' @keywords internal
.compute_2d_surface <- function(lik, param1, param2,
                                n_points = 50,
                                interval1 = NULL,
                                interval2 = NULL) {
  # Get parameter indices
  idx1 <- which(lik$param_names == param1)
  idx2 <- which(lik$param_names == param2)

  if (length(idx1) == 0 || length(idx2) == 0) {
    stop("Parameters not found in model", call. = FALSE)
  }

  # Get MLEs and SEs
  mle1 <- lik$mle[idx1]
  mle2 <- lik$mle[idx2]
  vcov_mat <- vcov(lik)
  se1 <- sqrt(vcov_mat[idx1, idx1])
  se2 <- sqrt(vcov_mat[idx2, idx2])

  # Set intervals if not provided
  if (is.null(interval1)) {
    interval1 <- c(mle1 - 4 * se1, mle1 + 4 * se1)
  }
  if (is.null(interval2)) {
    interval2 <- c(mle2 - 4 * se2, mle2 + 4 * se2)
  }

  # Create grid
  param1_vals <- seq(interval1[1], interval1[2], length.out = n_points)
  param2_vals <- seq(interval2[1], interval2[2], length.out = n_points)
  grid <- expand.grid(param1 = param1_vals, param2 = param2_vals)

  # Compute log-likelihood for each grid point
  n_params <- length(lik$mle)
  other_params <- setdiff(seq_len(n_params), c(idx1, idx2))

  loglik_vals <- numeric(nrow(grid))

  for (i in seq_len(nrow(grid))) {
    # Fix param1 and param2, optimize over others
    if (length(other_params) > 0) {
      opt <- optim(
        par = lik$mle[other_params],
        fn = function(other) {
          full_params <- lik$mle
          full_params[idx1] <- grid$param1[i]
          full_params[idx2] <- grid$param2[i]
          full_params[other_params] <- other
          -lik$loglik(full_params)
        },
        method = "BFGS"
      )
      loglik_vals[i] <- -opt$value
    } else {
      # Only 2 parameters in model
      full_params <- numeric(n_params)
      full_params[idx1] <- grid$param1[i]
      full_params[idx2] <- grid$param2[i]
      loglik_vals[i] <- lik$loglik(full_params)
    }
  }

  # Create matrix form for contour/image plots
  loglik_matrix <- matrix(loglik_vals, nrow = n_points, ncol = n_points)

  list(
    param1_vals = param1_vals,
    param2_vals = param2_vals,
    loglik_matrix = loglik_matrix,
    mle1 = mle1,
    mle2 = mle2,
    se1 = se1,
    se2 = se2,
    grid = grid,
    loglik_vals = loglik_vals
  )
}


#' Plot 2D likelihood contours
#' @export
plot_2d_contour <- function(lik,
                            param1,
                            param2,
                            n_points = 50,
                            ci_levels = c(0.50, 0.90, 0.95, 0.99),
                            add_mle = TRUE,
                            add_labels = TRUE,
                            colors = NULL,
                            filled = FALSE,
                            ...) {
  # Compute surface
  surface <- .compute_2d_surface(lik, param1, param2, n_points)

  # Relative likelihood
  rel_loglik <- surface$loglik_matrix - max(surface$loglik_matrix)
  rel_lik <- exp(rel_loglik)

  # Contour levels for CI
  # For 2 parameters, use chi-square with df=2
  contour_levels <- exp(-qchisq(ci_levels, df = 2) / 2)

  # Colors
  if (is.null(colors)) {
    colors <- colorRampPalette(c("blue", "cyan", "yellow", "red"))(100)
  }

  # Plot
  if (filled) {
    filled.contour(
      x = surface$param1_vals,
      y = surface$param2_vals,
      z = rel_lik,
      levels = seq(0, 1, length.out = 20),
      col = colors,
      xlab = param1,
      ylab = param2,
      main = sprintf("2D Likelihood Surface: %s vs %s", param1, param2),
      plot.axes = {
        axis(1)
        axis(2)
        contour(surface$param1_vals, surface$param2_vals, rel_lik,
          levels = contour_levels,
          add = TRUE, lwd = 2, labcex = 1
        )
        if (add_mle) {
          points(surface$mle1, surface$mle2, pch = 19, cex = 2, col = "red")
        }
      }
    )
  } else {
    # Standard contour plot
    contour(
      x = surface$param1_vals,
      y = surface$param2_vals,
      z = rel_lik,
      levels = contour_levels,
      labels = if (add_labels) paste0(ci_levels * 100, "%") else "",
      xlab = param1,
      ylab = param2,
      main = sprintf("2D Likelihood Contours: %s vs %s", param1, param2),
      lwd = 2,
      labcex = 1,
      ...
    )

    # Add MLE point
    if (add_mle) {
      points(surface$mle1, surface$mle2, pch = 19, cex = 2, col = "red")
      if (add_labels) {
        text(surface$mle1, surface$mle2, "MLE", pos = 3, col = "red", font = 2)
      }
    }

    # Add grid
    grid(col = "gray90", lty = 1)
  }

  invisible(surface)
}


#' Plot 2D likelihood heatmap
#' @export
plot_2d_heatmap <- function(lik,
                            param1,
                            param2,
                            n_points = 50,
                            add_contours = TRUE,
                            ci_levels = c(0.90, 0.95, 0.99),
                            add_mle = TRUE,
                            color_scheme = "viridis",
                            ...) {
  # Compute surface
  surface <- .compute_2d_surface(lik, param1, param2, n_points)

  # Relative likelihood
  rel_loglik <- surface$loglik_matrix - max(surface$loglik_matrix)
  rel_lik <- exp(rel_loglik)

  # Color palette
  colors <- switch(color_scheme,
    "viridis" = hcl.colors(100, "Viridis"),
    "heat" = heat.colors(100, rev = TRUE),
    "terrain" = terrain.colors(100, rev = TRUE),
    "blues" = colorRampPalette(c("white", "lightblue", "darkblue"))(100),
    "reds" = colorRampPalette(c("white", "pink", "darkred"))(100),
    hcl.colors(100, "Viridis")
  )

  # Plot heatmap
  image(
    x = surface$param1_vals,
    y = surface$param2_vals,
    z = rel_lik,
    col = colors,
    xlab = param1,
    ylab = param2,
    main = sprintf("2D Likelihood Heatmap: %s vs %s", param1, param2),
    las = 1,
    ...
  )

  # Add contours
  if (add_contours) {
    contour_levels <- exp(-qchisq(ci_levels, df = 2) / 2)
    contour(
      surface$param1_vals,
      surface$param2_vals,
      rel_lik,
      levels = contour_levels,
      add = TRUE,
      lwd = 2,
      col = "white",
      labcex = 1
    )
  }

  # Add MLE
  if (add_mle) {
    points(surface$mle1, surface$mle2, pch = 19, cex = 2, col = "red")
    points(surface$mle1, surface$mle2, pch = 1, cex = 2.5, col = "white", lwd = 2)
  }

  invisible(surface)
}


#' Plot 3D likelihood surface
#' @export
plot_3d_surface <- function(lik,
                            param1,
                            param2,
                            n_points = 40,
                            color_scheme = "viridis",
                            theta = 30,
                            phi = 30,
                            add_contours = TRUE,
                            ...) {
  # Compute surface
  surface <- .compute_2d_surface(lik, param1, param2, n_points)

  # Relative likelihood
  rel_loglik <- surface$loglik_matrix - max(surface$loglik_matrix)
  rel_lik <- exp(rel_loglik)

  # Color palette
  colors <- switch(color_scheme,
    "viridis" = hcl.colors(100, "Viridis"),
    "heat" = heat.colors(100, rev = TRUE),
    "terrain" = terrain.colors(100),
    hcl.colors(100, "Viridis")
  )

  # Get colors for each facet
  nrz <- nrow(rel_lik)
  ncz <- ncol(rel_lik)
  zfacet <- rel_lik[-1, -1] + rel_lik[-1, -ncz] +
    rel_lik[-nrz, -1] + rel_lik[-nrz, -ncz]
  facetcol <- cut(zfacet, 100)

  # 3D surface plot
  persp(
    x = surface$param1_vals,
    y = surface$param2_vals,
    z = rel_lik,
    theta = theta,
    phi = phi,
    col = colors[facetcol],
    xlab = param1,
    ylab = param2,
    zlab = "Relative Likelihood",
    main = sprintf("3D Likelihood Surface: %s vs %s", param1, param2),
    ticktype = "detailed",
    shade = 0.5,
    border = NA,
    ...
  )

  invisible(surface)
}


#' Interactive 3D plot using plotly
#' @export
plotly_3d_surface <- function(lik,
                              param1,
                              param2,
                              n_points = 50,
                              color_scheme = "Viridis") {
  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("Package 'plotly' required for this function", call. = FALSE)
  }

  # Compute surface
  surface <- .compute_2d_surface(lik, param1, param2, n_points)

  # Relative likelihood
  rel_loglik <- surface$loglik_matrix - max(surface$loglik_matrix)
  rel_lik <- exp(rel_loglik)

  # Create plotly
  p <- plotly::plot_ly() %>%
    plotly::add_surface(
      x = surface$param1_vals,
      y = surface$param2_vals,
      z = rel_lik,
      colorscale = color_scheme,
      colorbar = list(title = "Rel. Lik")
    ) %>%
    plotly::add_trace(
      x = surface$mle1,
      y = surface$mle2,
      z = 1,
      type = "scatter3d",
      mode = "markers",
      marker = list(size = 8, color = "red"),
      name = "MLE",
      showlegend = TRUE
    ) %>%
    plotly::layout(
      title = sprintf("Interactive 3D Surface: %s vs %s", param1, param2),
      scene = list(
        xaxis = list(title = param1),
        yaxis = list(title = param2),
        zaxis = list(title = "Relative Likelihood")
      )
    )

  p
}


#' Plot 2D contours with marginal distributions
#' @export
plot_2d_with_marginals <- function(lik,
                                   param1,
                                   param2,
                                   n_points = 50,
                                   ci_levels = c(0.90, 0.95, 0.99),
                                   n_marginal_points = 200) {
  # Set up layout
  layout(
    matrix(c(
      2, 0,
      1, 3
    ), 2, 2, byrow = TRUE),
    widths = c(4, 1), heights = c(1, 4)
  )

  # Store original par
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))

  # Get surface data
  surface <- .compute_2d_surface(lik, param1, param2, n_points)
  rel_loglik <- surface$loglik_matrix - max(surface$loglik_matrix)
  rel_lik <- exp(rel_loglik)

  # 1. Main contour plot (bottom left)
  par(mar = c(4, 4, 1, 1))
  contour_levels <- exp(-qchisq(ci_levels, df = 2) / 2)

  contour(
    x = surface$param1_vals,
    y = surface$param2_vals,
    z = rel_lik,
    levels = contour_levels,
    labels = paste0(ci_levels * 100, "%"),
    xlab = param1,
    ylab = param2,
    lwd = 2,
    labcex = 1
  )
  points(surface$mle1, surface$mle2, pch = 19, cex = 2, col = "red")
  grid(col = "gray90")

  # 2. Top marginal (param1)
  par(mar = c(1, 4, 2, 1))
  marg1_vals <- seq(surface$param1_vals[1],
    surface$param1_vals[length(surface$param1_vals)],
    length.out = n_marginal_points
  )
  marg1_profile <- lik$profile(param1, marg1_vals)
  marg1_rel <- exp(marg1_profile$loglik - max(marg1_profile$loglik))

  plot(marg1_vals, marg1_rel,
    type = "l", lwd = 2,
    xlab = "", ylab = "Rel. Lik", main = param1,
    xaxt = "n", las = 1
  )
  abline(v = surface$mle1, col = "red", lty = 2)

  # 3. Right marginal (param2)
  par(mar = c(4, 1, 1, 2))
  marg2_vals <- seq(surface$param2_vals[1],
    surface$param2_vals[length(surface$param2_vals)],
    length.out = n_marginal_points
  )
  marg2_profile <- lik$profile(param2, marg2_vals)
  marg2_rel <- exp(marg2_profile$loglik - max(marg2_profile$loglik))

  plot(marg2_rel, marg2_vals,
    type = "l", lwd = 2,
    xlab = "Rel. Lik", ylab = "", yaxt = "n"
  )
  abline(h = surface$mle2, col = "red", lty = 2)

  # Reset layout
  layout(1)

  invisible(surface)
}


#' ggplot2 version of 2D contour plot
#' @export
ggplot_2d_contour <- function(lik,
                              param1,
                              param2,
                              n_points = 50,
                              ci_levels = c(0.50, 0.90, 0.95, 0.99),
                              filled = TRUE,
                              theme = "minimal") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' required for this function", call. = FALSE)
  }

  # Compute surface
  surface <- .compute_2d_surface(lik, param1, param2, n_points)

  # Create data frame
  plot_data <- data.frame(
    param1 = surface$grid$param1,
    param2 = surface$grid$param2,
    loglik = surface$loglik_vals
  )

  # Relative likelihood
  plot_data$rel_lik <- exp(plot_data$loglik - max(plot_data$loglik))

  # Contour levels
  contour_levels <- exp(-qchisq(ci_levels, df = 2) / 2)

  # Base plot
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = param1, y = param2))

  if (filled) {
    p <- p +
      ggplot2::geom_tile(ggplot2::aes(fill = rel_lik)) +
      ggplot2::scale_fill_viridis_c(
        name = "Rel. Lik",
        option = "viridis"
      )
  }

  p <- p +
    ggplot2::geom_contour(ggplot2::aes(z = rel_lik),
      breaks = contour_levels,
      color = if (filled) "white" else "black",
      linewidth = 1
    ) +
    ggplot2::geom_point(
      data = data.frame(x = surface$mle1, y = surface$mle2),
      ggplot2::aes(x = x, y = y),
      color = "red", size = 4, shape = 19
    ) +
    ggplot2::labs(
      x = param1,
      y = param2,
      title = sprintf("2D Likelihood: %s vs %s", param1, param2)
    )

  # Apply theme
  p <- p + switch(theme,
    "minimal" = ggplot2::theme_minimal(),
    "classic" = ggplot2::theme_classic(),
    "bw" = ggplot2::theme_bw(),
    ggplot2::theme_minimal()
  )

  p + ggplot2::theme(
    plot.title = ggplot2::element_text(size = 14, face = "bold"),
    axis.title = ggplot2::element_text(size = 12),
    legend.position = "right"
  )
}


#' Interactive 2D contour with plotly
#' @export
plotly_2d_contour <- function(lik,
                              param1,
                              param2,
                              n_points = 50,
                              ci_levels = c(0.50, 0.90, 0.95, 0.99)) {
  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("Package 'plotly' required for this function", call. = FALSE)
  }

  # Compute surface
  surface <- .compute_2d_surface(lik, param1, param2, n_points)

  # Relative likelihood
  rel_loglik <- surface$loglik_matrix - max(surface$loglik_matrix)
  rel_lik <- exp(rel_loglik)

  # Contour levels
  contour_levels <- exp(-qchisq(ci_levels, df = 2) / 2)

  # Create plotly
  p <- plotly::plot_ly() %>%
    plotly::add_contour(
      x = surface$param1_vals,
      y = surface$param2_vals,
      z = rel_lik,
      contours = list(
        start = min(contour_levels),
        end = 1,
        size = (1 - min(contour_levels)) / 20,
        showlabels = TRUE
      ),
      colorscale = "Viridis",
      colorbar = list(title = "Rel. Lik")
    ) %>%
    plotly::add_trace(
      x = surface$mle1,
      y = surface$mle2,
      type = "scatter",
      mode = "markers",
      marker = list(size = 12, color = "red", symbol = "x"),
      name = "MLE",
      showlegend = TRUE
    ) %>%
    plotly::layout(
      title = sprintf("Interactive 2D Contours: %s vs %s", param1, param2),
      xaxis = list(title = param1),
      yaxis = list(title = param2)
    )

  p
}


# =============================================================================
# SPECIALIZED VISUALIZATIONS
# =============================================================================

#' Plot correlation ellipse from likelihood
#' @export
plot_correlation_ellipse <- function(lik,
                                     param1,
                                     param2,
                                     ci_levels = c(0.50, 0.90, 0.95),
                                     colors = NULL,
                                     add_axes = TRUE,
                                     ...) {
  # Get indices
  idx1 <- which(lik$param_names == param1)
  idx2 <- which(lik$param_names == param2)

  # Get covariance matrix
  vcov_mat <- vcov(lik)
  cov_sub <- vcov_mat[c(idx1, idx2), c(idx1, idx2)]

  # Get means
  means <- lik$mle[c(idx1, idx2)]

  # Colors
  if (is.null(colors)) {
    colors <- rainbow(length(ci_levels), alpha = 0.3)
  }

  # Set up plot
  plot(means[1], means[2],
    type = "n",
    xlim = means[1] + c(-4, 4) * sqrt(cov_sub[1, 1]),
    ylim = means[2] + c(-4, 4) * sqrt(cov_sub[2, 2]),
    xlab = param1, ylab = param2,
    main = "Likelihood Correlation Ellipses",
    las = 1, ...
  )

  # Draw ellipses for each CI level
  for (i in seq_along(ci_levels)) {
    # Chi-square quantile for 2 df
    radius <- sqrt(qchisq(ci_levels[i], df = 2))

    # Generate ellipse points
    theta <- seq(0, 2 * pi, length.out = 100)
    circle <- cbind(cos(theta), sin(theta))

    # Eigen decomposition for rotation
    eigen_decomp <- eigen(cov_sub)
    ellipse <- t(means + radius * eigen_decomp$vectors %*%
      diag(sqrt(eigen_decomp$values)) %*% t(circle))

    # Plot
    polygon(ellipse, border = colors[i], col = colors[i], lwd = 2)
  }

  # Add MLE point
  points(means[1], means[2], pch = 19, cex = 2, col = "red")

  # Add axes
  if (add_axes) {
    abline(v = means[1], lty = 2, col = "gray50")
    abline(h = means[2], lty = 2, col = "gray50")
  }

  # Legend
  legend("topright",
    legend = paste0(ci_levels * 100, "%"),
    fill = colors,
    border = colors,
    bty = "n"
  )

  grid(col = "gray90")

  invisible(NULL)
}


#' Profile trace plot (parameter path during profiling)
#' @export
plot_profile_trace <- function(lik,
                               param1,
                               param2,
                               n_points = 30,
                               ...) {
  # Get indices
  idx1 <- which(lik$param_names == param1)
  idx2 <- which(lik$param_names == param2)

  mle1 <- lik$mle[idx1]
  mle2 <- lik$mle[idx2]

  se1 <- sqrt(diag(vcov(lik)))[idx1]
  se2 <- sqrt(diag(vcov(lik)))[idx2]

  # Generate param1 values
  param1_vals <- seq(mle1 - 3 * se1, mle1 + 3 * se1, length.out = n_points)

  # For each param1 value, find optimal param2
  param2_profile <- numeric(n_points)

  n_params <- length(lik$mle)
  other_params <- setdiff(seq_len(n_params), c(idx1, idx2))

  for (i in seq_along(param1_vals)) {
    if (length(other_params) > 0) {
      # Optimize over param2 and other parameters
      opt <- optim(
        par = c(lik$mle[idx2], lik$mle[other_params]),
        fn = function(par) {
          full_params <- lik$mle
          full_params[idx1] <- param1_vals[i]
          full_params[idx2] <- par[1]
          if (length(other_params) > 0) {
            full_params[other_params] <- par[-1]
          }
          -lik$loglik(full_params)
        },
        method = "BFGS"
      )
      param2_profile[i] <- opt$par[1]
    } else {
      # Only optimize param2
      opt <- optimize(
        f = function(p2) {
          full_params <- numeric(n_params)
          full_params[idx1] <- param1_vals[i]
          full_params[idx2] <- p2
          -lik$loglik(full_params)
        },
        interval = c(mle2 - 5 * se2, mle2 + 5 * se2)
      )
      param2_profile[i] <- opt$minimum
    }
  }

  # Plot
  plot(param1_vals, param2_profile,
    type = "l", lwd = 2,
    xlab = param1, ylab = param2,
    main = "Profile Likelihood Trace",
    las = 1, ...
  )

  points(mle1, mle2, pch = 19, cex = 2, col = "red")
  text(mle1, mle2, "MLE", pos = 3, col = "red", font = 2)

  grid(col = "gray90")

  invisible(data.frame(param1 = param1_vals, param2 = param2_profile))
}


#' Pairwise likelihood contours for all parameter combinations
#' @export
plot_all_pairwise_contours <- function(lik,
                                       n_points = 30,
                                       ci_levels = c(0.90, 0.95)) {
  n_params <- length(lik$param_names)

  if (n_params < 2) {
    stop("Need at least 2 parameters for pairwise plots", call. = FALSE)
  }

  # Set up grid
  pairs <- combn(n_params, 2)
  n_pairs <- ncol(pairs)

  n_cols <- ceiling(sqrt(n_pairs))
  n_rows <- ceiling(n_pairs / n_cols)

  par(mfrow = c(n_rows, n_cols), mar = c(3, 3, 2, 1))

  for (i in seq_len(n_pairs)) {
    idx1 <- pairs[1, i]
    idx2 <- pairs[2, i]

    param1 <- lik$param_names[idx1]
    param2 <- lik$param_names[idx2]

    tryCatch(
      {
        surface <- .compute_2d_surface(lik, param1, param2, n_points)
        rel_loglik <- surface$loglik_matrix - max(surface$loglik_matrix)
        rel_lik <- exp(rel_loglik)

        contour_levels <- exp(-qchisq(ci_levels, df = 2) / 2)

        contour(
          x = surface$param1_vals,
          y = surface$param2_vals,
          z = rel_lik,
          levels = contour_levels,
          xlab = param1,
          ylab = param2,
          main = sprintf("%s vs %s", param1, param2),
          lwd = 1.5,
          cex.main = 0.9
        )

        points(surface$mle1, surface$mle2, pch = 19, cex = 1.5, col = "red")
      },
      error = function(e) {
        plot.new()
        text(0.5, 0.5, sprintf("Error:\n%s vs %s", param1, param2), cex = 0.8)
      }
    )
  }

  par(mfrow = c(1, 1))

  invisible(NULL)
}
