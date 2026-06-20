#' Plot Multiple Consonance Functions for Comparison
#'
#' Overlays multiple consonance or surprisal functions on a single plot
#' for direct visual comparison of effect estimates across groups,
#' time periods, or studies.
#'
#' @param ... Named concurve dataframes to compare. Names become legend labels.
#'   Alternatively, pass a single named list of dataframes.
#' @param type Character. Type of function to plot: "c" for consonance (default),
#'   "s" for surprisal.
#' @param measure Character. Scale type: "default" for linear, "ratio" for
#'   log scale (odds ratios, hazard ratios, etc.).
#' @param nullvalue Numeric. Value(s) to mark with vertical reference line(s).
#'   Use single value (e.g., 0) or vector for range (e.g., c(-0.5, 0.5)).
#' @param position Character. Orientation of consonance function: "pyramid"
#'   (default) or "inverted".
#' @param title Character. Plot title.
#' @param subtitle Character. Plot subtitle.
#' @param xaxis Character or expression. X-axis label.
#' @param yaxis1 Character or expression. Primary y-axis label.
#' @param yaxis2 Character. Secondary y-axis label (for CI levels).
#' @param colors Character vector. Colors for each curve. If NULL, uses
#'   colorblind-friendly palette.
#' @param alpha Numeric. Transparency for ribbon fill (0-1). Default is 0.15.
#' @param legend.position Character or numeric vector. Legend position.
#'   Default is "bottom".
#'
#' @return A ggplot2 object.
#'
#' @examples
#' \dontrun{
#' # Compare two studies
#' study1 <- curve_from_se(point = 0.5, se = 0.2, df = 50)
#' study2 <- curve_from_se(point = 0.8, se = 0.25, df = 80)
#'
#' plot_multi(
#'   "Study A" = study1[[1]],
#'   "Study B" = study2[[1]],
#'   nullvalue = 0,
#'   title = "Comparison of Treatment Effects"
#' )
#'
#' # Using a list
#' curves_list <- list("Group 1" = study1[[1]], "Group 2" = study2[[1]])
#' plot_multi(curves_list, type = "s", nullvalue = 0)
#' }
#'
#' @seealso [ggcurve()], [plot_compare()], [curve_compare()]
#' @importFrom ggplot2 ggplot aes geom_line geom_ribbon scale_color_manual
#' @importFrom ggplot2 scale_fill_manual scale_y_continuous scale_y_reverse
#' @importFrom ggplot2 scale_x_continuous scale_x_log10 labs theme_minimal theme
#' @importFrom ggplot2 element_text geom_vline annotate sec_axis guide_legend
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr bind_rows mutate
#' @importFrom scales pretty_breaks
#' @importFrom colorspace darken
#' @export

plot_multi <- function(...,
                       type = "c",
                       measure = "default",
                       nullvalue = NULL,
                       position = "pyramid",
                       title = "Comparison of Consonance Functions",
                       subtitle = "Functions display intervals at every level.",
                       xaxis = expression(theta == ~"Effect Size"),
                       yaxis1 = expression(paste(italic(p), "-value")),
                       yaxis2 = "Confidence Level (%)",
                       colors = NULL,
                       alpha = 0.15,
                       legend.position = "bottom") {
  # Handle input: either named args or a single list
  args <- list(...)
  if (length(args) == 1 && is.list(args[[1]]) && !is.data.frame(args[[1]])) {
    curves_list <- args[[1]]
  } else {
    curves_list <- args
  }

  # Validate inputs
  if (length(curves_list) == 0) {
    stop("Error: At least one concurve dataframe must be provided")
  }
  if (is.null(names(curves_list)) || any(names(curves_list) == "")) {
    names(curves_list) <- paste0("Curve ", seq_along(curves_list))
  }

  # Check all inputs are concurve dataframes
  for (nm in names(curves_list)) {
    if (!inherits(curves_list[[nm]], "concurve")) {
      # Try to use as-is if it has the right columns
      if (!all(c("lower.limit", "upper.limit", "pvalue") %in% names(curves_list[[nm]]))) {
        stop(paste0("Error: '", nm, "' must be a concurve dataframe with required columns"))
      }
    }
  }

  n_curves <- length(curves_list)

  # Default colorblind-friendly palette
  if (is.null(colors)) {
    default_colors <- c(
      "#0072B2", "#D55E00", "#009E73", "#CC79A7",
      "#F0E442", "#56B4E9", "#E69F00", "#000000"
    )
    colors <- default_colors[1:min(n_curves, length(default_colors))]
    if (n_curves > length(default_colors)) {
      colors <- c(colors, scales::hue_pal()(n_curves - length(default_colors)))
    }
  }

  # Combine dataframes with source identifier
  curves_combined <- dplyr::bind_rows(
    lapply(names(curves_list), function(nm) {
      df <- curves_list[[nm]]
      class(df) <- "data.frame"
      df$source <- nm
      df
    })
  )

  # Reshape for plotting
  curves_long <- tidyr::pivot_longer(
    curves_combined,
    cols = c("lower.limit", "upper.limit"),
    names_to = "bound",
    values_to = "value"
  )

  # Determine y variable based on type
  if (type == "c") {
    yvar <- "pvalue"
    ylab <- yaxis1
  } else if (type == "s") {
    yvar <- "svalue"
    ylab <- "S-value (bits)"
  } else {
    stop("Error: 'type' must be 'c' (consonance) or 's' (surprisal)")
  }

  # Build plot
  p <- ggplot2::ggplot(curves_long, ggplot2::aes(
    x = value, y = .data[[yvar]],
    color = source, fill = source
  )) +
    ggplot2::geom_line(linewidth = 0.8, alpha = 0.9) +
    ggplot2::scale_color_manual(values = colors, name = NULL) +
    ggplot2::scale_fill_manual(values = colors, name = NULL) +
    ggplot2::labs(title = title, subtitle = subtitle, x = xaxis, y = ylab) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 12, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 10),
      axis.title = ggplot2::element_text(size = 10),
      legend.position = legend.position
    )

  # Add ribbons for each source
  for (i in seq_along(curves_list)) {
    nm <- names(curves_list)[i]
    df <- curves_list[[nm]]
    class(df) <- "data.frame"

    if (type == "c") {
      p <- p +
        ggplot2::geom_ribbon(
          data = df,
          ggplot2::aes(x = lower.limit, ymin = min(pvalue), ymax = pvalue),
          fill = colors[i], color = NA, alpha = alpha, inherit.aes = FALSE
        ) +
        ggplot2::geom_ribbon(
          data = df,
          ggplot2::aes(x = upper.limit, ymin = min(pvalue), ymax = pvalue),
          fill = colors[i], color = NA, alpha = alpha, inherit.aes = FALSE
        )
    } else {
      p <- p +
        ggplot2::geom_ribbon(
          data = df,
          ggplot2::aes(x = lower.limit, ymin = max(svalue), ymax = svalue),
          fill = colors[i], color = NA, alpha = alpha, inherit.aes = FALSE
        ) +
        ggplot2::geom_ribbon(
          data = df,
          ggplot2::aes(x = upper.limit, ymin = max(svalue), ymax = svalue),
          fill = colors[i], color = NA, alpha = alpha, inherit.aes = FALSE
        )
    }
  }

  # X-axis scale
  if (measure == "ratio") {
    p <- p + ggplot2::scale_x_log10(breaks = scales::pretty_breaks(n = 6))
  } else {
    p <- p + ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 6))
  }

  # Y-axis scale
  if (type == "c") {
    if (position == "inverted") {
      p <- p + ggplot2::scale_y_reverse(
        expand = ggplot2::expansion(mult = c(0.01, 0.025)),
        breaks = seq(0, 1, 0.1),
        sec.axis = ggplot2::sec_axis(~ (1 - .) * 100,
          name = yaxis2,
          breaks = seq(0, 100, 10)
        )
      )
    } else {
      p <- p + ggplot2::scale_y_continuous(
        expand = ggplot2::expansion(mult = c(0.01, 0.025)),
        breaks = seq(0, 1, 0.1),
        sec.axis = ggplot2::sec_axis(~ (1 - .) * 100,
          name = yaxis2,
          breaks = seq(0, 100, 10)
        )
      )
    }
  } else {
    p <- p + ggplot2::scale_y_continuous(
      breaks = seq(0, 14, 2),
      expand = ggplot2::expansion(mult = c(0.01, 0.05))
    )
  }

  # Add null reference
  if (!is.null(nullvalue) && is.numeric(nullvalue)) {
    if (length(nullvalue) == 1) {
      p <- p + ggplot2::geom_vline(
        xintercept = nullvalue,
        linetype = "dashed",
        color = "#990000",
        alpha = 0.5
      )
    } else {
      p <- p + ggplot2::annotate(
        "rect",
        xmin = min(nullvalue), xmax = max(nullvalue),
        ymin = -Inf, ymax = Inf,
        fill = "#990000", alpha = 0.05
      )
    }
  }

  return(p)
}

# RMD Check
utils::globalVariables(c(
  "source", "value", "bound", "lower.limit", "upper.limit",
  "pvalue", "svalue"
))
