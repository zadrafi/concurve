#' Construct Consonance Function from Ratio Estimate
#'
#' Convenience function to construct consonance functions from ratio measures
#' (odds ratios, hazard ratios, risk ratios) given a point estimate and
#' confidence interval bounds.
#'
#' @param ratio Point estimate of the ratio.
#' @param lower Lower bound of the confidence interval.
#' @param upper Upper bound of the confidence interval.
#' @param conf.level Confidence level of the provided interval. Default is 0.95.
#' @param steps Number of consonance levels to compute. Default is 1000.
#' @param cores Number of cores for parallel computation.
#' @param table Logical. If TRUE (default), includes a summary table.
#'
#' @return A list with class "concurve" containing the intervals dataframe,
#'   density dataframe, and optionally a summary table.
#'
#' @details
#' This is a convenience wrapper around \code{\link{curve_rev}} specifically
#' for ratio measures. It automatically handles the log transformation
#' and sets appropriate measure type.
#'
#' @examples
#' \dontrun{
#' # From a published odds ratio: OR = 1.5, 95% CI [1.1, 2.0]
#' result <- curve_from_ratio(ratio = 1.5, lower = 1.1, upper = 2.0)
#' ggcurve(result[[1]], type = "c", nullvalue = TRUE)
#'
#' # Hazard ratio from survival analysis: HR = 0.75, 95% CI [0.60, 0.95]
#' result <- curve_from_ratio(ratio = 0.75, lower = 0.60, upper = 0.95)
#' ggcurve(result[[1]], type = "c", nullvalue = TRUE)
#' }
#'
#' @seealso [curve_from_se()] for constructing from standard error
#' @seealso [curve_rev()] for the underlying function
#'
#' @export
curve_from_ratio <- function(ratio, lower, upper,
                              conf.level = 0.95,
                              steps = 1000,
                              cores = getOption("mc.cores", 1L),
                              table = TRUE) {

  # Validate inputs
  if (ratio <= 0 || lower <= 0 || upper <= 0) {
    stop("Ratio and confidence bounds must be positive")
  }

  if (lower >= upper) {
    stop("Lower bound must be less than upper bound")
  }

  if (ratio < lower || ratio > upper) {
    warning("Point estimate is outside the confidence interval bounds")
  }

  # Delegate to curve_rev with ratio measure
  curve_rev(
    point = ratio,
    LL = lower,
    UL = upper,
    conf.level = conf.level,
    type = "c",
    measure = "ratio",
    steps = steps,
    cores = cores,
    table = table
  )
}


#' Construct Consonance Function from Standard Error
#'
#' Convenience function to construct consonance functions given a point
#' estimate and its standard error.
#'
#' @param estimate Point estimate.
#' @param se Standard error of the estimate.
#' @param measure Type of measure: "mean" for differences (default),
#'   "ratio" for ratio measures (estimate should be on original scale).
#' @param steps Number of consonance levels to compute. Default is 1000.
#' @param cores Number of cores for parallel computation.
#' @param table Logical. If TRUE (default), includes a summary table.
#'
#' @return A list with class "concurve" containing the intervals dataframe,
#'   density dataframe, and optionally a summary table.
#'
#' @details
#' This function constructs consonance intervals using the normal approximation:
#' \code{estimate +/- z * se} where z varies across confidence levels.
#'
#' For ratio measures, the function handles the log transformation internally.
#'
#' @examples
#' \dontrun{
#' # Mean difference with SE
#' result <- curve_from_se(estimate = 2.5, se = 0.8, measure = "mean")
#' ggcurve(result[[1]], type = "c", nullvalue = TRUE)
#'
#' # Odds ratio with SE (on log scale internally)
#' result <- curve_from_se(estimate = 1.5, se = 0.2, measure = "ratio")
#' ggcurve(result[[1]], type = "c", nullvalue = TRUE)
#' }
#'
#' @seealso [curve_from_ratio()] for constructing from CI bounds
#' @seealso [curve_rev()] for the underlying function
#'
#' @export
curve_from_se <- function(estimate, se,
                           measure = "mean",
                           steps = 1000,
                           cores = getOption("mc.cores", 1L),
                           table = TRUE) {

  # Validate inputs
  if (!is.numeric(estimate) || !is.numeric(se)) {
    stop("'estimate' and 'se' must be numeric")
  }

  if (se <= 0) {
    stop("Standard error must be positive")
  }

  if (measure == "ratio" && estimate <= 0) {
    stop("For ratio measures, estimate must be positive")
  }

  # Calculate 95% CI bounds for curve_rev
  z <- qnorm(0.975)

  if (measure == "ratio") {
    # Work on log scale
    log_est <- log(estimate)
    log_se <- se / estimate  # Delta method approximation
    lower <- exp(log_est - z * log_se)
    upper <- exp(log_est + z * log_se)
  } else {
    lower <- estimate - z * se
    upper <- estimate + z * se
  }

  # Delegate to curve_rev
  curve_rev(
    point = estimate,
    LL = lower,
    UL = upper,
    conf.level = 0.95,
    type = "c",
    measure = measure,
    steps = steps,
    cores = cores,
    table = table
  )
}


#' Calculate Overlap Between Consonance Functions
#'
#' Quantifies the area of overlap between two consonance functions,
#' providing a measure of compatibility between two estimates.
#'
#' @param data1 First concurve object or intervals data frame.
#' @param data2 Second concurve object or intervals data frame.
#' @param type Function type for comparison: "c" for consonance (default),
#'   "s" for surprisal.
#' @param plot Logical. If TRUE (default), displays overlap visualization.
#' @param title Plot title.
#'
#' @return A list containing:
#' \describe{
#'   \item{overlap_area}{Estimated area of overlap}
#'   \item{total_area}{Total combined area}
#'   \item{overlap_ratio}{Ratio of overlap to total area}
#'   \item{max_shared_level}{Maximum confidence level where intervals overlap}
#' }
#'
#' @details
#' This function computes the overlap between two consonance functions,
#' which provides a measure of how compatible two estimates are with each other.
#' Higher overlap indicates greater compatibility.
#'
#' The overlap is calculated by finding the intersection of intervals at each
#' confidence level and integrating over the shared region.
#'
#' @examples
#' \dontrun{
#' # Compare two study results
#' study1 <- curve_from_ratio(1.5, 1.1, 2.0)
#' study2 <- curve_from_ratio(1.3, 0.9, 1.8)
#'
#' overlap <- curve_overlap(study1[[1]], study2[[1]])
#' print(overlap)
#' }
#'
#' @seealso [curve_compare()] for graphical comparison
#' @seealso [plot_compare()] for visualization
#'
#' @export
curve_overlap <- function(data1, data2,
                           type = "c",
                           plot = TRUE,
                           title = "Consonance Function Overlap") {

  # Extract data frames if concurve objects
  if (is.list(data1) && !is.data.frame(data1)) {
    df1 <- data1[[1]]
  } else {
    df1 <- data1
  }

  if (is.list(data2) && !is.data.frame(data2)) {
    df2 <- data2[[1]]
  } else {
    df2 <- data2
  }

  # Find common confidence levels
  common_levels <- intersect(round(df1$intrvl.level, 4), round(df2$intrvl.level, 4))

  if (length(common_levels) == 0) {
    stop("No common confidence levels found between the two functions")
  }

  # Calculate overlap at each level
  overlap_data <- lapply(common_levels, function(lvl) {
    row1 <- df1[which.min(abs(df1$intrvl.level - lvl)), ]
    row2 <- df2[which.min(abs(df2$intrvl.level - lvl)), ]

    # Check if intervals overlap
    lower_max <- max(row1$lower.limit, row2$lower.limit)
    upper_min <- min(row1$upper.limit, row2$upper.limit)

    if (lower_max < upper_min) {
      shared_width <- upper_min - lower_max
    } else {
      shared_width <- 0
    }

    data.frame(
      level = lvl,
      shared_width = shared_width,
      width1 = row1$intrvl.width,
      width2 = row2$intrvl.width
    )
  })

  overlap_df <- do.call(rbind, overlap_data)

  # Calculate summary statistics
  max_shared_idx <- which.max(overlap_df$shared_width > 0)
  max_shared_level <- if (length(max_shared_idx) > 0) {
    max(overlap_df$level[overlap_df$shared_width > 0])
  } else {
    0
  }

  # Estimate areas using trapezoidal integration
  overlap_area <- sum(diff(overlap_df$level) *
    (head(overlap_df$shared_width, -1) + tail(overlap_df$shared_width, -1)) / 2)
  area1 <- sum(diff(overlap_df$level) *
    (head(overlap_df$width1, -1) + tail(overlap_df$width1, -1)) / 2)
  area2 <- sum(diff(overlap_df$level) *
    (head(overlap_df$width2, -1) + tail(overlap_df$width2, -1)) / 2)

  total_area <- area1 + area2 - overlap_area
  overlap_ratio <- if (total_area > 0) overlap_area / total_area else 0

  result <- list(
    overlap_area = overlap_area,
    total_area = total_area,
    overlap_ratio = overlap_ratio,
    max_shared_level = max_shared_level,
    overlap_data = overlap_df
  )

  # Plot if requested
  if (plot) {
    p <- plot_compare(df1, df2, type = type)
    print(p + ggplot2::ggtitle(title))
  }

  return(result)
}


#' Plot Multiple Consonance Functions
#'
#' Creates a combined plot showing multiple consonance functions for comparison.
#'
#' @param ... Concurve objects or intervals data frames to plot.
#' @param type Plot type: "c" for consonance (default), "s" for surprisal.
#' @param labels Character vector of labels for each curve. If NULL, uses
#'   names from input or generates sequential labels.
#' @param colors Character vector of colors for each curve.
#' @param nullvalue Null value to display as vertical line. Default is NULL (no line).
#' @param title Plot title.
#' @param alpha Transparency for curves. Default is 0.7.
#'
#' @return A ggplot object.
#'
#' @details
#' This function overlays multiple consonance or surprisal functions on a
#' single plot for easy comparison. Each curve is distinguished by color.
#'
#' @examples
#' \dontrun{
#' # Compare three analyses
#' result1 <- curve_from_ratio(1.5, 1.1, 2.0)
#' result2 <- curve_from_ratio(1.3, 0.9, 1.8)
#' result3 <- curve_from_ratio(1.8, 1.2, 2.5)
#'
#' plot_multi(result1[[1]], result2[[1]], result3[[1]],
#'   labels = c("Study A", "Study B", "Study C"),
#'   nullvalue = 1,
#'   title = "Comparison of Odds Ratios"
#' )
#' }
#'
#' @seealso [ggcurve()] for single curve plotting
#' @seealso [curve_overlap()] for quantifying overlap
#'
#' @export
plot_multi <- function(...,
                        type = "c",
                        labels = NULL,
                        colors = NULL,
                        nullvalue = NULL,
                        title = "Consonance Functions Comparison",
                        alpha = 0.7) {

  # Capture all curve arguments
  curves <- list(...)

  if (length(curves) == 0) {
    stop("At least one curve must be provided")
  }

  # Extract data frames
  dfs <- lapply(seq_along(curves), function(i) {
    x <- curves[[i]]
    if (is.list(x) && !is.data.frame(x)) {
      df <- x[[1]]
    } else if (is.data.frame(x)) {
      df <- x
    } else {
      stop("Each argument must be a concurve object or data frame")
    }
    df$curve_id <- i
    df
  })

  # Generate labels
  if (is.null(labels)) {
    labels <- paste("Curve", seq_along(curves))
  }

  # Combine all data
  combined <- do.call(rbind, dfs)
  combined$curve_label <- factor(combined$curve_id, labels = labels)

  # Set up colors
  if (is.null(colors)) {
    colors <- scales::hue_pal()(length(curves))
  }

  # Determine y variable based on type
  if (type == "c") {
    y_var <- "intrvl.level"
    y_lab <- "Confidence Level"
  } else if (type == "s") {
    y_var <- "svalue"
    y_lab <- "S-value (Surprisal)"
  } else {
    stop("'type' must be 'c' (consonance) or 's' (surprisal)")
  }

  # Create long format for plotting both bounds
  df_lower <- combined[, c("lower.limit", y_var, "curve_label")]
  df_lower$bound <- "lower"
  names(df_lower)[1] <- "x"

  df_upper <- combined[, c("upper.limit", y_var, "curve_label")]
  df_upper$bound <- "upper"
  names(df_upper)[1] <- "x"

  df_long <- rbind(df_lower, df_upper)

  # Build plot
  p <- ggplot2::ggplot(df_long, ggplot2::aes(
    x = .data$x,
    y = .data[[y_var]],
    color = .data$curve_label,
    group = interaction(.data$curve_label, .data$bound)
  )) +
    ggplot2::geom_line(alpha = alpha, linewidth = 1) +
    ggplot2::scale_color_manual(values = colors, name = "Analysis") +
    ggplot2::labs(
      title = title,
      x = "Parameter Value",
      y = y_lab
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")

  # Add null value line
  if (!is.null(nullvalue)) {
    p <- p + ggplot2::geom_vline(
      xintercept = nullvalue,
      linetype = "dashed",
      color = "gray50"
    )
  }

  return(p)
}


#' Generate Summary Statistics for Consonance Objects
#'
#' Produces a summary of key statistics from a consonance function.
#'
#' @param data A concurve object or intervals data frame.
#' @param levels Confidence levels to include in summary. Default is c(0.50, 0.90, 0.95, 0.99).
#' @param null_value Reference value for compatibility assessment.
#'   Default is 0 for differences, 1 for ratios.
#' @param digits Number of decimal places in output. Default is 4.
#'
#' @return A data frame with summary statistics.
#'
#' @details
#' This function provides a summary of a consonance function including:
#' \itemize{
#'   \item Point estimate (value at maximum consonance)
#'   \item Confidence intervals at specified levels
#'   \item P-value and S-value at the null hypothesis
#'   \item Interval width as a measure of precision
#' }
#'
#' @examples
#' \dontrun{
#' model <- lm(mpg ~ wt, data = mtcars)
#' result <- curve_gen(model, "wt")
#'
#' # Get summary
#' curve_summary(result[[1]])
#'
#' # Custom levels
#' curve_summary(result[[1]], levels = c(0.80, 0.95))
#' }
#'
#' @seealso [curve_table()] for formatted interval tables
#'
#' @export
curve_summary <- function(data,
                          levels = c(0.50, 0.90, 0.95, 0.99),
                          null_value = NULL,
                          digits = 4) {

  # Extract data frame
  if (is.list(data) && !is.data.frame(data)) {
    df <- data[[1]]
  } else if (is.data.frame(data)) {
    df <- data
  } else {
    stop("'data' must be a concurve object or data frame")
  }

  # Find point estimate (at maximum consonance)
  max_idx <- which.max(df$intrvl.level)
  point_est <- mean(c(df$lower.limit[max_idx], df$upper.limit[max_idx]))

  # Detect if ratio measure
  is_ratio <- all(df$lower.limit > 0, na.rm = TRUE)
  if (is.null(null_value)) {
    null_value <- if (is_ratio) 1 else 0
  }

  # Get intervals at specified levels
  intervals <- lapply(levels, function(lvl) {
    idx <- which.min(abs(df$intrvl.level - lvl))
    data.frame(
      Level = paste0(round(lvl * 100, 1), "%"),
      Lower = round(df$lower.limit[idx], digits),
      Upper = round(df$upper.limit[idx], digits),
      Width = round(df$intrvl.width[idx], digits),
      P_value = round(df$pvalue[idx], digits),
      S_value = round(df$svalue[idx], 2)
    )
  })

  summary_df <- do.call(rbind, intervals)

  # Add header info
  attr(summary_df, "point_estimate") <- round(point_est, digits)
  attr(summary_df, "null_value") <- null_value
  attr(summary_df, "measure_type") <- if (is_ratio) "ratio" else "difference"

  # Print summary
  cat("Consonance Function Summary\n")
  cat("===========================\n\n")
  cat("Point Estimate:", round(point_est, digits), "\n")
  cat("Measure Type:", attr(summary_df, "measure_type"), "\n")
  cat("Null Value:", null_value, "\n\n")
  print(summary_df, row.names = FALSE)

  invisible(summary_df)
}


# R CMD check global variables
utils::globalVariables(c("curve_label", "bound", "x", ".data", "curve_id"))
