#' Plots Consonance, Surprisal, and Likelihood Functions
#'
#' Takes the dataframe produced by the interval functions and
#' plots the p-values/s-values, consonance (confidence) levels, and
#' the interval estimates to produce a p-value/s-value function
#' using ggplot2 graphics.
#'
#' @param data The dataframe produced by one of the interval functions
#' in which the intervals are stored.
#' @param type Choose whether to plot a "consonance" function, a
#' "surprisal" function or "likelihood". The default option is set to "c".
#' The type must be set in quotes, for example ggcurve (type = "s") or
#' ggcurve(type = "c"). Other options include "pd" for the consonance
#' distribution function, and "cd" for the consonance density function,
#' "l1" for relative likelihood, "l2" for log-likelihood, "l3" for likelihood
#' and "d" for deviance function.
#' @param measure Indicates whether the object has a log transformation
#' or is normal/default. The default setting is "default". If the measure
#' is set to "ratio", it will take logarithmically transformed values and
#' convert them back to normal values in the dataframe. This is typically a
#' setting used for binary outcomes and their measures such as risk ratios,
#' hazard ratios, and odds ratios.
#' @param levels Indicates which interval levels should be plotted on the function.
#' By default it is set to 0.95 to plot the 95% interval on the consonance function,
#' but more levels can be plotted by using the c() function for example,
#' levels = c(0.50, 0.75, 0.95).
#' @param nullvalue Indicates whether the null value for the measure
#' should be plotted. By default, it is set to NULL, meaning it will not be
#' plotted as a vertical line. Changing this to a numerical vector will specify the
#' region where a line should be plotted or an area that should be shaded. The input
#' must be a numerical vector, for example c(-0.5, 0.5) or a single numerical vector such as 0 or 1.
#' @param position Determines the orientation of the P-value (consonance) function.
#' By default, it is set to "pyramid", meaning the p-value function will
#' stand right side up, like a pyramid. However, it can also be inverted
#' via the option "inverted". This will also change the sequence of the
#' y-axes to match the orientation.This can be set as such,
#' ggcurve(type = "c", data = df, position = "inverted").
#' @param title A custom title for the graph. By default, it is
#' set to "Consonance Function". In order to set a title, it must
#' be in quotes. For example, ggcurve(type = "c",
#' data = x, title = "Custom Title").
#' @param subtitle A custom subtitle for the graph. By default, it is set
#' to "The function contains consonance/confidence intervals at every level
#' and the P-values." In order to set a subtitle, it must be in quotes.
#' For example, ggcurve(type = "c", data = x, subtitle = "Custom Subtitle").
#' @param xaxis A custom x-axis title for the graph. By default,
#' it is set to "Range of Values.
#' In order to set a x-axis title, it must be in quotes. For example,
#' ggcurve(type = "c", data = x, xaxis = "Hazard Ratio").
#' @param yaxis1 A custom y-axis title for the graph. By default,
#' it is set to "Consonance Level".
#' In order to set a y-axis title, it must be in quotes. For example,
#' ggcurve(type = "c", data = x, yxis1= "Confidence Level").
#' @param yaxis2 A custom y-axis title for the graph. By default,
#' it is set to "Levels for CI".
#' In order to set a y-axis title, it must be in quotes. For example,
#' ggcurve(type = "c", data = x, yxis2= "Confidence Level").
#' @param color Item that allows the user to choose the color of the points
#' and the ribbons in the graph. By default, it is set to color = "#555555".
#' The inputs must be in quotes.
#' For example, ggcurve(type = "c", data = x, color = "#333333").
#' @param fill Item that allows the user to choose the color of the ribbons in the graph.
#' By default, it is set to fill = "#239a98". The inputs must be in quotes. For example,
#' ggcurve(type = "c", data = x, fill = "#333333").
#'
#' @return A plot with intervals at every consonance level graphed with their corresponding
#' p-values and compatibility levels.
#'
#' @examples
#' \dontrun{
#' # Simulate random data
#'
#' library(concurve)
#'
#' GroupA <- rnorm(500)
#' GroupB <- rnorm(500)
#'
#' RandomData <- data.frame(GroupA, GroupB)
#'
#' intervalsdf <- suppressMessages(curve_mean(GroupA, GroupB, data = RandomData, method = "default"))
#' ggcurve(type = "c", intervalsdf[[1]], nullvalue =c(0))
#' }
#' @seealso [plot_compare()]
#'

ggcurve <- function(data, type = "c", measure = "default", levels = 0.95, nullvalue = NULL,
                    position = "pyramid",
                    title = "Consonance Function",
                    subtitle = "The function displays intervals at every level.",
                    xaxis = expression(theta == ~"Range of Values"),
                    yaxis1 = expression(paste(italic(p), "-value")),
                    yaxis2 = "Levels for CI (%)",
                    color = darken("#009E73", 0.5),
                    fill = "#239a98") {


  # Consonance Curve -----------------------------------------------------

  if (type == "c") {
    if (ncol(data) != 7) {
      stop("Error: 'data' or 'list' must be from 'concurve'.")
    }
    if (is.character(measure) != TRUE) {
      stop("Error: 'measure' must be a string such as 'default' or 'ratio'.")
    }
    #  if (is.logical(nullvalue) != TRUE | is.numeric(nullvalue) != TRUE) {
    #   stop("Error: 'nullvalue' must be a logical statement such as 'TRUE' or 'FALSE' or a numeric vector.")
    #  }
    if (is.character(position) != TRUE) {
      stop("Error: 'position' must be a string such as 'pyramid' or 'inverted'.")
    }
    if (is.character(title) != TRUE) {
      stop("Error: 'title' must be a string.")
    }
    if (is.character(subtitle) != TRUE) {
      stop("Error: 'subtitle' must be a string.")
    }

    if (is.character(fill) != TRUE) {
      stop("Error: 'fill' must be a string for the color.")
    }


    # Plotting Intervals ------------------------------------------------------

    interval <- pbmclapply(levels, FUN = function(i) (c(i, subset(data, intrvl.level == i)[, 1], subset(data, intrvl.level == i)[, 2])), mc.cores = getOption("mc.cores", 1L))
    interval <- data.frame(do.call(rbind, interval))
    interval <- pivot_longer(interval, X2:X3, names_to = "levels", values_to = "limits")
    interval <- interval[, -2]
    colum_names <- c("levels", "limits")
    colnames(interval) <- colum_names


    ggplot(data = data) +
      geom_line(aes(x = lower.limit, y = pvalue),
        color = color
      ) +
      geom_line(aes(x = upper.limit, y = pvalue),
        color = color
      ) +
      geom_point(data = interval, mapping = aes(x = limits, y = 1 - levels), size = 1.75, shape = 18) +
      geom_line(data = interval, mapping = aes(x = limits, y = 1 - levels, group = levels), size = .30) +
      geom_ribbon(aes(x = lower.limit, ymin = min(pvalue), ymax = pvalue),
        fill = fill, alpha = 0.10
      ) +
      geom_ribbon(aes(x = upper.limit, ymin = min(pvalue), ymax = pvalue),
        fill = fill, alpha = 0.10
      ) +
      labs(
        title = title,
        subtitle = subtitle,
        x = xaxis,
        y = yaxis1
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 11),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        text = element_text(size = 11)
      ) +
      {
        if (measure == "default") scale_x_continuous(breaks = scales::pretty_breaks(n = 5))
      } +
      {
        if (measure == "ratio") scale_x_log10(breaks = scales::pretty_breaks(n = 5))
      } +
      {
        if (position == "inverted") {
          scale_y_reverse(
            expand = expansion(mult = c(0.01, 0.025)),
            breaks = seq(0, 1, 0.10),
            sec.axis = sec_axis(~ (1 - .) * 100, name = yaxis2, breaks = seq(0, 100, 10))
          )
        }
      } +
      {
        if (position == "pyramid") {
          scale_y_continuous(
            expand = expansion(mult = c(0.01, 0.025)),
            breaks = seq(0, 1, 0.10),
            sec.axis = sec_axis(~ (1 - .) * 100, name = yaxis2, breaks = seq(0, 100, 10))
          )
        }
      } +
      {
        if (is.numeric(nullvalue) == TRUE) {
          annotate("rect",
            xmin = min(nullvalue), xmax = max(nullvalue), ymin = 0, ymax = 1,
            fill = "#d46c5b", color = "#d46c5b", alpha = 0.05, linetype = 3, size = 0.2
          )
        }
      }

    # Surprisal Curve ------------------------------------------------------
  } else if (type == "s") {
    if (is(data, "concurve") != TRUE) {
      stop("Error: 'data' must be a data frame from 'concurve'.")
    }
    if (ncol(data) != 7) {
      stop("Error: 'data' must be a data frame from 'concurve'.")
    }
    if (is.character(measure) != TRUE) {
      stop("Error: 'measure' must be a string such as 'default' or 'ratio'.")
    }
    if (is.character(title) != TRUE) {
      stop("Error: 'title' must be a string.")
    }
    if (is.character(subtitle) != TRUE) {
      stop("Error: 'subtitle' must be a string.")
    }
    if (is.character(fill) != TRUE) {
      stop("Error: 'fill' must be a string for the color.")
    }


    # Plotting Intervals ------------------------------------------------------

    interval <- pbmclapply(levels, FUN = function(i) (c(i, subset(data, intrvl.level == i)[, 1], subset(data, intrvl.level == i)[, 2])), mc.cores = getOption("mc.cores", 1L))
    interval <- data.frame(do.call(rbind, interval))
    interval <- gather(interval, key = "levels", value = "limits", X2:X3)
    interval <- interval[, -2]
    colum_names <- c("levels", "limits")
    colnames(interval) <- colum_names

    ggplot(data = data) +
      geom_line(aes(x = lower.limit, y = svalue),
        color = color
      ) +
      geom_line(aes(x = upper.limit, y = svalue),
        color = color
      ) +
      geom_point(data = interval, mapping = aes(x = limits, y = (-log2(1 - levels))), size = 1.75, shape = 18) +
      geom_line(data = interval, mapping = aes(x = limits, y = (-log2(1 - levels)), group = levels), size = .30) +
      geom_ribbon(aes(x = lower.limit, ymin = max(svalue), ymax = svalue),
        fill = fill, alpha = 0.10
      ) +
      geom_ribbon(aes(x = upper.limit, ymin = max(svalue), ymax = svalue),
        fill = fill, alpha = 0.10
      ) +
      labs(
        title = title,
        subtitle = subtitle,
        x = xaxis,
        y = "S-value \n(Bits of Information)"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 11),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        text = element_text(size = 11)
      ) +
      {
        if (measure == "default") scale_x_continuous(breaks = scales::pretty_breaks(n = 5))
      } +
      {
        if (measure == "ratio") scale_x_log10(breaks = scales::pretty_breaks(n = 5))
      } +
      scale_y_continuous(breaks = seq(0, 14, 1), expand = c(0.0075, 0.0075)) +
      if (is.numeric(nullvalue) == TRUE) {
        annotate("rect",
          xmin = min(nullvalue), xmax = max(nullvalue), ymin = RobustMin((interval$svalue)), ymax = RobustMax((interval$svalue)), fill = "#d46c5b", color = "#d46c5b", alpha = 0.05, linetype = 3, size = 0.2
        )
      }




    # Consonance Distribution -----------------------------------------------------
  } else if (type == "cdf") {
    if (is(data, "concurve") != TRUE) {
      stop("Error: 'data' must be a data frame from 'concurve'.")
    }
    if (ncol(data) != 1) {
      stop("Error: 'data' must be a data frame from 'concurve'.")
    }
    if (is.character(measure) != TRUE) {
      stop("Error: 'measure' must be a string such as 'default' or 'ratio'.")
    }
    if (is.character(position) != TRUE) {
      stop("Error: 'position' must be a string such as 'pyramid' or 'inverted'.")
    }
    if (is.character(title) != TRUE) {
      stop("Error: 'title' must be a string.")
    }
    if (is.character(subtitle) != TRUE) {
      stop("Error: 'subtitle' must be a string.")
    }
    if (is.character(fill) != TRUE) {
      stop("Error: 'fill' must be a string for the color.")
    }

    ggplot(data = data, mapping = aes(x = x)) +
      stat_ecdf(geom = "point", color = darken("#e7998c", 0.2), size = 0.75, shape = 5, alpha = 0.75) +
      geom_hline(yintercept = 0.50, linetype = "dotted", alpha = 0.5) +
      labs(
        title = title,
        subtitle = subtitle,
        x = xaxis,
        y = "Cumulative Confidence"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 12),
        plot.caption = element_text(size = 8),
        axis.title.x = element_text(size = 13),
        axis.title.y = element_text(size = 13),
        text = element_text(size = 15)
      ) +
      {
        if (measure == "ratio") scale_x_log10(breaks = scales::pretty_breaks(n = 10))
      } +
      scale_y_continuous(expand = expansion(mult = c(0.01, 0.05)), breaks = scales::pretty_breaks(n = 10)) +
      if (is.numeric(nullvalue) == TRUE) {
        annotate("rect",
          xmin = min(nullvalue), xmax = max(nullvalue), ymin = 0, ymax = 1,
          fill = "#d46c5b", color = "#d46c5b", alpha = 0.05, linetype = 3, size = 0.2
        )
      }

    # Consonance Density ---------------------------------------------
  } else if (type == "cd") {
    if (ncol(data) != 1) {
      stop("Error: 'data' must be a data frame from the curve_boot function in 'concurve'.")
    }
    if (is.character(title) != TRUE) {
      stop("Error: 'title' must be a string.")
    }
    if (is.character(subtitle) != TRUE) {
      stop("Error: 'subtitle' must be a string.")
    }
    if (is.character(fill) != TRUE) {
      stop("Error: 'fill' must be a string for the color.")
    }

    ggplot(data = data, mapping = aes(x = x)) +
      geom_density(fill = fill, color = color, alpha = 0.20) +
      labs(
        title = title,
        subtitle = subtitle,
        x = xaxis,
        y = "Density"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 12),
        plot.caption = element_text(size = 8),
        axis.title.x = element_text(size = 13),
        axis.title.y = element_text(size = 13),
        text = element_text(size = 15)
      ) +
      {
        if (measure == "ratio") scale_x_log10(breaks = scales::pretty_breaks(n = 10))
      } +
      scale_y_continuous(expand = expansion(mult = c(0.01, 0.05)), breaks = scales::pretty_breaks(n = 10)) +
      if (is.numeric(nullvalue) == TRUE) {
        annotate("rect",
          xmin = min(nullvalue), xmax = max(nullvalue), ymin = min(density(data$x)[["y"]]), ymax = max(density(data$x)[["y"]]),
          fill = "#d46c5b", color = "#d46c5b", alpha = 0.05, linetype = 3, size = 0.2
        )
      }

    # Relative Likelihood Function -----------------------------------------------------
  } else if (type == "l1") {
    if (ncol(data) != 5) {
      stop("Error: 'data' must be a data frame from 'concurve'.")
    }
    if (is.character(measure) != TRUE) {
      stop("Error: 'measure' must be a string such as 'default' or 'ratio'.")
    }

    if (is.character(title) != TRUE) {
      stop("Error: 'title' must be a string.")
    }
    if (is.character(subtitle) != TRUE) {
      stop("Error: 'subtitle' must be a string.")
    }
    if (is.character(fill) != TRUE) {
      stop("Error: 'fill' must be a string for the color.")
    }

    ggplot(data = data, mapping = aes(x = values, y = support)) +
      geom_line() +
      geom_ribbon(aes(x = values, ymin = min(support), ymax = support), fill = fill, color = color, alpha = 0.10) +
      labs(
        title = title,
        subtitle = subtitle,
        x = xaxis,
        y = "Relative Likelihood"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 12),
        plot.caption = element_text(size = 8),
        axis.title.x = element_text(size = 13),
        axis.title.y = element_text(size = 13),
        text = element_text(size = 15)
      ) +
      {
        if (measure == "ratio") scale_x_log10(breaks = scales::pretty_breaks(n = 10))
      } +
      scale_y_continuous(expand = expansion(mult = c(0.01, 0.05)), breaks = scales::pretty_breaks(n = 10)) +
      if (is.numeric(nullvalue) == TRUE) {
        annotate("rect",
          xmin = min(nullvalue), xmax = max(nullvalue), ymin = 0, ymax = 1,
          fill = "#d46c5b", color = "#d46c5b", alpha = 0.05, linetype = 3, size = 0.2
        )
      }
    # Log-Likelihood Function -----------------------------------------------------
  } else if (type == "l2") {
    if (ncol(data) != 5) {
      stop("Error: 'data' must be a data frame from 'concurve'.")
    }
    if (is.character(measure) != TRUE) {
      stop("Error: 'measure' must be a string such as 'default' or 'ratio'.")
    }

    if (is.character(title) != TRUE) {
      stop("Error: 'title' must be a string.")
    }
    if (is.character(subtitle) != TRUE) {
      stop("Error: 'subtitle' must be a string.")
    }
    if (is.character(fill) != TRUE) {
      stop("Error: 'fill' must be a string for the color.")
    }

    ggplot(data = data, mapping = aes(x = values, y = loglikelihood)) +
      geom_line() +
      geom_ribbon(aes(x = values, ymin = min(loglikelihood), ymax = loglikelihood), fill = fill, color = color, alpha = 0.10) +
      labs(
        title = title,
        subtitle = subtitle,
        x = xaxis,
        y = "Log-Likelihood"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 12),
        plot.caption = element_text(size = 8),
        axis.title.x = element_text(size = 13),
        axis.title.y = element_text(size = 13),
        text = element_text(size = 15)
      ) +
      {
        if (measure == "ratio") scale_x_log10(breaks = scales::pretty_breaks(n = 10))
      } +
      scale_y_continuous(expand = expansion(mult = c(0.01, 0.05)), breaks = scales::pretty_breaks(n = 10)) +
      if (is.numeric(nullvalue) == TRUE) {
        annotate("rect",
          xmin = min(nullvalue), xmax = max(nullvalue), ymin = RobustMin(data$loglikelihood), ymax = RobustMax(data$loglikelihood),
          fill = "#d46c5b", color = "#d46c5b", alpha = 0.05, linetype = 3, size = 0.2
        )
      }

    # Likelihood Function -----------------------------------------------------
  } else if (type == "l3") {
    if (ncol(data) != 5) {
      stop("Error: 'data' must be a data frame from 'concurve'.")
    }
    if (is.character(measure) != TRUE) {
      stop("Error: 'measure' must be a string such as 'default' or 'ratio'.")
    }

    if (is.character(title) != TRUE) {
      stop("Error: 'title' must be a string.")
    }
    if (is.character(subtitle) != TRUE) {
      stop("Error: 'subtitle' must be a string.")
    }
    if (is.character(fill) != TRUE) {
      stop("Error: 'fill' must be a string for the color.")
    }

    ggplot(data = data, mapping = aes(x = values, y = likelihood)) +
      geom_line() +
      geom_ribbon(aes(x = values, ymin = min(likelihood), ymax = likelihood), fill = fill, color = color, alpha = 0.10) +
      labs(
        title = title,
        subtitle = subtitle,
        x = xaxis,
        y = "Likelihood"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 12),
        plot.caption = element_text(size = 8),
        axis.title.x = element_text(size = 13),
        axis.title.y = element_text(size = 13),
        text = element_text(size = 15)
      ) +
      {
        if (measure == "ratio") scale_x_log10(breaks = scales::pretty_breaks(n = 10))
      } +
      scale_y_continuous(expand = expansion(mult = c(0.01, 0.05)), breaks = scales::pretty_breaks(n = 10)) +
      if (is.numeric(nullvalue) == TRUE) {
        annotate("rect",
          xmin = min(nullvalue), xmax = max(nullvalue), ymin = 0, ymax = RobustMax(data$likelihood),
          fill = "#d46c5b", color = "#d46c5b", alpha = 0.05, linetype = 3, size = 0.2
        )
      }

    # Deviance Function -----------------------------------------------------
  } else if (type == "d") {
    if (ncol(data) != 5) {
      stop("Error: 'data' must be a data frame from 'concurve'.")
    }
    if (is.character(measure) != TRUE) {
      stop("Error: 'measure' must be a string such as 'default' or 'ratio'.")
    }

    if (is.character(title) != TRUE) {
      stop("Error: 'title' must be a string.")
    }
    if (is.character(subtitle) != TRUE) {
      stop("Error: 'subtitle' must be a string.")
    }
    if (is.character(fill) != TRUE) {
      stop("Error: 'fill' must be a string for the color.")
    }

    ggplot(data = data, mapping = aes(x = values, y = deviancestat)) +
      geom_line() +
      geom_ribbon(aes(x = values, ymin = deviancestat, ymax = max(deviancestat)), fill = fill, color = color, alpha = 0.10) +
      labs(
        title = title,
        subtitle = subtitle,
        x = xaxis,
        y = "Deviance Statistic"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 12),
        plot.caption = element_text(size = 8),
        axis.title.x = element_text(size = 13),
        axis.title.y = element_text(size = 13),
        text = element_text(size = 15)
      ) +
      {
        if (measure == "ratio") scale_x_log10(breaks = scales::pretty_breaks(n = 10))
      } +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10), expand = c(0.0075, 0.0075)) +
      {
        if (is.numeric(nullvalue) == TRUE) {
          annotate("rect",
            xmin = min(nullvalue), xmax = max(nullvalue), ymin = 0, ymax = RobustMax(data$deviancestat),
            fill = "#d46c5b", color = "#d46c5b", alpha = 0.05, linetype = 3, size = 0.2
          )
        }
      }
  }
}

# RMD Check
utils::globalVariables(c("df", "lower.limit", "upper.limit", "intrvl.width", "intrvl.level", "cdf", "pvalue", "svalue"))
utils::globalVariables(c("X2", "X3", "limits", "x"))
