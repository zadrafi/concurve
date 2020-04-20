#' Graph and Compare Consonance, Surprisal, and Likelihood Functions
#'
#' Compares the p-value/s-value, and likelihood functions using ggplot2 graphics.
#'
#' @param data1 The first dataframe produced by one of the interval functions in which the
#' intervals are stored.
#' @param data2 The second dataframe produced by one of the interval functions in which the
#' intervals are stored.
#' @param type Choose whether to plot a "consonance" function, a
#' "surprisal" function or "likelihood". The default option is set to "c".
#' The type must be set in quotes, for example plot_compare(type = "s") or
#' plot_compare(type = "c"). Other options include "pd" for the consonance
#' distribution function, and "cd" for the consonance density function,
#' "l1" for relative likelihood, "l2" for log-likelihood, "l3" for likelihood
#' and "d" for deviance function.
#' @param measure Indicates whether the object has a log transformation
#' or is normal/default. The default setting is "default". If the measure
#' is set to "ratio", it will take logarithmically transformed values and
#' convert them back to normal values in the dataframe. This is typically a
#' setting used for binary outcomes and their measures such as risk ratios,
#' hazard ratios, and odds ratios.
#' @param nullvalue Indicates whether the null value for the measure
#' should be plotted. By default, it is set to FALSE, meaning it will not be
#' plotted as a vertical line. Changing this to TRUE, will plot a vertical
#' line at 0 when the measure is set to " default" and a vertical line at
#' 1 when the measure is set to "ratio". For example,
#' plot_compare(type = "c", data = df, measure = "ratio", nullvalue = "present").
#' This feature is not yet available for surprisal functions.
#' @param position Determines the orientation of the P-value (consonance) function.
#' By default, it is set to "pyramid", meaning the p-value function will
#' stand right side up, like a pyramid. However, it can also be inverted
#' via the option "inverted". This will also change the sequence of the
#' y-axes to match the orientation.This can be set as such,
#' plot_compare(type = "c", data = df, position = "inverted").
#' @param title A custom title for the graph. By default, it is
#' set to "Consonance Function". In order to set a title, it must
#' be in quotes. For example, plot_compare(type = "c",
#' data = x, title = "Custom Title").
#' @param subtitle A custom subtitle for the graph. By default, it is set
#' to "The function contains consonance/confidence intervals at every level
#' and the P-values." In order to set a subtitle, it must be in quotes.
#' For example, plot_compare(type = "c", data = x, subtitle = "Custom Subtitle").
#' @param xaxis A custom x-axis title for the graph. By default,
#' it is set to "Range of Values.
#' In order to set a x-axis title, it must be in quotes. For example,
#' plot_compare(type = "c", data = x, xaxis = "Hazard Ratio").
#' @param yaxis A custom y-axis title for the graph. By default,
#' it is set to "Consonance Level".
#' In order to set a y-axis title, it must be in quotes. For example,
#' plot_compare(type = "c", data = x, yxis = "Confidence Level").
#' @param color Item that allows the user to choose the color of the points
#' and the ribbons in the graph. By default, it is set to color = "#555555".
#' The inputs must be in quotes.
#' For example, plot_compare(type = "c", data = x, color = "#333333").
#' @param fill1 Item that allows the user to choose the color of the ribbons in the graph
#' for data1. By default, it is set to fill1 = "#239a98". The inputs must be in quotes.
#' For example, plot_compare(type = "c", data = x, fill1 = "#333333").
#' @param fill2 Item that allows the user to choose the color of the ribbons in the graph
#' for data1. By default, it is set to fill2 = "#d46c5b". The inputs must be in quotes.
#' For example, plot_compare(type = "c", data = x, fill2 = "#333333").
#' @return A plot that compares two functions.
#' @examples
#' \donttest{
#' library(concurve)
#'
#' GroupA <- rnorm(50)
#' GroupB <- rnorm(50)
#' RandomData <- data.frame(GroupA, GroupB)
#' intervalsdf <- curve_mean(GroupA, GroupB, data = RandomData)
#' GroupA2 <- rnorm(50)
#' GroupB2 <- rnorm(50)
#' RandomData2 <- data.frame(GroupA2, GroupB2)
#' model <- lm(GroupA2 ~ GroupB2, data = RandomData2)
#'
#' randomframe <- curve_gen(model, "GroupB2")
#'
#' plot_compare(intervalsdf[[1]], randomframe[[1]], type = "c")
#' }
#'
#' @seealso [ggcurve()]
#' @seealso [curve_compare()]
#'
plot_compare <- function(data1, data2, type = "c", measure = "default", nullvalue = FALSE, position = "pyramid",
                         title = "Interval Functions",
                         subtitle = "The function displays intervals at every level.",
                         xaxis = expression(Theta ~ "Range of Values"),
                         yaxis = expression(paste(italic(p), "-value")),
                         color = "#000000",
                         fill1 = "#239a98",
                         fill2 = "#EE6A50") {
  cols <- c(fill1, fill2)

  # Consonance Curve -----------------------------------------------------

  if (type == "c") {
    if (is(data1, "concurve") != TRUE) {
      stop("Error: 'data1' must be a data frame from 'concurve'.")
    }
    if (ncol(data1) != 7) {
      stop("Error: 'x' must be a data frame from 'concurve'.")
    }
    if (is(data2, "concurve") != TRUE) {
      stop("Error: 'data2' must be a data frame from 'concurve'.")
    }
    if (ncol(data2) != 7) {
      stop("Error: 'x' must be a data frame from 'concurve'.")
    }
    if (is.character(measure) != TRUE) {
      stop("Error: 'measure' must be a string such as 'default' or 'ratio'.")
    }
    if (is.logical(nullvalue) != TRUE) {
      stop("Error: 'nullvalue' must be a logical statement such as 'TRUE' or 'FALSE'.")
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
    if (is.character(fill1) != TRUE) {
      stop("Error: 'fill1' must be a string for the color.")
    }
    if (is.character(fill2) != TRUE) {
      stop("Error: 'fill2' must be a string for the color.")
    }
    ggplot(data = data1) +
      geom_line(aes(x = lower.limit, y = pvalue),
        color = color
      ) +
      geom_line(aes(x = upper.limit, y = pvalue),
        color = color
      ) +
      geom_ribbon(aes(x = lower.limit, ymin = min(pvalue), ymax = pvalue, fill = fill1),
        alpha = 0.20
      ) +
      geom_ribbon(aes(x = upper.limit, ymin = min(pvalue), ymax = pvalue, fill = fill1),
        alpha = 0.20
      ) +
      geom_line(
        data = data2, aes(x = lower.limit, y = pvalue),
        color = color
      ) +
      geom_line(
        data = data2, aes(x = upper.limit, y = pvalue),
        color = color
      ) +
      geom_ribbon(
        data = data2, aes(x = lower.limit, ymin = min(pvalue), ymax = pvalue, fill = fill2),
        alpha = 0.20
      ) +
      geom_ribbon(
        data = data2, aes(x = upper.limit, ymin = min(pvalue), ymax = pvalue, fill = fill2),
        alpha = 0.20
      ) +
      theme_minimal() +
      labs(
        title = "Consonance Curves",
        subtitle = subtitle,
        x = xaxis,
        y = yaxis
      ) +
      theme(
        plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 11),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        text = element_text(size = 11),
        legend.background = element_blank(),
        legend.position = c(.998, .95),
        legend.justification = c("right", "top"),
        legend.key = element_rect(linetype = 1, color = alpha(cols, 0.20)),
        legend.key.size = unit(0.495, "cm")
      ) +
      scale_fill_manual(
        aesthetics = "fill",
        values = cols,
        labels = c("Study 1", "Study 2")
      ) +
      guides(fill = guide_legend(
        title = "Identity",
        title.theme = element_text(
          size = 8
        ),
        label.theme = element_text(
          size = 8
        ),
        label.hjust = 4.5
      )) +
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
            sec.axis = sec_axis(~ (1 - .) * 100, name = "Levels for CI (%)", breaks = seq(0, 100, 10))
          )
        }
      } +
      {
        if (position == "pyramid") {
          scale_y_continuous(
            expand = expansion(mult = c(0.01, 0.025)),
            breaks = seq(0, 1, 0.10),
            sec.axis = sec_axis(~ (1 - .) * 100, name = "Levels for CI (%)", breaks = seq(0, 100, 10))
          )
        }
      } +
      if (nullvalue == TRUE) {
        if (measure == "default") {
          annotate("segment",
            x = 0, xend = 0, y = 0, yend = 1,
            color = "#990000", alpha = 0.4, size = .75, linetype = 3
          )
        } else if (measure == "ratio") {
          annotate("segment",
            x = 1, xend = 1, y = 0, yend = 1,
            color = "#990000", alpha = 0.4, size = .75, linetype = 3
          )
        }
      }
      else if (nullvalue == FALSE) {
      }


    # Surprisal Curve ------------------------------------------------------
  } else if (type == "s") {
    if (is(data1, "concurve") != TRUE) {
      stop("Error: 'data1' must be a data frame from 'concurve'.")
    }
    if (ncol(data1) != 7) {
      stop("Error: 'x' must be a data frame from 'concurve'.")
    }
    if (is(data2, "concurve") != TRUE) {
      stop("Error: 'data2' must be a data frame from 'concurve'.")
    }
    if (ncol(data2) != 7) {
      stop("Error: 'x' must be a data frame from 'concurve'.")
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
    if (is.character(fill1) != TRUE) {
      stop("Error: 'fill1' must be a string for the color.")
    }
    if (is.character(fill2) != TRUE) {
      stop("Error: 'fill2' must be a string for the color.")
    }
    ggplot(data = data1) +
      geom_line(aes(x = lower.limit, y = svalue),
        color = color
      ) +
      geom_line(aes(x = upper.limit, y = svalue),
        color = color
      ) +
      geom_ribbon(aes(x = lower.limit, ymin = max(svalue), ymax = svalue, fill = fill1),
        alpha = 0.20
      ) +
      geom_ribbon(aes(x = upper.limit, ymin = max(svalue), ymax = svalue, fill = fill1),
        alpha = 0.20
      ) +
      geom_line(
        data = data2, aes(x = lower.limit, y = svalue),
        color = color
      ) +
      geom_line(
        data = data2, aes(x = upper.limit, y = svalue),
        color = color
      ) +
      geom_ribbon(
        data = data2, aes(x = lower.limit, ymin = max(svalue), ymax = svalue, fill = fill2),
        alpha = 0.20
      ) +
      geom_ribbon(
        data = data2, aes(x = upper.limit, ymin = max(svalue), ymax = svalue, fill = fill2),
        fill = fill2, alpha = 0.20
      ) +
      labs(
        title = "Surprisal Functions",
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
        text = element_text(size = 11),
        legend.background = element_blank(),
        legend.position = c(.998, .25),
        legend.justification = c("right", "top"),
        legend.key = element_rect(linetype = 1, color = alpha(cols, 0.20)),
        legend.key.size = unit(0.495, "cm")
      ) +
      scale_fill_manual(
        aesthetics = "fill",
        values = cols,
        labels = c("Study 1", "Study 2")
      ) +
      guides(fill = guide_legend(
        title = "Identity",
        title.theme = element_text(
          size = 8
        ),
        label.theme = element_text(
          size = 8
        ),
        label.hjust = 4.5
      )) +
      {
        if (measure == "default") scale_x_continuous(breaks = scales::pretty_breaks(n = 5))
      } +
      {
        if (measure == "ratio") scale_x_log10(breaks = scales::pretty_breaks(n = 5))
      } +
      scale_y_continuous(breaks = seq(0, 14, 1.0), expand = c(0.0075, 0.0075))


    # Relative Likelihood Function -----------------------------------------------------
  } else if (type == "l1") {
    if (ncol(data1) != 5) {
      stop("Error: 'data1' must be a data frame from 'concurve'.")
    }
    if (ncol(data2) != 5) {
      stop("Error: 'data2' must be a data frame from 'concurve'.")
    }
    if (is.character(measure) != TRUE) {
      stop("Error: 'measure' must be a string such as 'default' or 'ratio'.")
    }
    if (is.logical(nullvalue) != TRUE) {
      stop("Error: 'nullvalue' must be a logical statement such as 'TRUE' or 'FALSE'.")
    }
    if (is.character(title) != TRUE) {
      stop("Error: 'title' must be a string.")
    }
    if (is.character(subtitle) != TRUE) {
      stop("Error: 'subtitle' must be a string.")
    }
    if (is.character(fill1) != TRUE) {
      stop("Error: 'fill1' must be a string for the color.")
    }
    if (is.character(fill2) != TRUE) {
      stop("Error: 'fill2' must be a string for the color.")
    }

    ggplot(data = data1, mapping = aes(x = values, y = support)) +
      geom_line() +
      geom_ribbon(aes(x = values, ymin = min(support), ymax = support, fill = fill1), alpha = 0.20) +
      geom_line(data = data2) +
      geom_ribbon(data = data2, aes(x = values, ymin = min(support), ymax = support, fill = fill2), alpha = 0.20) +
      labs(
        title = "Relative Likelihood Functions",
        subtitle = subtitle,
        x = xaxis,
        y = "Relative Likelihood"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 11),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        text = element_text(size = 11),
        legend.background = element_blank(),
        legend.position = c(.998, .95),
        legend.justification = c("right", "top"),
        legend.key = element_rect(linetype = 1, color = alpha(cols, 0.20)),
        legend.key.size = unit(0.495, "cm")
      ) +
      scale_fill_manual(
        aesthetics = "fill",
        values = cols,
        labels = c("Study 1", "Study 2")
      ) +
      guides(fill = guide_legend(
        title = "Identity",
        title.theme = element_text(
          size = 8
        ),
        label.theme = element_text(
          size = 8
        ),
        label.hjust = 4.5
      )) +
      {
        if (measure == "ratio") scale_x_log10(breaks = scales::pretty_breaks(n = 10))
      } +
      scale_y_continuous(expand = expansion(mult = c(0.01, 0.05)), breaks = scales::pretty_breaks(n = 10)) +
      if (nullvalue == TRUE) {
        if (measure == "default") {
          annotate("segment",
            x = 0, xend = 0, y = 0, yend = 1,
            color = "#990000", alpha = 0.4, size = .75, linetype = 3
          )
        } else if (measure == "ratio") {
          annotate("segment",
            x = 1, xend = 1, y = 0, yend = 1,
            color = "#990000", alpha = 0.4, size = .75, linetype = 3
          )
        }
      }


    # Log-Likelihood Function -----------------------------------------------------
  } else if (type == "l2") {
    if (ncol(data1) != 5) {
      stop("Error: 'data1' must be a data frame from 'concurve'.")
    }
    if (ncol(data2) != 5) {
      stop("Error: 'data2' must be a data frame from 'concurve'.")
    }
    if (is.character(measure) != TRUE) {
      stop("Error: 'measure' must be a string such as 'default' or 'ratio'.")
    }
    if (is.logical(nullvalue) != TRUE) {
      stop("Error: 'nullvalue' must be a logical statement such as 'TRUE' or 'FALSE'.")
    }
    if (is.character(title) != TRUE) {
      stop("Error: 'title' must be a string.")
    }
    if (is.character(subtitle) != TRUE) {
      stop("Error: 'subtitle' must be a string.")
    }
    if (is.character(fill1) != TRUE) {
      stop("Error: 'fill1' must be a string for the color.")
    }
    if (is.character(fill2) != TRUE) {
      stop("Error: 'fill2' must be a string for the color.")
    }

    ggplot(data = data1, mapping = aes(x = values, y = loglikelihood)) +
      geom_line() +
      geom_ribbon(aes(x = values, ymin = min(loglikelihood), ymax = loglikelihood, fill = fill1), alpha = 0.20) +
      geom_line(data = data2) +
      geom_ribbon(data = data2, aes(x = values, ymin = min(loglikelihood), ymax = loglikelihood, fill = fill2), alpha = 0.20) +
      labs(
        title = "Log-Likelihood Function",
        subtitle = subtitle,
        x = xaxis,
        y = "Log-Likelihood"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 11),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        text = element_text(size = 11),
        legend.background = element_blank(),
        legend.position = c(.998, .95),
        legend.justification = c("right", "top"),
        legend.key = element_rect(linetype = 1, color = alpha(cols, 0.20)),
        legend.key.size = unit(0.495, "cm")
      ) +
      scale_fill_manual(
        aesthetics = "fill",
        values = cols,
        labels = c("Study 1", "Study 2")
      ) +
      guides(fill = guide_legend(
        title = "Identity",
        title.theme = element_text(
          size = 8
        ),
        label.theme = element_text(
          size = 8
        ),
        label.hjust = 4.5
      )) +
      {
        if (measure == "ratio") scale_x_log10(breaks = scales::pretty_breaks(n = 10))
      } +
      scale_y_continuous(expand = expansion(mult = c(0.01, 0.05)), breaks = scales::pretty_breaks(n = 10)) +
      if (nullvalue == TRUE) {
        if (measure == "default") {
          annotate("segment",
            x = 0, xend = 0, y = 0, yend = 1,
            color = "#990000", alpha = 0.4, size = .75, linetype = 3
          )
        } else if (measure == "ratio") {
          annotate("segment",
            x = 1, xend = 1, y = 0, yend = 1,
            color = "#990000", alpha = 0.4, size = .75, linetype = 3
          )
        }
      }


    # Likelihood Function -----------------------------------------------------
  } else if (type == "l3") {
    if (ncol(data1) != 5) {
      stop("Error: 'data1' must be a data frame from 'concurve'.")
    }
    if (ncol(data2) != 5) {
      stop("Error: 'data2' must be a data frame from 'concurve'.")
    }
    if (is.character(measure) != TRUE) {
      stop("Error: 'measure' must be a string such as 'default' or 'ratio'.")
    }
    if (is.logical(nullvalue) != TRUE) {
      stop("Error: 'nullvalue' must be a logical statement such as 'TRUE' or 'FALSE'.")
    }
    if (is.character(title) != TRUE) {
      stop("Error: 'title' must be a string.")
    }
    if (is.character(subtitle) != TRUE) {
      stop("Error: 'subtitle' must be a string.")
    }
    if (is.character(fill1) != TRUE) {
      stop("Error: 'fill1' must be a string for the color.")
    }
    if (is.character(fill2) != TRUE) {
      stop("Error: 'fill2' must be a string for the color.")
    }

    ggplot(data = data1, mapping = aes(x = values, y = likelihood)) +
      geom_line() +
      geom_ribbon(aes(x = values, ymin = min(likelihood), ymax = likelihood, fill = fill1), alpha = 0.20) +
      geom_line(data = data2) +
      geom_ribbon(data = data2, aes(x = values, ymin = min(likelihood), ymax = likelihood, fill = fill2), alpha = 0.20) +
      labs(
        title = "Likelihood Function",
        subtitle = subtitle,
        x = xaxis,
        y = "Likelihood"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 11),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        text = element_text(size = 11),
        legend.background = element_blank(),
        legend.position = c(.998, .95),
        legend.justification = c("right", "top"),
        legend.key = element_rect(linetype = 1, color = alpha(cols, 0.20)),
        legend.key.size = unit(0.495, "cm")
      ) +
      scale_fill_manual(
        aesthetics = "fill",
        values = cols,
        labels = c("Study 1", "Study 2")
      ) +
      guides(fill = guide_legend(
        title = "Identity",
        title.theme = element_text(
          size = 8
        ),
        label.theme = element_text(
          size = 8
        ),
        label.hjust = 4.5
      )) +
      {
        if (measure == "ratio") scale_x_log10(breaks = scales::pretty_breaks(n = 10))
      } +
      scale_y_continuous(expand = expansion(mult = c(0.01, 0.05)), breaks = scales::pretty_breaks(n = 10)) +
      if (nullvalue == TRUE) {
        if (measure == "default") {
          annotate("segment",
            x = 0, xend = 0, y = 0, yend = 1,
            color = "#990000", alpha = 0.4, size = .75, linetype = 3
          )
        } else if (measure == "ratio") {
          annotate("segment",
            x = 1, xend = 1, y = 0, yend = 1,
            color = "#990000", alpha = 0.4, size = .75, linetype = 3
          )
        }
      }

    # Deviance Function -----------------------------------------------------
  } else if (type == "d") {
    if (ncol(data1) != 5) {
      stop("Error: 'data1' must be a data frame from 'concurve'.")
    }
    if (ncol(data2) != 5) {
      stop("Error: 'data2' must be a data frame from 'concurve'.")
    }
    if (is.character(measure) != TRUE) {
      stop("Error: 'measure' must be a string such as 'default' or 'ratio'.")
    }
    if (is.logical(nullvalue) != TRUE) {
      stop("Error: 'nullvalue' must be a logical statement such as 'TRUE' or 'FALSE'.")
    }
    if (is.character(title) != TRUE) {
      stop("Error: 'title' must be a string.")
    }
    if (is.character(subtitle) != TRUE) {
      stop("Error: 'subtitle' must be a string.")
    }
    if (is.character(fill1) != TRUE) {
      stop("Error: 'fill1' must be a string for the color.")
    }
    if (is.character(fill2) != TRUE) {
      stop("Error: 'fill2' must be a string for the color.")
    }

    ggplot(data = data1, mapping = aes(x = values, y = deviancestat)) +
      geom_line() +
      geom_ribbon(aes(x = values, ymin = deviancestat, ymax = max(deviancestat), fill = fill1), alpha = 0.20) +
      geom_line(data = data2) +
      geom_ribbon(data = data2, aes(x = values, ymin = deviancestat, ymax = max(deviancestat), fill = fill2), alpha = 0.20) +
      labs(
        title = "Deviance Functions",
        subtitle = subtitle,
        x = xaxis,
        y = "Deviance Statistic \n2ln(MLR)"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 11),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        text = element_text(size = 11),
        legend.background = element_blank(),
        legend.position = c(.998, .35),
        legend.justification = c("right", "top"),
        legend.key = element_rect(linetype = 1, color = alpha(cols, 0.20)),
        legend.key.size = unit(0.495, "cm")
      ) +
      scale_fill_manual(
        aesthetics = "fill",
        values = cols,
        labels = c("Study 1", "Study 2")
      ) +
      guides(fill = guide_legend(
        title = "Identity",
        title.theme = element_text(
          size = 8
        ),
        label.theme = element_text(
          size = 8
        ),
        label.hjust = 4.5
      )) +
      {
        if (measure == "ratio") scale_x_log10(breaks = scales::pretty_breaks(n = 10))
      } +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10), expand = c(0.0075, 0.0075))
  }
}


# RMD Check
utils::globalVariables(c("df", "lower.limit", "upper.limit", "intrvl.width", "intrvl.level", "cdf", "pvalue", "svalue"))
