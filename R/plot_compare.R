plot_compare <- function(data1, data2, type = "c", measure = "default", nullvalue = FALSE, position = "pyramid",
                         title = "Interval Functions",
                         subtitle = "The function displays intervals at every level.",
                         xaxis = expression(Theta ~ "Range of Values"),
                         yaxis = "P-value",
                         color = "#000000",
                         fill1 = "#239a98",
                         fill2 = "#d46c5b") {
  Cairo.capabilities()
  cols <- c(fill1, fill2)

  # Consonance Function -----------------------------------------------------

  if (type == "c") {
    if (is.data.frame(data1) != TRUE) {
      stop("Error: 'x' must be a data frame from 'concurve'.")
    }
    if (ncol(data1) != 6) {
      stop("Error: 'x' must be a data frame from 'concurve'.")
    }
    if (is.data.frame(data2) != TRUE) {
      stop("Error: 'x' must be a data frame from 'concurve'.")
    }
    if (ncol(data2) != 6) {
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
    if (is.character(yaxis) != TRUE) {
      stop("Error: 'yaxis' must be a string.")
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
        alpha = 0.30
      ) +
      geom_ribbon(aes(x = upper.limit, ymin = min(pvalue), ymax = pvalue, fill = fill1),
        alpha = 0.30
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
        alpha = 0.30
      ) +
      geom_ribbon(
        data = data2, aes(x = upper.limit, ymin = min(pvalue), ymax = pvalue, fill = fill2),
        alpha = 0.30
      ) +
      theme_hc() +
      labs(
        title = title,
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
        legend.key = element_rect(linetype = 1),
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
        if (measure == "default") scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
      } +
      {
        if (measure == "ratio") scale_x_log10(breaks = scales::pretty_breaks(n = 10))
      } +
      {
        if (position == "inverted") {
          scale_y_reverse(
            breaks = seq(0, 1, .05),
            sec.axis = sec_axis(~ (1 - .) * 100, name = "Levels for CI (%)", breaks = seq(0, 100, 5))
          )
        }
      } +
      {
        if (position == "pyramid") {
          scale_y_continuous(
            breaks = seq(0, 1, .05),
            sec.axis = sec_axis(~ (1 - .) * 100, name = "Levels for CI (%)", breaks = seq(0, 100, 5))
          )
        }
      } +
      if (nullvalue == TRUE) {
        if (measure == "default") {
          annotate("segment",
            x = 0, xend = 0, y = 0, yend = 1,
            color = "#990000", alpha = 0.3, size = .75, linetype = 1
          )
        } else if (measure == "ratio") {
          annotate("segment",
            x = 1, xend = 1, y = 0, yend = 1,
            color = "#990000", alpha = 0.3, size = .75, linetype = 1
          )
        }
      }
      else if (nullvalue == FALSE) {
      }


    # Surprisal Function ------------------------------------------------------
  } else if (type == "s") {
    if (is.data.frame(data1) != TRUE) {
      stop("Error: 'x' must be a data frame from 'concurve'.")
    }
    if (ncol(data1) != 6) {
      stop("Error: 'x' must be a data frame from 'concurve'.")
    }
    if (is.data.frame(data2) != TRUE) {
      stop("Error: 'x' must be a data frame from 'concurve'.")
    }
    if (ncol(data2) != 6) {
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
    if (is.character(yaxis) != TRUE) {
      stop("Error: 'yaxis' must be a string.")
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
        alpha = 0.30
      ) +
      geom_ribbon(aes(x = upper.limit, ymin = max(svalue), ymax = svalue, fill = fill1),
        alpha = 0.30
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
        alpha = 0.30
      ) +
      geom_ribbon(
        data = data2, aes(x = upper.limit, ymin = max(svalue), ymax = svalue, fill = fill2),
        fill = fill2, alpha = 0.30
      ) +
      labs(
        title = title,
        subtitle = subtitle,
        x = xaxis,
        y = "S-value (bits of information)"
      ) +
      theme_hc() +
      theme(
        plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 11),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        text = element_text(size = 11),
        legend.background = element_blank(),
        legend.position = c(.998, .25),
        legend.justification = c("right", "top"),
        legend.key = element_rect(linetype = 1),
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
        if (measure == "default") scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
      } +
      {
        if (measure == "ratio") scale_x_log10(breaks = scales::pretty_breaks(n = 10))
      } +
      scale_y_continuous(breaks = seq(0, 14, 0.5), expand = c(0, 0))


    # Relative Likelihood Function -----------------------------------------------------
  } else if (type == "l1") {
    if (ncol(data1) != 6) {
      stop("Error: 'data1' must be a data frame from 'concurve'.")
    }
    if (ncol(data2) != 6) {
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
    if (is.character(yaxis) != TRUE) {
      stop("Error: 'yaxis' must be a string.")
    }
    if (is.character(fill1) != TRUE) {
      stop("Error: 'fill1' must be a string for the color.")
    }
    if (is.character(fill2) != TRUE) {
      stop("Error: 'fill2' must be a string for the color.")
    }

    ggplot(data = data1, mapping = aes(x = values, y = support)) +
      geom_line() +
      geom_ribbon(aes(x = values, ymin = min(support), ymax = support, fill = fill1), alpha = 0.30) +
      geom_line(data = data2) +
      geom_ribbon(data = data2, aes(x = values, ymin = min(support), ymax = support, fill = fill2), alpha = 0.30) +
      labs(
        title = "Relative Likelihood Functions",
        subtitle = subtitle,
        x = xaxis,
        y = "Relative Likelihood \n(1/MLR)"
      ) +
      theme_hc() +
      theme(
        plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 11),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        text = element_text(size = 11),
        legend.background = element_blank(),
        legend.position = c(.998, .95),
        legend.justification = c("right", "top"),
        legend.key = element_rect(linetype = 1),
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
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
      if (nullvalue == TRUE) {
        if (measure == "default") {
          annotate("segment",
            x = 0, xend = 0, y = 0, yend = 1,
            color = "#990000", alpha = 0.3, size = .75, linetype = 1
          )
        } else if (measure == "ratio") {
          annotate("segment",
            x = 1, xend = 1, y = 0, yend = 1,
            color = "#990000", alpha = 0.3, size = .75, linetype = 1
          )
        }
      }


    # Log-Likelihood Function -----------------------------------------------------
  } else if (type == "l2") {
    if (ncol(data1) != 6) {
      stop("Error: 'data1' must be a data frame from 'concurve'.")
    }
    if (ncol(data2) != 6) {
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
    if (is.character(yaxis) != TRUE) {
      stop("Error: 'yaxis' must be a string.")
    }
    if (is.character(fill1) != TRUE) {
      stop("Error: 'fill1' must be a string for the color.")
    }
    if (is.character(fill2) != TRUE) {
      stop("Error: 'fill2' must be a string for the color.")
    }

    ggplot(data = data1, mapping = aes(x = values, y = loglikelihood)) +
      geom_line() +
      geom_ribbon(aes(x = values, ymin = min(loglikelihood), ymax = loglikelihood, fill = fill1), alpha = 0.30) +
      geom_line(data = data2) +
      geom_ribbon(data = data2, aes(x = values, ymin = min(loglikelihood), ymax = loglikelihood, fill = fill2), alpha = 0.30) +
      labs(
        title = "Log-Likelihood Functions",
        subtitle = subtitle,
        x = xaxis,
        y = "Log-Likelihood"
      ) +
      theme_hc() +
      theme(
        plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 11),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        text = element_text(size = 11),
        legend.background = element_blank(),
        legend.position = c(.998, .95),
        legend.justification = c("right", "top"),
        legend.key = element_rect(linetype = 1),
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
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
      if (nullvalue == TRUE) {
        if (measure == "default") {
          annotate("segment",
            x = 0, xend = 0, y = 0, yend = 1,
            color = "#990000", alpha = 0.3, size = .75, linetype = 1
          )
        } else if (measure == "ratio") {
          annotate("segment",
            x = 1, xend = 1, y = 0, yend = 1,
            color = "#990000", alpha = 0.3, size = .75, linetype = 1
          )
        }
      }


    # Likelihood Function -----------------------------------------------------
  } else if (type == "l3") {
    if (ncol(data1) != 6) {
      stop("Error: 'data1' must be a data frame from 'concurve'.")
    }
    if (ncol(data2) != 6) {
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
    if (is.character(yaxis) != TRUE) {
      stop("Error: 'yaxis' must be a string.")
    }
    if (is.character(fill1) != TRUE) {
      stop("Error: 'fill1' must be a string for the color.")
    }
    if (is.character(fill2) != TRUE) {
      stop("Error: 'fill2' must be a string for the color.")
    }

    ggplot(data = data1, mapping = aes(x = values, y = likelihood)) +
      geom_line() +
      geom_ribbon(aes(x = values, ymin = min(likelihood), ymax = likelihood, fill = fill1), alpha = 0.30) +
      geom_line(data = data2) +
      geom_ribbon(data = data2, aes(x = values, ymin = min(likelihood), ymax = likelihood, fill = fill2), alpha = 0.30) +
      labs(
        title = "Likelihood Functions",
        subtitle = subtitle,
        x = xaxis,
        y = "Likelihood"
      ) +
      theme_hc() +
      theme(
        plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 11),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        text = element_text(size = 11),
        legend.background = element_blank(),
        legend.position = c(.998, .95),
        legend.justification = c("right", "top"),
        legend.key = element_rect(linetype = 1),
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
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
      if (nullvalue == TRUE) {
        if (measure == "default") {
          annotate("segment",
            x = 0, xend = 0, y = 0, yend = 1,
            color = "#990000", alpha = 0.3, size = .75, linetype = 1
          )
        } else if (measure == "ratio") {
          annotate("segment",
            x = 1, xend = 1, y = 0, yend = 1,
            color = "#990000", alpha = 0.3, size = .75, linetype = 1
          )
        }
      }

    # Deviance Function -----------------------------------------------------
  } else if (type == "d") {
    if (ncol(data1) != 6) {
      stop("Error: 'data1' must be a data frame from 'concurve'.")
    }
    if (ncol(data2) != 6) {
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
    if (is.character(yaxis) != TRUE) {
      stop("Error: 'yaxis' must be a string.")
    }
    if (is.character(fill1) != TRUE) {
      stop("Error: 'fill1' must be a string for the color.")
    }
    if (is.character(fill2) != TRUE) {
      stop("Error: 'fill2' must be a string for the color.")
    }

    ggplot(data = data1, mapping = aes(x = values, y = deviancestat)) +
      geom_line() +
      geom_ribbon(aes(x = values, ymin = deviancestat, ymax = max(deviancestat), fill = fill1), alpha = 0.30) +
      geom_line(data = data2) +
      geom_ribbon(data = data2, aes(x = values, ymin = deviancestat, ymax = max(deviancestat), fill = fill2), alpha = 0.30) +
      labs(
        title = "Deviance Functions",
        subtitle = subtitle,
        x = xaxis,
        y = "Deviance Statistic \n2ln(MLR)"
      ) +
      theme_hc() +
      theme(
        plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 11),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        text = element_text(size = 11),
        legend.background = element_blank(),
        legend.position = c(.998, .35),
        legend.justification = c("right", "top"),
        legend.key = element_rect(linetype = 1),
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
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
      if (nullvalue == TRUE) {
        if (measure == "default") {
          annotate("segment",
            x = 0, xend = 0, y = 0, yend = 1,
            color = "#990000", alpha = 0.3, size = .8, linetype = 1
          )
        } else if (measure == "ratio") {
          annotate("segment",
            x = 1, xend = 1, y = 0, yend = 1,
            color = "#990000", alpha = 0.3, size = .8, linetype = 1
          )
        }
      }
  }
}


# RMD Check
utils::globalVariables(c("df", "lower.limit", "upper.limit", "intrvl.width", "intrvl.level", "pvalue", "svalue"))
