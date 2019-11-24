plot_compare <- function(data1, data2, type = "consonance", measure = "default", nullvalue = "absent", position = "pyramid",
                         title = "Interval Functions",
                         subtitle = "The function displays intervals at every level.",
                         xaxis = "\u0398 Range of Values",
                         yaxis = "P-value",
                         color = "#555555",
                         fill1 = "#239a98",
                         fill2 = "#d46c5b") {


  # Consonance Function -----------------------------------------------------
  if (type == "consonance") {
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
    if (is.character(nullvalue) != TRUE) {
      stop("Error: 'nullvalue' must be a string such as 'absent' or 'present'.")
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
    if (is.character(xaxis) != TRUE) {
      stop("Error: 'xaxis' must be a string.")
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
      geom_point(aes(x = lower.limit, y = pvalue),
        color = color, fill = fill1, alpha = 0.5, shape = 20, size = 0.1
      ) +
      geom_point(aes(x = upper.limit, y = pvalue),
        color = color, fill = fill1, alpha = 0.5, shape = 20, size = 0.1
      ) +
      geom_ribbon(aes(x = lower.limit, ymin = min(pvalue), ymax = pvalue),
        fill = fill1, alpha = 0.30
      ) +
      geom_ribbon(aes(x = upper.limit, ymin = min(pvalue), ymax = pvalue),
        fill = fill1, alpha = 0.30
      ) +
      geom_point(
        data = data2, aes(x = lower.limit, y = pvalue),
        color = color, fill = fill2, alpha = 0.5, shape = 20, size = 0.1
      ) +
      geom_point(
        data = data2, aes(x = upper.limit, y = pvalue),
        color = color, fill = fill2, alpha = 0.5, shape = 20, size = 0.1
      ) +
      geom_ribbon(
        data = data2, aes(x = lower.limit, ymin = min(pvalue), ymax = pvalue),
        fill = fill2, alpha = 0.30
      ) +
      geom_ribbon(
        data = data2, aes(x = upper.limit, ymin = min(pvalue), ymax = pvalue),
        fill = fill2, alpha = 0.30
      ) +
      theme_bw() +
      labs(
        title = title,
        subtitle = subtitle,
        x = xaxis,
        y = yaxis,
        color = fill1
      ) +
      theme(
        plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 11),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        text = element_text(size = 11),
        legend.position = "bottom"
      ) +
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
      if (nullvalue == "present") {
        if (measure == "default") {
          annotate("segment",
            x = 0, xend = 0, y = 0, yend = 1,
            color = "#990000", alpha = 0.3, size = .99, linetype = 1
          )
        } else if (measure == "ratio") {
          annotate("segment",
            x = 1, xend = 1, y = 0, yend = 1,
            color = "#990000", alpha = 0.3, size = .99, linetype = 1
          )
        }
      }
      else if (nullvalue == "absent") {
      }


    # Surprisal Function ------------------------------------------------------
  } else if (type == "surprisal") {
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
    if (is.character(xaxis) != TRUE) {
      stop("Error: 'xaxis' must be a string.")
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
      geom_point(aes(x = lower.limit, y = svalue),
        color = color, fill = fill1, alpha = 0.5, shape = 20, size = 0.1
      ) +
      geom_point(aes(x = upper.limit, y = svalue),
        color = color, fill = fill1, alpha = 0.5, shape = 20, size = 0.1
      ) +
      geom_ribbon(aes(x = lower.limit, ymin = max(svalue), ymax = svalue),
        fill = fill1, alpha = 0.30
      ) +
      geom_ribbon(aes(x = upper.limit, ymin = max(svalue), ymax = svalue),
        fill = fill1, alpha = 0.30
      ) +
      geom_point(
        data = data2, aes(x = lower.limit, y = svalue),
        color = color, fill = fill2, alpha = 0.5, shape = 20, size = 0.1
      ) +
      geom_point(
        data = data2, aes(x = upper.limit, y = svalue),
        color = color, fill = fill2, alpha = 0.5, shape = 20, size = 0.1
      ) +
      geom_ribbon(
        data = data2, aes(x = lower.limit, ymin = max(svalue), ymax = svalue),
        fill = fill2, alpha = 0.30
      ) +
      geom_ribbon(
        data = data2, aes(x = upper.limit, ymin = max(svalue), ymax = svalue),
        fill = fill2, alpha = 0.30
      ) +
      labs(
        title = title,
        subtitle = subtitle,
        x = xaxis,
        y = "S-value (bits of information)"
      ) +
      theme_bw() +
      theme(
        plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 11),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        text = element_text(size = 11)
      ) +
      {
        if (measure == "default") scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
      } +
      {
        if (measure == "ratio") scale_x_log10(breaks = scales::pretty_breaks(n = 10))
      } +
      scale_y_continuous(breaks = seq(0, 14, 0.5), expand = c(0, 0))


    # Likelihood Function -----------------------------------------------------
  } else if (type == "likelihood") {
    if (ncol(data1) != 4) {
      stop("Error: 'data1' must be a data frame from 'concurve'.")
    }
    if (ncol(data2) != 4) {
      stop("Error: 'data2' must be a data frame from 'concurve'.")
    }
    if (is.character(measure) != TRUE) {
      stop("Error: 'measure' must be a string such as 'default' or 'ratio'.")
    }
    if (is.character(nullvalue) != TRUE) {
      stop("Error: 'nullvalue' must be a string such as 'absent' or 'present'.")
    }
    if (is.character(title) != TRUE) {
      stop("Error: 'title' must be a string.")
    }
    if (is.character(subtitle) != TRUE) {
      stop("Error: 'subtitle' must be a string.")
    }
    if (is.character(xaxis) != TRUE) {
      stop("Error: 'xaxis' must be a string.")
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
      geom_ribbon(aes(x = values, ymin = min(support), ymax = support), fill = fill1, alpha = 0.30) +
      geom_line(data = data2) +
      geom_ribbon(data = data2, aes(x = values, ymin = min(support), ymax = support), fill = fill2, alpha = 0.30) +
      labs(
        title = "Likelihood Functions",
        subtitle = subtitle,
        x = xaxis,
        y = "Relative Likelihood \n(1/MLR)"
      ) +
      theme_bw() +
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
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
      if (nullvalue == "present") {
        if (measure == "default") {
          annotate("segment",
            x = 0, xend = 0, y = 0, yend = 1,
            color = "#990000", alpha = 0.3, size = .99, linetype = 1
          )
        } else if (measure == "ratio") {
          annotate("segment",
            x = 1, xend = 1, y = 0, yend = 1,
            color = "#990000", alpha = 0.3, size = .99, linetype = 1
          )
        }
      }
    # Deviance Function -----------------------------------------------------
  } else if (type == "deviance") {
    if (ncol(data1) != 4) {
      stop("Error: 'data1' must be a data frame from 'concurve'.")
    }
    if (ncol(data2) != 4) {
      stop("Error: 'data2' must be a data frame from 'concurve'.")
    }
    if (is.character(measure) != TRUE) {
      stop("Error: 'measure' must be a string such as 'default' or 'ratio'.")
    }
    if (is.character(nullvalue) != TRUE) {
      stop("Error: 'nullvalue' must be a string such as 'absent' or 'present'.")
    }
    if (is.character(title) != TRUE) {
      stop("Error: 'title' must be a string.")
    }
    if (is.character(subtitle) != TRUE) {
      stop("Error: 'subtitle' must be a string.")
    }
    if (is.character(xaxis) != TRUE) {
      stop("Error: 'xaxis' must be a string.")
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
      geom_ribbon(aes(x = values, ymin = deviancestat, ymax = max(deviancestat)), fill = fill1, alpha = 0.30) +
      geom_line(data = data2) +
      geom_ribbon(data = data2, aes(x = values, ymin = deviancestat, ymax = max(deviancestat)), fill = fill2, alpha = 0.30) +
      labs(
        title = "Deviance Functions",
        subtitle = subtitle,
        x = xaxis,
        y = "Deviance Statistic \n2ln(MLR)"
      ) +
      theme_bw() +
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
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
      if (nullvalue == "present") {
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
utils::globalVariables(c("df", "lower.limit", "upper.limit", "limit.ratio", "intrvl.level", "pvalue", "svalue"))
