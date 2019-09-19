ggconcurve <- function(type = "consonance", data, measure = "default", nullvalue = "absent", position = "pyramid",
                       title = "Consonance Function",
                       subtitle = "The function contains consonance intervals at every level.",
                       xaxis = "Range of Values",
                       yaxis = "P-value",
                       color = "#555555",
                       fill = "#239a98") {
  if (type == "consonance") {
    if (is.data.frame(data) != TRUE) {
      stop("Error: 'x' must be a data frame from 'concurve'.")
    }
    if (ncol(data) != 5) {
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
    if (is.character(fill) != TRUE) {
      stop("Error: 'fill' must be a string for the color.")
    }
    ggplot(data = data) +
      geom_point(aes(x = lower.limit, y = pvalue),
        color = color, fill = fill, alpha = 0.5, shape = 20, size = 0.1
      ) +
      geom_point(aes(x = upper.limit, y = pvalue),
        color = color, fill = fill, alpha = 0.5, shape = 20, size = 0.1
      ) +
      geom_ribbon(aes(x = lower.limit, ymin = min(pvalue), ymax = pvalue),
        fill = fill, alpha = 0.30
      ) +
      geom_ribbon(aes(x = upper.limit, ymin = min(pvalue), ymax = pvalue),
        fill = fill, alpha = 0.30
      ) +
      labs(
        title = title,
        subtitle = subtitle,
        x = xaxis,
        y = yaxis
      ) +
      theme_light() +
      theme(
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12)
      ) + {
        if (measure == "default") scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
      } + {
        if (measure == "ratio") scale_x_log10(breaks = scales::pretty_breaks(n = 10))
      } + {
        if (position == "inverted") {
          scale_y_reverse(
            breaks = seq(0, 1, .05),
            sec.axis = sec_axis(~ (1 - .) * 100, name = "Levels for CI (%)", breaks = seq(0, 100, 5))
          )
        }
      } + {
        if (position == "pyramid") {
          scale_y_continuous(
            breaks = seq(0, 1, .05),
            sec.axis = sec_axis(~ (1 - .) * 100, name = "Levels for CI (%)", breaks = seq(0, 100, 5))
          )
        }
      } +
      theme(text = element_text(size = 11)) +
      theme(
        plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 11)
      ) +
      if (nullvalue == "present") {
        if (measure == "default") {
          annotate("segment",
            x = 0, xend = 0, y = 0, yend = 1,
            color = "#990000", alpha = 0.3, size = .6
          )
        } else if (measure == "ratio") {
          annotate("segment",
            x = 1, xend = 1, y = 0, yend = 1,
            color = "#990000", alpha = 0.3, size = .6
          )
        }
      }
      else if (nullvalue == "absent") {
      }
  } else if (type == "surprisal") {
    if (is.data.frame(data) != TRUE) {
      stop("Error: 'data' must be a data frame from 'concurve'")
    }
    if (ncol(data) != 5) {
      stop("Error: 'data' must be a data frame from 'concurve'")
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
    if (is.character(fill) != TRUE) {
      stop("Error: 'fill' must be a string for the color.")
    }
    ggplot(data = data) +
      geom_point(aes(x = lower.limit, y = svalue),
        color = color, fill = fill, alpha = 0.5, shape = 20, size = 0.1
      ) +
      geom_point(aes(x = upper.limit, y = svalue),
        color = color, fill = fill, alpha = 0.5, shape = 20, size = 0.1
      ) +
      geom_ribbon(aes(x = lower.limit, ymin = max(svalue), ymax = svalue),
        fill = fill, alpha = 0.30
      ) +
      geom_ribbon(aes(x = upper.limit, ymin = max(svalue), ymax = svalue),
        fill = fill, alpha = 0.30
      ) +
      labs(
        title = title,
        subtitle = subtitle,
        x = xaxis,
        y = "S-value (bits of information)"
      ) +
      theme_light() +
      theme(
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12)
      ) + {
        if (measure == "default") scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
      } + {
        if (measure == "ratio") scale_x_log10(breaks = scales::pretty_breaks(n = 10))
      } +
      scale_y_continuous(breaks = seq(0, 14, 0.5), expand = c(0, 0)) +
      theme(text = element_text(size = 11)) +
      theme(
        plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 11)
      )
  }
}

# RMD Check
utils::globalVariables(c("df", "lower.limit", "upper.limit", "intrvl.level", "pvalue", "svalue"))
