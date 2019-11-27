ggconcurve <- function(data, type = "c", measure = "default", levels = 0.95, nullvalue = FALSE, position = "pyramid",
                       title = "Interval Function",
                       subtitle = "The function displays intervals at every level.",
                       xaxis = expression(Theta ~ "Range of Values"),
                       yaxis = "P-value",
                       color = "#000000",
                       fill = "#239a98") {
  Cairo.capabilities()

  # Consonance Function -----------------------------------------------------

  if (type == "c") {
    if (is.data.frame(data) != TRUE) {
      stop("Error: 'data' must be a data frame from 'concurve'.")
    }
    if (ncol(data) != 6) {
      stop("Error: 'data' must be a data frame from 'concurve'.")
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
    if (is.character(fill) != TRUE) {
      stop("Error: 'fill' must be a string for the color.")
    }


    # Plotting Intervals ------------------------------------------------------

    interval <- mclapply(levels, FUN = function(i) (c(i, subset(data, intrvl.level == i)[, 1], subset(data, intrvl.level == i)[, 2])), mc.cores = detectCores(logical = FALSE) - 1)
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
      geom_point(data = interval, mapping = aes(x = limits, y = 1 - levels), size = .95) +
      geom_line(data = interval, mapping = aes(x = limits, y = 1 - levels, group = levels), size = .40) +
      geom_ribbon(aes(x = lower.limit, ymin = min(pvalue), ymax = pvalue),
        fill = fill, alpha = 0.20
      ) +
      geom_ribbon(aes(x = upper.limit, ymin = min(pvalue), ymax = pvalue),
        fill = fill, alpha = 0.20
      ) +
      labs(
        title = "Consonance Function",
        subtitle = subtitle,
        x = xaxis,
        y = yaxis
      ) +
      theme_hc() +
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
    if (is.data.frame(data) != TRUE) {
      stop("Error: 'data' must be a data frame from 'concurve'.")
    }
    if (ncol(data) != 6) {
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
    if (is.character(yaxis) != TRUE) {
      stop("Error: 'yaxis' must be a string.")
    }
    if (is.character(fill) != TRUE) {
      stop("Error: 'fill' must be a string for the color.")
    }


    # Plotting Intervals ------------------------------------------------------

    interval <- mclapply(levels, FUN = function(i) (c(i, subset(data, intrvl.level == i)[, 1], subset(data, intrvl.level == i)[, 2])), mc.cores = detectCores(logical = FALSE) - 1)
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
      geom_point(data = interval, mapping = aes(x = limits, y = (-log2(1 - levels))), size = .95) +
      geom_line(data = interval, mapping = aes(x = limits, y = (-log2(1 - levels)), group = levels), size = .40) +
      geom_ribbon(aes(x = lower.limit, ymin = max(svalue), ymax = svalue),
        fill = fill, alpha = 0.30
      ) +
      geom_ribbon(aes(x = upper.limit, ymin = max(svalue), ymax = svalue),
        fill = fill, alpha = 0.30
      ) +
      labs(
        title = "Surprisal Function",
        subtitle = subtitle,
        x = xaxis,
        y = "S-value \n(Bits of Information)"
      ) +
      theme_hc() +
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


    # Relative Likelihood Function -----------------------------------------------------
  } else if (type == "l1") {
    if (is.data.frame(data) != TRUE) {
      stop("Error: 'data' must be a data frame from 'concurve'.")
    }
    if (ncol(data) != 6) {
      stop("Error: 'data' must be a data frame from 'concurve'.")
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
    if (is.character(fill) != TRUE) {
      stop("Error: 'fill' must be a string for the color.")
    }

    ggplot(data = data, mapping = aes(x = values, y = support)) +
      geom_line() +
      geom_ribbon(aes(x = values, ymin = min(support), ymax = support), fill = "#239a98", alpha = 0.30) +
      labs(
        title = "Relative Likelihood Function",
        subtitle = subtitle,
        x = xaxis,
        y = "Relative Likelihood \n(1/MLR)"
      ) +
      theme_hc() +
      theme(
        plot.title = element_text(size = 16),
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
    if (is.data.frame(data) != TRUE) {
      stop("Error: 'data' must be a data frame from 'concurve'.")
    }
    if (ncol(data) != 6) {
      stop("Error: 'data' must be a data frame from 'concurve'.")
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
    if (is.character(fill) != TRUE) {
      stop("Error: 'fill' must be a string for the color.")
    }

    ggplot(data = data, mapping = aes(x = values, y = loglikelihood)) +
      geom_line() +
      geom_ribbon(aes(x = values, ymin = min(loglikelihood), ymax = loglikelihood), fill = "#239a98", alpha = 0.30) +
      labs(
        title = "Log-Likelihood Function",
        subtitle = subtitle,
        x = xaxis,
        y = "Log Likelihood"
      ) +
      theme_hc() +
      theme(
        plot.title = element_text(size = 16),
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
    if (is.data.frame(data) != TRUE) {
      stop("Error: 'data' must be a data frame from 'concurve'.")
    }
    if (ncol(data) != 6) {
      stop("Error: 'data' must be a data frame from 'concurve'.")
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
    if (is.character(fill) != TRUE) {
      stop("Error: 'fill' must be a string for the color.")
    }

    ggplot(data = data, mapping = aes(x = values, y = likelihood)) +
      geom_line() +
      geom_ribbon(aes(x = values, ymin = min(likelihood), ymax = likelihood), fill = "#239a98", alpha = 0.30) +
      labs(
        title = "Likelihood Function",
        subtitle = subtitle,
        x = xaxis,
        y = "Likelihood"
      ) +
      theme_hc() +
      theme(
        plot.title = element_text(size = 16),
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
    if (is.data.frame(data) != TRUE) {
      stop("Error: 'data' must be a data frame from 'concurve'.")
    }
    if (ncol(data) != 6) {
      stop("Error: 'data' must be a data frame from 'concurve'.")
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
    if (is.character(fill) != TRUE) {
      stop("Error: 'fill' must be a string for the color.")
    }

    ggplot(data = data, mapping = aes(x = values, y = deviancestat)) +
      geom_line() +
      geom_ribbon(aes(x = values, ymin = deviancestat, ymax = max(deviancestat)), fill = "#239a98", alpha = 0.30) +
      labs(
        title = "Deviance Function",
        subtitle = subtitle,
        x = xaxis,
        y = "Deviance Statistic \n2ln(MLR)"
      ) +
      theme_hc() +
      theme(
        plot.title = element_text(size = 16),
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
utils::globalVariables(c("df", "lower.limit", "upper.limit", "limit.ratio", "intrvl.level", "pvalue", "svalue"))
