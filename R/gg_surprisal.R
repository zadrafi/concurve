gg_surprisal <- function(x, measure = "default",
                   title = "Surprisal Function",
                   subtitle = "The function contains consonance/confidence intervals at every level and the \ncorresponding S-values.",
                   caption = "Produced with the concurve R package.",
                   xaxis = "Range of Values",
                   yaxis = "S-value (bits of information)",
                   color = "#555555",
                   fill = "#1f7f79") {
  if (is.data.frame(x) != TRUE) {
    stop("Error: 'x' must be a data frame from 'concurve'")
  }
  if (ncol(x) != 5) {
    stop("Error: 'x' must be a data frame from 'concurve'")
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
  if (is.character(caption) != TRUE) {
    stop("Error: 'caption' must be a string.")
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
  ggplot(data = x) +
    geom_point(aes(x = lower.limit, y = svalue), color = color, fill = fill, alpha = 0.5, shape = 20, size = 0.1) +
    geom_point(aes(x = upper.limit, y = svalue), color = color, fill = fill, alpha = 0.5, shape = 20, size = 0.1) +
    geom_ribbon(aes(x = lower.limit, ymin = max(svalue), ymax = svalue), fill = fill, alpha = 0.30) +
    geom_ribbon(aes(x = upper.limit, ymin = max(svalue), ymax = svalue), fill = fill, alpha = 0.30) +
    labs(title = title,
         subtitle = subtitle,
         caption = caption,
         x = xaxis,
         y = yaxis) +
    theme_light() +
    theme(axis.title.x = element_text(size = 13),
          axis.title.y = element_text(size = 13)) +
   {if (measure == "default") scale_x_continuous(breaks = scales::pretty_breaks(n = 10))} +
   {if (measure == "ratio") scale_x_log10(breaks = scales::pretty_breaks(n = 10))} +
    scale_y_continuous(breaks = seq(0,14,0.5), expand = c(0, 0)) +
    theme(text = element_text(size = 15)) +
    theme(plot.title = element_text(size = 16),
          plot.subtitle = element_text(size = 12),
          plot.caption = element_text(size = 8))
}

# RMD Check
utils::globalVariables(c("df", "lower.limit", "upper.limit", "intrvl.level", "pvalue", "svalue"))
