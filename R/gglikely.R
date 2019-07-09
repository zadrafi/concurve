gglikely <- function(data, x, y,
                     title = "Support Function",
                     subtitle = "A log-likelihood function to assess the probability of the data.",
                     caption = "Produced with the concurve R package.",
                     xaxis = "Theta",
                     yaxis = "Log-likelihood",
                     fill = "#239a98") {
  if (is.data.frame(data) != TRUE) {
    stop("Error: 'data' must be a data frame from 'concurve'")
  }
  if (ncol(data) != 4) {
    stop("Error: 'data' must be a data frame from 'concurve'")
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
  ggplot(data = data, aes(x = x, y = y)) +
    geom_line() +
    geom_ribbon(aes(x = x, ymin = min(y), ymax = y), fill = fill, alpha = 0.30) +
    labs(
      title = title,
      subtitle = subtitle,
      caption = caption,
      x = xaxis,
      y = yaxis
    ) +
    theme_light() +
    theme(
      axis.title.x = element_text(size = 13),
      axis.title.y = element_text(size = 13)
    ) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
    theme(text = element_text(size = 15)) +
    theme(
      plot.title = element_text(size = 16),
      plot.subtitle = element_text(size = 12),
      plot.caption = element_text(size = 8)
    )
}

# RMD Check
utils::globalVariables(c("df", "lower.limit", "upper.limit", "intrvl.level", "pvalue", "svalue"))
