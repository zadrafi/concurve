geom_likely<-function(data, x, y,
                   title = "Support Function",
                   subtitle = "A log-likelihood function to assess the probability of the data.",
                   caption = "Produced with the concurve R package.",
                   xaxis = "Theta",
                   yaxis = "Log-likelihood",
                   fill = "#239a98") {
  if(is.data.frame(data) != TRUE){
    stop("Error: 'data' must be a data frame from 'concurve'")
  }
  if(ncol(data) != 4){
    stop("Error: 'data' must be a data frame from 'concurve'")
  }
  ggplot(data = data, aes(x = x, y = y)) +
    geom_line() +
    geom_ribbon(aes(x = x, ymin = min(y), ymax = y), fill = fill, alpha = 0.30) +
    labs(title = title,
         subtitle = subtitle,
         caption = caption,
         x = xaxis,
         y = yaxis) +
    theme_light() +
    theme(axis.title.x = element_text(size = 13),
          axis.title.y = element_text(size = 13)) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
    theme(text = element_text(size = 15)) +
    theme(plot.title = element_text(size = 16),
          plot.subtitle = element_text(size = 12),
          plot.caption = element_text(size = 8))
}
