plotpint<-function(x, measure="default",
                   title = "Consonance Function",
                   subtitle = "The function contains consonance/confidence intervals at every level and the \ncorresponding P-values.",
                   caption = "Produced with the concurve R package.",
                   xaxis = "Range of Values",
                   yaxis = "Consonance Level (%)",
                   color = "#239a98") {
  if(is.data.frame(x) != TRUE){
    stop("Error: 'x' must be a data frame from 'concurve'")
  }
  if(ncol(x) != 5){
    stop("Error: 'x' must be a data frame from 'concurve'")
  }
  if(is.character(measure) != TRUE){
    stop("Error: 'measure' must be a string such as 'default' or 'ratio'")
  }
  ggplot(data = x) +
    geom_point(aes(x = lower.limit, y = intrvl.level*100), color = color, fill = color, alpha = 0.5, shape = 21, stroke = 0.8, size = 1) +
    geom_point(aes(x = upper.limit, y = intrvl.level*100), color = color, fill = color, alpha = 0.5, shape = 21, stroke = 0.8, size = 1) +
    geom_ribbon(aes(x = lower.limit, ymin = max(intrvl.level*100), ymax = intrvl.level*100), fill = color, alpha = 0.30) +
    geom_ribbon(aes(x = upper.limit, ymin = max(intrvl.level*100), ymax = intrvl.level*100), fill = color, alpha = 0.30) +
    labs(title = title,
         subtitle = subtitle,
         caption = caption,
         x = xaxis,
         y = yaxis) +
    theme_light() +
    theme(axis.title.x = element_text(size = 13),
          axis.title.y = element_text(size = 13)) +
 {if(measure == "default") scale_x_continuous(breaks = scales::pretty_breaks(n = 10))} +
 {if(measure == "ratio") scale_x_log10(breaks = scales::pretty_breaks(n = 10))} +
    scale_y_continuous(breaks=seq(0,100,5),
                       expand = c(0, 0)) +
    theme(text = element_text(size = 15)) +
    theme(plot.title = element_text(size = 16),
          plot.subtitle = element_text(size = 12),
          plot.caption = element_text(size = 8)) +
  if(measure == "default") {
    annotate("segment", x = 0, xend = 0, y = 0, yend = 100,
             colour = "#990000", alpha = 0.5, size = 1)
  } else if(measure == "ratio") {
    annotate("segment", x = 1, xend = 1, y = 0, yend = 100,
              colour = "#990000", alpha = 0.5, size = 1)
  }
}

# RMD Check
utils::globalVariables(c("df", "lower.limit", "upper.limit", "intrvl.level", "pvalue", "svalue"))
