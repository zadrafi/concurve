plotsint<-function(x) {
  if(is.data.frame(x) != TRUE){
    stop("Error: 'x' must be a data frame from 'concurve'")
  }
  if(ncol(x) != 5){
    stop("Error: 'x' must be a data frame from 'concurve'")
  }
  ggplot(data=x) +
    geom_point(aes(x=lower.limit, y=svalue), colour = "#d46c5b", shape=8, size=0.50) +
    geom_point(aes(x=upper.limit, y=svalue), colour = "#d46c5b", shape=8, size=0.50) +
    geom_ribbon(aes(x = lower.limit, ymin = max(svalue), ymax = svalue), fill = "#d46c5b", alpha=0.50)+
    geom_ribbon(aes(x = upper.limit, ymin = max(svalue), ymax = svalue), fill = "#d46c5b", alpha=0.50)+
    ggtitle("S-value Function") +
    labs(x="Range of Values",
         y="S-value
         (bits of information)") +
    theme_classic() +
    theme(axis.title.x = element_text(size=15),
          axis.title.y = element_text(size=15)) +
    scale_y_continuous(breaks=seq(0,14,0.5), expand = c(0, 0)) +
    theme(text = element_text(size = 15)) +
    theme(plot.title = element_text(size=16, hjust = 0.5))
}

#RMD Check
utils::globalVariables(c("df", "lower.limit", "upper.limit", "intrvl.level", "pvalue", "svalue"))
