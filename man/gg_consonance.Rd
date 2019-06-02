\name{gg_consonance}
\alias{gg_consonance}

\title{
Plots the P-Value (Consonance) Function
}
\description{
Takes the dataframe produced by the interval functions and plots the p-values, consonance (confidence)
levels, and the interval estimates to produce a p-value function using the geom_point function in ggplot2.
}
\usage{
gg_consonance(x, measure, title, subtitle, caption, xaxis, yaxis, color)
}

\arguments{
  \item{x}{
The dataframe produced by one of the interval functions in which the intervals are stored.
}
  \item{measure}{
Indicates whether the object has a log transformation or is normal/default. The default setting is "default.
If the measure is set to "ratio", it will take logarithmically transformed values and convert them back to normal
values in the dataframe. This is typically a setting used for binary outcomes such as risk ratios, hazard ratios, and odds ratios.
}
  \item{title}{
A custom title for the graph. By default, it is set to "Consonance Function". In order to set a title, it must be in
quotes. For example, gg_consonance(x, title = "Custom Title").
}
  \item{subtitle}{
A custom subtitle for the graph. By default, it is set to "The function contains consonance/confidence intervals at
every level and the P-values." In order to set a subtitle, it must be in quotes. For example,
gg_consonance(x, subtitle = "Custom Subtitle").
}
  \item{caption}{
A custom caption for the graph. By default, it is set to "Produced with the concurve R package.". In order to set a caption,
it must be in quotes. For example, gg_consonance(x, caption = "Custom Caption").
}
  \item{xaxis}{
A custom x-axis title for the graph. By default, it is set to "Range of Values. In order to set a x-axis title,
it must be in quotes. For example, gg_consonance(x, xaxis = "Hazard Ratio").
}
  \item{yaxis}{
A custom y-axis title for the graph. By default, it is set to "Consonance Level". In order to set a y-axis title,
it must be in quotes. For example, gg_consonance(x, yxis = "Confidence Level").
}
  \item{color}{
Item that allows the user to choose the color of the points and the ribbons in the graph. By default, it is set to
color = "#239a98". The inputs must be in quotes. For example, gg_consonance(x, color = "#333333").
}
}

\value{
Plot with intervals at every consonance level graphed with their corresponding p-values and compatibility levels.
}
\references{
Poole C. Beyond the confidence interval. Am J Public Health. 1987;77(2):195-199.

Sullivan KM, Foster DA. Use of the confidence interval function. Epidemiology. 1990;1(1):39-42.

Rothman KJ, Greenland S, Lash TL, Others. Modern epidemiology. 2008.
}

\examples{
# Simulate random data

GroupA<-rnorm(50)
GroupB<-rnorm(50)

RandomData<-data.frame(GroupA, GroupB)
RandomModel<-lm(GroupA ~ GroupB, data=RandomData)

intervalsdf<-genintervals(RandomModel, "GroupB")

p<-gg_consonance(intervalsdf)
p
}
