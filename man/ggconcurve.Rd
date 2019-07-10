\name{ggconcurve}
\alias{ggconcurve}

\title{
Plots the P-Value (Consonance) and S-value (Surprisal) Function via ggplot2
}
\description{
Takes the dataframe produced by the interval functions and plots the p-values/s-values, consonance (confidence)
levels, and the interval estimates to produce a p-value/s-value function using ggplot2 graphics.
}
\usage{
ggconcurve(type, data, measure, nullvalue, position,
           title, subtitle, caption, xaxis, yaxis, color, fill)
}

\arguments{
  \item{type}{
Choose whether to plot a "consonance" function or a "surprisal" function. The default option is set to "consonance". The type must be set in quotes, for example ggconcurve(type = "surprisal") or ggconcurve(type = "consonance").
}
  \item{data}{
The dataframe produced by one of the interval functions in which the intervals are stored.
}
  \item{measure}{
Indicates whether the object has a log transformation or is normal/default. The default setting is "default". If the measure is set to "ratio", it will take logarithmically transformed values and convert them back to normal values in the dataframe. This is typically a setting used for binary outcomes and their measures such as risk ratios, hazard ratios, and odds ratios.
}
  \item{nullvalue}{
Indicates whether the null value for the measure should be plotted. By default, it is set to "absent", meaning it will not be plotted as a vertical line. Changing this to "present", will plot a vertical line at 0 when the measure is set to "default" and a vertical line at 1 when the measure is set to "ratio". For example, ggconcurve(type = "consonance", data = df, measure = "ratio", nullvalue = "present"). This feature is not yet available for surprisal functions.
}

  \item{position}{
Determines the orientation of the P-value (consonance) function By default, it is set to "pyramid", meaning the p-value function will stand right side up, like a pyramid. However, it can also be inverted via the option "inverted". This will also change the sequence of the y-axes to match the orientation. This can be set as such,
ggconcurve(type = "consonance", data = df, position = "inverted")
}

  \item{title}{
A custom title for the graph. By default, it is set to "Consonance Function". In order to set a title, it must be in quotes. For example, ggconcurve(x, title = "Custom Title").
}
  \item{subtitle}{
A custom subtitle for the graph. By default, it is set to "The function contains consonance/confidence intervals at every level and the P-values." In order to set a subtitle, it must be in quotes. For example, ggconcurve(x, subtitle = "Custom Subtitle").
}
  \item{caption}{
A custom caption for the graph. By default, it is set to "Produced with the concurve R package.". In order to set a caption, it must be in quotes. For example, ggconcurve(data, caption = "Custom Caption").
}
  \item{xaxis}{
A custom x-axis title for the graph. By default, it is set to "Range of Values. In order to set a x-axis title, it must be in quotes. For example, ggconcurve(data, xaxis = "Hazard Ratio").
}
  \item{yaxis}{
A custom y-axis title for the graph. By default, it is set to "Consonance Level". In order to set a y-axis title, it must be in quotes. For example, ggconcurve(data, yxis = "Confidence Level").
}
  \item{color}{
Item that allows the user to choose the color of the points and the ribbons in the graph. By default, it is set to color = "#555555". The inputs must be in quotes. For example, ggconcurve(data, color = "#333333").
}
  \item{fill}{
Item that allows the user to choose the color of the ribbons in the graph. By default, it is set to color = "#239a98". The inputs must be in quotes. For example, ggconcurve(data, fill = "#333333").
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

GroupA <- rnorm(50)
GroupB <- rnorm(50)

RandomData <- data.frame(GroupA, GroupB)
RandomModel <- lm(GroupA ~ GroupB, data = RandomData)

intervalsdf <- curve_gen(RandomModel, "GroupB")

p <- ggconcurve(type = "consonance", data = intervalsdf, nullvalue = "present")
p

r <- ggconcurve(type = "consonance", data = intervalsdf, position = "inverted")
r

s <- ggconcurve(type = "surprisal", data = intervalsdf)
s

}
