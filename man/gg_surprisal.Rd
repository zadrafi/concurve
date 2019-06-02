\name{gg_surprisal}
\alias{gg_surprisal}

\title{
Plots the S-Value (Surprisal) Function
}
\description{
Takes the dataframe produced by the interval functions and plots the s-values, consonance (confidence)
levels, and the interval estimates to produce a s-value function using the geom_point function in ggplot2.
}
\usage{
gg_surprisal(x, measure, title, subtitle, caption, xaxis, yaxis, color)
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
A custom title for the graph. By default, it is set to "Surprisal Function". In order to set a title, it must be in
quotes. For example, gg_surprisal(x, title="Custom Title").
}
  \item{subtitle}{
A custom subtitle for the graph. By default, it is set to "The function contains consonance/confidence intervals at
every level and the S-values." In order to set a subtitle, it must be in quotes. For example,
gg_surprisal(x, subtitle="Custom Subtitle").
}
  \item{caption}{
A custom caption for the graph. By default, it is set to "Produced with the concurve R package.". In order to set a caption,
it must be in quotes. For example, gg_surprisal(x, caption="Custom Caption").
}
  \item{xaxis}{
A custom x-axis title for the graph. By default, it is set to "Range of Values. In order to set a x-axis title,
it must be in quotes. For example, gg_surprisal(x, xaxis="Hazard Ratio").
}
  \item{yaxis}{
A custom y-axis title for the graph. By default, it is set to "S-value (bits of information)". In order to set a y-axis title,
it must be in quotes. For example, gg_surprisal(x, yxis="Surpisal Values").
}
  \item{color}{
Item that allows the user to choose the color of the points and the ribbons in the graph. By default, it is set to
color = "#d46c5b". The inputs must be in quotes. For example, gg_surprisal(x, color = "#333333").
}
}

\value{
Plot with intervals at every consonance level graphed with their corresponding s-values.
}
\references{
Amrhein V, Trafimow D, Greenland S. Inferential Statistics as Descriptive Statistics:
There Is No Replication Crisis If We Don’t Expect Replication. Am Stat; 2018.

Greenland S. Valid P-values behave exactly as they should: Some misleading criticisms of P-values
and their resolution with S-values. Am Stat. 2018;18(136).

Greenland S. The unconditional information in P-values, and its refutational interpretation
via S-values. 2018.

Shannon CE. A Mathematical Theory of Communication. Bell System Technical Journal.
1948;27(3):379-423. doi:10.1002/j.1538-7305.1948.tb01338.x

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

s<-gg_surprisal(intervalsdf)
s
}
