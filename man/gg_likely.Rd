\name{gg_likely}
\alias{gg_likely}
\title{
Plots the Log-likelihood (Support) Function
}
\description{
Takes the dataframe produced by the interval functions and plots the log-likelihood function using
ggplot2 .
}
\usage{
gg_likely(data, x, y, title = "Support Function", subtitle = "A log-likelihood function to assess the probability of the data.", caption = "Produced with the concurve R package.", xaxis = "Theta", yaxis = "Log-likelihood", fill = "#239a98")
}
\arguments{
  \item{data}{
The dataframe produced by the interval function that computed the likelihoods.
}
  \item{x}{
The vector in the dataframe that corresponds to theta.
}
  \item{y}{
The log-likelihoods stored in the dataframe.
}
  \item{title}{
A custom title for the graph. By default, it is set to "Support Function". In order to set a title, it must be in
quotes. For example, gg_likely(x, title="Custom Title").
}
  \item{subtitle}{
A custom subtitle for the graph. By default, it is set to "A log-likelihood function to assess the probability of the data."
In order to set a subtitle, it must be in quotes. For example, gg_likely(x, subtitle="Custom Subtitle").
}
  \item{caption}{
A custom caption for the graph. By default, it is set to "Produced with the concurve R package.". In order to set a caption,
it must be in quotes. For example, gg_likely(x, caption="Custom Caption").
}
  \item{xaxis}{
A custom x-axis title for the graph. By default, it is set to "Theta". In order to set a x-axis title,
it must be in quotes. For example, gg_likely(x, xaxis="Hazard Ratio").
}
  \item{yaxis}{
A custom y-axis title for the graph. By default, it is set to "Log-likelihood". In order to set a y-axis title,
it must be in quotes. For example, gg_likely(x, yxis="Log-likelihood").
}
  \item{fill}{
Item that allows the user to choose the color of the ribbon in the graph. By default, it is set to
color = "#239a98". The inputs must be in quotes. For example, gg_likely(x, color = "#333333").
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}

\references{
%% ~put references to the literature/web site here ~
}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

}

