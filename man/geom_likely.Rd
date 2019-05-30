\name{geom_likely}
\alias{geom_likely}
\title{
Plots the Log-likelihood (Support) Function
}
\description{
Takes the dataframe produced by the interval functions and plots the log-likelihood function using
ggplot2 .
}
\usage{
geom_likely(data, x, y, title = "Support Function", subtitle = "A log-likelihood function to assess the probability of the data.", caption = "Produced with the concurve R package.", xaxis = "Theta", yaxis = "Log-likelihood", fill = "#239a98")
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
quotes. For example, geom_likely(x, title="Custom Title").
}
  \item{subtitle}{
A custom subtitle for the graph. By default, it is set to "A log-likelihood function to assess the probability of the data."
In order to set a subtitle, it must be in quotes. For example, geom_likely(x, subtitle="Custom Subtitle").
}
  \item{caption}{
A custom caption for the graph. By default, it is set to "Produced with the concurve R package.". In order to set a caption,
it must be in quotes. For example, geom_likely(x, caption="Custom Caption").
}
  \item{xaxis}{
A custom x-axis title for the graph. By default, it is set to "Theta". In order to set a x-axis title,
it must be in quotes. For example, geom_likely(x, xaxis="Hazard Ratio").
}
  \item{yaxis}{
A custom y-axis title for the graph. By default, it is set to "Log-likelihood". In order to set a y-axis title,
it must be in quotes. For example, geom_likely(x, yxis="Log-likelihood").
}
  \item{fill}{
Item that allows the user to choose the color of the ribbon in the graph. By default, it is set to
color = "#239a98". The inputs must be in quotes. For example, geom_likely(x, color = "#333333").
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}

\references{
%% ~put references to the literature/web site here ~
}

\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (data, x, y, title = "Support Function", subtitle = "A log-likelihood function to assess the probability of the data.",
    caption = "Produced with the concurve R package.", xaxis = "Theta",
    yaxis = "Log-likelihood", fill = "#239a98")
{
    if (is.data.frame(data) != TRUE) {
        stop("Error: 'data' must be a data frame from 'concurve'")
    }
    if (ncol(data) != 4) {
        stop("Error: 'data' must be a data frame from 'concurve'")
    }
    ggplot(data = data, aes(x = x, y = y)) + geom_line() + geom_ribbon(aes(x = x,
        ymin = min(y), ymax = y), fill = fill, alpha = 0.3) +
        labs(title = title, subtitle = subtitle, caption = caption,
            x = xaxis, y = yaxis) + theme_light() + theme(axis.title.x = element_text(size = 13),
        axis.title.y = element_text(size = 13)) + scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
        theme(text = element_text(size = 15)) + theme(plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 12), plot.caption = element_text(size = 8))
  }
}

