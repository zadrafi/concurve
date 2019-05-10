\name{plotpint}
\alias{plotpint}

\title{Plot the P-Value Function
}
\description{
Takes the dataframe produced by the interval functions and plots the p-values, consonance (confidence)
levels, and the interval estimates to produce a p-value functionusing the geom_point function in ggplot2.
}
\usage{
plotpint(x)
}

\arguments{
  \item{x}{
The dataframe produced by one of the interval functions in which the intervals are stored.
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

p<-plotpint(intervalsdf)
p
}
