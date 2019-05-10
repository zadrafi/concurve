\name{plotsint}
\alias{plotsint}

\title{Plot the S-Value Function
}
\description{
Takes the dataframe produced by the interval functions and plots the s-values, consonance (confidence)
levels, and the interval estimates to produce a s-value function using the geom_point function in ggplot2.
}
\usage{
plotsint(x)
}

\arguments{
  \item{x}{
The dataframe produced by one of the interval functions in which the intervals are stored.
}
}

\value{
Plot with intervals at every consonance level graphed with their corresponding s-values
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

s<-plotsint(intervalsdf)
s
}
