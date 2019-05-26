\name{reveng}
\alias{reveng}

\title{
Reverse Engineer Consonance and Surprisal Functions From Confidence Limits and Point Estimates
}
\description{
Using the confidence limits and point estimates from a dataset, one can use these estimates to
compute thousands of consonance intervals and graph the intervals to form a consonance and
surprisal function.
}
\usage{
reveng(point, LL, UL, measure = "default")
}

\arguments{
  \item{point}{
The point estimate from an analysis. Ex: 1.20
}
  \item{LL}{
The lower confidence limit from an analysis Ex: 1.0
}
  \item{UL}{
The upper confidence limit from an analysis Ex: 1.4
}
  \item{measure}{
The type of data being used. If they involve mean differences,
then the "default" option should be used, which is also the default setting.
If the data are ratios, then the "ratio" option should be used.
}
}

\references{
Poole C. Beyond the confidence interval. Am J Public Health. 1987;77(2):195-199.

Sullivan KM, Foster DA. Use of the confidence interval function. Epidemiology. 1990;1(1):39-42.

Rothman KJ, Greenland S, Lash TL, Others. Modern epidemiology. 2008.
}

