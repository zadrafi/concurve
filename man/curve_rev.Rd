\name{curve_rev}
\alias{curve_rev}

\title{
Reverse engineer consonance and surprisal functions from confidence limits and point estimates
}
\description{
Using the confidence limits and point estimates from a dataset, one can use these estimates to
compute thousands of consonance intervals and graph the intervals to form a consonance and
surprisal function.
}
\usage{
curve_rev(point, LL, UL, measure = "default")
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

\examples{

# From a real published study. Point estimate of the result was hazard ratio of 1.61 and
# lower bound of the interval is 0.997 while upper bound of the interval is 2.59.

df <- curve_rev(point = 1.61, LL = 0.997, UL = 2.59, measure = "ratio")

tibble::tibble(df)

}


