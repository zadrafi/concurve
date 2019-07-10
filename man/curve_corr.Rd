\name{curve_corr}
\alias{curve_corr}

\title{Computes consonance intervals for correlations
}
\description{
Computes consonance intervals to produce P- and S-value functions for correlational analyses
using the cor.test function in base R and places the interval limits for each interval level
into a data frame along with the corresponding p-values and s-values.

}
\usage{
curve_corr(x, y, alternative, method, steps = 10000)
}

\arguments{
  \item{x}{
A vector that contains the data for one of the variables that will be analyzed for correlational analysis.
}
  \item{y}{
A vector that contains the data for one of the variables that will be analyzed for correlational analysis.
}
  \item{alternative}{
Indicates the alternative hypothesis and must be one of "two.sided", "greater" or "less". You can specify just the initial letter. "greater" corresponds to positive association, "less" to negative association.
}
  \item{method}{
A character string indicating which correlation coefficient is to be used for the test. One of "pearson", "kendall", or "spearman", can be abbreviated.
}
  \item{steps}{
Indicates how many consonance intervals are to be calculated at various levels. For example, setting
this to 100 will produce 100 consonance intervals from 0 to 100. Setting this to 10000 will produce more consonance levels. By default, it is set to 1000. Increasing the number substantially is not recommended as it will take longer to produce all the intervals and store them into a dataframe.
}
}

\references{
Poole C. Beyond the confidence interval. Am J Public Health. 1987;77(2):195-199.

Sullivan KM, Foster DA. Use of the confidence interval function. Epidemiology. 1990;1(1):39-42.

Rothman KJ, Greenland S, Lash TL, Others. Modern epidemiology. 2008.
}

\examples{

GroupA <- rnorm(50)
GroupB <- rnorm(50)

joe <- curve_corr(x = GroupA, y = GroupB,
 alternative = "two.sided", method = "pearson")

tibble::tibble(joe)

}


