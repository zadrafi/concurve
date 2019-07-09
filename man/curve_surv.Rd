\name{curve_surv}
\alias{curve_surv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Produce Consonance Intervals for Survival Data
}
\description{
Computes thousands of consonance (confidence) intervals for the chosen parameter in the
Cox model computed by the 'survival' package and places the interval limits for each interval level
into a data frame along with the corresponding p-value and s-value.
}
\usage{
curve_surv(data, x, steps = 10000)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
Object where the Cox model is stored, typically a list produced by the 'survival' package.
}
  \item{x}{
Predictor of interest within the survival model for which the consonance intervals should be computed.
}
  \item{steps}{
Indicates how many consonance intervals are to be calculated at various levels. For example, setting
this to 100 will produce 100 consonance intervals from 0 to 100. Setting this to 10000 will produce more
consonance levels. By default, it is set to 1000. Increasing the number substantially is not recommended
as it will take longer to produce all the intervals and store them into a dataframe.
}
}

\references{
Poole C. Beyond the confidence interval. Am J Public Health. 1987;77(2):195-199.

Sullivan KM, Foster DA. Use of the confidence interval function. Epidemiology. 1990;1(1):39-42.

Rothman KJ, Greenland S, Lash TL, Others. Modern epidemiology. 2008.
}
