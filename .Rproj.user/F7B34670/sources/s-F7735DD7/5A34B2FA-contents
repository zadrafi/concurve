\name{meanintervals}
\alias{meanintervals}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{meanintervals
%%  ~~function to do ... ~~
}
\description{
Computes thousands of compatibility (confidence) intervals for the chosen parameter in a statistical test that compares means and places the interval limits for each interval level into a data frame along with the corresponding p-value and s-value.
}
\usage{
meanintervals(x, y, data, paired = F, method = "default",
replicates = 1000, steps = 10000)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Variable that contains the data for the first group being compared.
}
  \item{y}{
Variable that contains the data for the second group being compared.
}
  \item{data}{
Dataframe from which the variables are being extracted from.
}
  \item{paired}{
Indicates whether the statistical test is a paired difference test. By default, it is set to "F", which means the function will be an unpaired statistical test comparing two independent groups. Inserting "paired" will change the test to a paired difference test.
}
  \item{method}{
By default this is turned off (set to "default"), but allows for bootstrapping if "boot" is inserted into the function call.
}
  \item{replicates}{
Indicates how many bootstrap replicates are to be performed if bootstrapping is enabled as a method.
}
  \item{steps}{
Indicates how many compatibility intervals are to be calculated at various levels. For example, setting this to 100 will produce 100 compatibility intervals from 0 to 100. Setting this to 1,000 will produce more compatibility levels. By default, it is set to 10,000. Increasing the number is not recommended as it will take longer to produce all the intervals and store them into a dataframe.
}
}

\references{
Poole C. Beyond the confidence interval. Am J Public Health. 1987;77(2):195-199.

Sullivan KM, Foster DA. Use of the confidence interval function. Epidemiology. 1990;1(1):39-42.

Rothman KJ, Greenland S, Lash TL, Others. Modern epidemiology. 2008.
}

\examples{
# Simulate random data
GroupA<-runif(100, min=0, max=100)
GroupB<-runif(100, min=0, max=100)

RandomData<-data.frame(GroupA, GroupB)

bob<-meanintervals(GroupA, GroupB, RandomData)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
