\name{meanintervals}
\alias{meanintervals}

\title{Produce Consonance Intervals for Mean Differences
}
\description{
Computes thousands of consonance (confidence) intervals for the chosen parameter in a statistical test
that compares means and places the interval limits for each interval level into a data frame along with the corresponding p-value and s-value.
}
\usage{
meanintervals(x, y, data, paired = F, method = "default",
replicates = 1000, steps = 10000)
}

\arguments{
  \item{x}{
Variable that contains the data for the first group being compared.
}
  \item{y}{
Variable that contains the data for the second group being compared.
}
  \item{data}{
Data frame from which the variables are being extracted from.
}
  \item{paired}{
Indicates whether the statistical test is a paired difference test. By default, it is set to "F",
which means the function will be an unpaired statistical test comparing two independent groups.
Inserting "paired" will change the test to a paired difference test.
}
  \item{method}{
By default this is turned off (set to "default"), but allows for bootstrapping if "boot" is inserted
into the function call.
}
  \item{replicates}{
Indicates how many bootstrap replicates are to be performed if bootstrapping is enabled as a method.
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

\examples{
# Simulate random data
GroupA<-runif(100, min=0, max=100)
GroupB<-runif(100, min=0, max=100)

RandomData<-data.frame(GroupA, GroupB)

bob<-meanintervals(GroupA, GroupB, RandomData)
}
