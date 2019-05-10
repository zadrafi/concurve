\name{genintervals}
\alias{genintervals}

\title{Produce Consonance Intervals for Linear Models

}
\description{
Computes thousands of consonance (confidence) intervals for the chosen parameter in the
selected model(ANOVA, ANCOVA, regression, logistic regression) and places the interval limits
for each interval level into a data frame along with the corresponding p-value and s-value.
}
\usage{
genintervals(model, var, method = "default", replicates = 1000, steps = 10000)
}

\arguments{
  \item{model}{
The statistical model of interest(ANOVA, regression, logistic regression) is to be indicated here.
}
  \item{var}{
The variable of interest from the model (coefficients, intercept) for which the intervals are to be produced.
}
  \item{method}{
Chooses the method to be used to calculate the consonance intervals. There are currently four
methods: "default", "wald", "lm", and "boot". The "default" method uses the profile likelihood method to
compute intervals and can be used for models created by the 'lm' function. The "wald" method is typically
what most people are familiar with when computing intervals based on the calculated standard error.
The "lm" method allows this function to be used for specific scenarios like logistic regression and
the 'glm' function. The "boot" method allows for bootstrapping at certain levels.
}
  \item{replicates}{
Indicates how many bootstrap replicates are to be performed if bootstrapping is enabled as a method.
}
  \item{steps}{
Indicates how many consonance intervals are to be calculated at various levels. For example, setting
this to 100 will produce 100 compatibility intervals from 0 to 100. Setting this to 10000 will produce more consonance levels. By default, it is set to 1000. Increasing the number substantially is not recommended as it will take longer to produce all the intervals and store them into a dataframe.
}
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

rob<-glm(GroupA ~ GroupB, data=RandomData)
bob<-genintervals(rob, "GroupB", method="lm")

}
