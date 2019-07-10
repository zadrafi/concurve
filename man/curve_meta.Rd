\name{curve_meta}
\alias{curve_meta}

\title{Computes consonance intervals for meta-analysis data
}
\description{
Computes thousands of consonance (confidence) intervals for the chosen parameter in the
meta-analysis done by the metafor package and places the interval limits for each interval level
into a data frame along with the corresponding p-values and s-values.
}
\usage{
curve_meta(x, measure = "default", steps = 10000)
}

\arguments{
  \item{x}{
Object where the meta-analysis parameters are stored, typically a list produced by 'metafor'
}
  \item{measure}{
Indicates whether the object has a log transformation or is normal/default. The default setting is "default.
If the measure is set to "ratio", it will take logarithmically transformed values and convert them back to normal values in the dataframe. This is typically a setting used for binary outcomes such as risk ratios, hazard ratios, and odds ratios.
}
  \item{steps}{
Indicates how many consonance intervals are to be calculated at various levels. For example, setting
this to 100 will produce 100 consonance intervals from 0 to 100. Setting this to 10000 will produce more
consonance levels. By default, it is set to 1000. Increasing the number substantially is not recommended
as it will take longer to produce all the intervals and store them into a dataframe.
}
}

\references{
Viechtbauer W. Conducting meta-analyses in R with the metafor package. J Stat Softw. 2010;36(3).
https://www.jstatsoft.org/article/view/v036i03/v36i03.pdf.

Poole C. Beyond the confidence interval. Am J Public Health. 1987;77(2):195-199.

Sullivan KM, Foster DA. Use of the confidence interval function. Epidemiology. 1990;1(1):39-42.

Rothman KJ, Greenland S, Lash TL, Others. Modern epidemiology. 2008.
}

\examples{

# Simulate random data for two groups in two studies

GroupAData <- runif(20, min = 0, max = 100)
GroupAMean <- round(mean(GroupAData), digits = 2)
GroupASD <- round(sd(GroupAData), digits = 2)

GroupBData <- runif(20, min = 0, max = 100)
GroupBMean <- round(mean(GroupBData), digits = 2)
GroupBSD <- round(sd(GroupBData), digits = 2)

GroupCData <- runif(20, min = 0, max = 100)
GroupCMean <- round(mean(GroupCData), digits = 2)
GroupCSD <- round(sd(GroupCData), digits = 2)

GroupDData <- runif(20, min = 0, max = 100)
GroupDMean <- round(mean(GroupDData), digits = 2)
GroupDSD <- round(sd(GroupDData), digits = 2)

# Combine the data

StudyName <- c("Study1", "Study2")
MeanTreatment <- c(GroupAMean, GroupCMean)
MeanControl <- c(GroupBMean, GroupDMean)
SDTreatment <- c(GroupASD, GroupCSD)
SDControl <- c(GroupBSD, GroupDSD)
NTreatment <- c(20, 20)
NControl <- c(20, 20)

metadf <- data.frame(
  StudyName, MeanTreatment, MeanControl,
  SDTreatment, SDControl, NTreatment, NControl
)

# Use metafor to calculate the standardized mean difference

library(metafor)

dat <- escalc(
  measure = "SMD", m1i = MeanTreatment, sd1i = SDTreatment, n1i = NTreatment,
  m2i = MeanControl, sd2i = SDControl, n2i = NControl, data = metadf
)

# Pool the data using a particular method. Here "FE" is the fixed-effects model

res <- rma(yi, vi, data = dat, slab = paste(StudyName, sep = ", "), method = "FE", digits = 2)

# Calculate the intervals using the metainterval function

metaf <- curve_meta(res)

tibble::tibble(metaf)
}
