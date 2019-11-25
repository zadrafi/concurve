\name{plot_compare}
\alias{plot_compare}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Compares Different Functions on the Same Plot
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot_compare(data1, data2, type = "consonance", measure = "default", nullvalue = "absent", position = "pyramid", title = "Interval Function", subtitle = "The function displays intervals at every level.", xaxis = "Θ Range of Values", yaxis = "P-value", color = "#555555", fill1 = "#239a98", fill2 = "#d46c5b")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data1}{
%%     ~~Describe \code{data1} here~~
}
  \item{data2}{
%%     ~~Describe \code{data2} here~~
}
  \item{type}{
%%     ~~Describe \code{type} here~~
}
  \item{measure}{
%%     ~~Describe \code{measure} here~~
}
  \item{nullvalue}{
%%     ~~Describe \code{nullvalue} here~~
}
  \item{position}{
%%     ~~Describe \code{position} here~~
}
  \item{title}{
%%     ~~Describe \code{title} here~~
}
  \item{subtitle}{
%%     ~~Describe \code{subtitle} here~~
}
  \item{xaxis}{
%%     ~~Describe \code{xaxis} here~~
}
  \item{yaxis}{
%%     ~~Describe \code{yaxis} here~~
}
  \item{color}{
%%     ~~Describe \code{color} here~~
}
  \item{fill1}{
%%     ~~Describe \code{fill1} here~~
}
  \item{fill2}{
%%     ~~Describe \code{fill2} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
library(concurve)

GroupA <- rnorm(500)
GroupB <- rnorm(500)

RandomData <- data.frame(GroupA, GroupB)


intervalsdf <- curve_mean(GroupA, GroupB,
                          data = RandomData, method = "default"
)


(function1 <- ggconcurve(type = "consonance", intervalsdf, xaxis = expression(Theta~"= Range of Values")))

GroupA2 <- rnorm(500)
GroupB2 <- rnorm(500)

RandomData2 <- data.frame(GroupA2, GroupB2)

model <- lm(GroupA2 ~ GroupB2, data = RandomData2)

randomframe <- curve_gen(model, "GroupB2")

(function2 <- ggconcurve(type = "consonance", randomframe, levels = c(0.25, 0.50, 0.75, 0.85, 0.90, 0.95), nullvalue = "present", subtitle = "The function displays compatibility intervals at every level."))

(function3 <- ggconcurve(type = "surprisal", randomframe, levels = c(0.25, 0.50, 0.75, 0.85, 0.90, 0.95), nullvalue = "present", subtitle = "The function displays compatibility intervals at every surprisal level."))

(plot_compare(data1 = intervalsdf, data2 = randomframe, type = "consonance",  measure = "default", nullvalue = "present", title = "Comparison of Consonance Functions", subtitle = "The functions display compatibility intervals at every level."))

(plot_compare(data1 = intervalsdf, data2 = randomframe, type = "surprisal",  measure = "default", nullvalue = "present", title = "Comparison of Surprisal Functions", subtitle = "The functions display compatibility intervals at every surprisal level."))

lik1 <- curve_lik(point = 1.7, LL = 1.1, UL = 2.6, measure = "ratio", steps = 10000)
(ggconcurve(data = lik1, type = "likelihood", measure = "ratio", nullvalue = "present",  subtitle = "The function displays likelihood intervals at every level."))

lik2 <- curve_lik(point = 1.61, LL = 0.997, UL = 2.59, measure = "ratio", steps = 10000)
(ggconcurve(data = lik2, type = "deviance", measure = "ratio", nullvalue = "present",
subtitle = "The function displays deviance intervals at every level."))


(plot_compare(data1 = lik1, data2 = lik2, type = "likelihood",  measure = "ratio", nullvalue = "present", subtitle = "Comparison of Likelihood Functions", xaxis = expression(Theta~"= Hazard Ratio / Odds Ratio")))

(plot_compare(data1 = lik1, data2 = lik2, type = "deviance",  measure = "ratio", nullvalue = "present", subtitle = "Comparison of Deviance Functions",  xaxis = expression(Theta~"= Hazard Ratio / Odds Ratio")))


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
