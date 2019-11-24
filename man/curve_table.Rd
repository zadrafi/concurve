\name{curve_table}
\alias{curve_table}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Produces a Table of Relevant Statistics from the `concurve` functions
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
curve_table(data, levels, type = "data.frame")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{levels}{
%%     ~~Describe \code{levels} here~~
}
  \item{type}{
%%     ~~Describe \code{type} here~~
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

(z <- curve_table(intervalsdf, type = "data.frame"))
(z <- curve_table(intervalsdf, type = "tibble"))
(z <- curve_table(intervalsdf, type = "latex"))
(z <- curve_table(intervalsdf, type = "image"))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
