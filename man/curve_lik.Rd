\name{curve_lik}
\alias{curve_lik}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Computes Likelihood Intervals for Ratio Data
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
curve_lik(point, LL, UL, measure = "ratio", steps = 10000)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{point}{
%%     ~~Describe \code{point} here~~
}
  \item{LL}{
%%     ~~Describe \code{LL} here~~
}
  \item{UL}{
%%     ~~Describe \code{UL} here~~
}
  \item{measure}{
%%     ~~Describe \code{measure} here~~
}
  \item{steps}{
%%     ~~Describe \code{steps} here~~
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

lik1 <- curve_lik(point = 1.7, LL = 1.1, UL = 2.6, measure = "ratio", steps = 10000)

tibble::tibble(lik1)

lik2 <- curve_lik(point = 1.61, LL = 0.997, UL = 2.59, measure = "ratio", steps = 10000)

tibble::tibble(lik2)

lik1 <- curve_lik(point = 1.7, LL = 1.1, UL = 2.6, measure = "ratio", steps = 10000)
(ggconcurve(data = lik1, type = "likelihood", measure = "ratio", nullvalue = "present"))

lik2 <- curve_lik(point = 1.61, LL = 0.997, UL = 2.59, measure = "ratio", steps = 10000)
(ggconcurve(data = lik2, type = "deviance", measure = "ratio", nullvalue = "present"))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
