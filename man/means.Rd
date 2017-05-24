\name{means}
\alias{means}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Means
}
\description{
These functions calculate the arithmetic or geometric mean of a set of numbers.
}
\usage{
arithmeticmean(x)
geometricmean(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A vector of numbers
}
}
\details{
To calculate the arithmetic mean, each number in a vector is added up and then divided by the total number of entries in the vector. To calculate the geometric mean, each number is multiplied together and then the nth root is taken, where n is the number of entries in the vector.
}
\value{
Will return a number.
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
a <- c(-3,6,11,-2)
arithmeticmean(a)

b <- c(3,9,5,8)
geometricmean(b)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~arith }% use one of  RShowDoc("KEYWORDS")
