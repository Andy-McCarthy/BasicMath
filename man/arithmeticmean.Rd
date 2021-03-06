% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/means.R
\name{arithmeticmean}
\alias{arithmeticmean}
\title{Calculating the Arithmetic Mean}
\usage{
arithmeticmean(input, na.rm = FALSE)
}
\arguments{
\item{input}{a vector of numbers}

\item{na.rm}{true or false for whether to remove NA's before performing the calculation}
}
\value{
A number
}
\description{
Calculates the arithmetic mean of a set of numbers. The function adds up
the numbers in the vector and divides by the number of elements in the vector.
}
\examples{
a <- c(-4,9,1,-6)
arithmeticmean(a)

b <- c(-4,NA,NA,9,1,-6)
arithmeticmean(b)

d <- c(-4,NA,NA,9,1,-6)
arithmeticmean(d,na.rm=T)

}
