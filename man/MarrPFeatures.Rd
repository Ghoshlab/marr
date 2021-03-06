% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllGenerics.R, R/AllslotAccessors.R
\docType{methods}
\name{MarrPFeatures}
\alias{MarrPFeatures}
\alias{MarrPFeatures,Marr-method}
\title{Generic function that returns the Marr P Features}
\usage{
MarrPFeatures(object)

\S4method{MarrPFeatures}{Marr}(object)
}
\arguments{
\item{object}{an object of class \code{Marr}.}
}
\value{
Value of \code{MarrPFeatures} argument passed 
to \code{Marr}
}
\description{
Given a Marr object, this function returns the
Marr P Features

Accessors for the 'MarrPFeatures'
slot of a Marr object.
}
\examples{
data <- matrix(rnorm(2400), nrow=200, ncol=12)
data_Marr <- Marr(object = data, pSamplepairs=0.75,
                  pFeatures=0.75, alpha=0.05)
MarrPFeatures(data_Marr)

}
