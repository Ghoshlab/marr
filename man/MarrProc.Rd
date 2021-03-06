% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MarrProc.R
\docType{methods}
\name{MarrProc}
\alias{MarrProc}
\title{MarrProc}
\usage{
MarrProc(object, alpha = 0.05)
}
\arguments{
\item{object}{an object which is a \code{matrix} or
\code{data.frame} with features (e.g. metabolites or genes) on
the rows and samples as the columns. Alternatively,
a user can provide a \code{SummarizedExperiment} object
and the \code{assay(object)} will be used as input
for the Marr procedure.}

\item{alpha}{(Optional) level of significance to control
the False Discovery Rate (FDR).
Default is 0.05.}
}
\value{
A list of percent reproducible statistics including
\item{samplepairs}{the distribution of
percent reproducible  features (column-wise) per sample pair}
\item{features}{the distribution of
percent reproducible  sample pairs (row-wise) per feature}
}
\description{
This function is a helper function that
computes distributions of reproducible sample pairs per feature
and reproducible features per sample pair for the function
\code{Marr}.
}
\examples{
data <- matrix(rnorm(2400), nrow=200, ncol=12)
data_MarrProc <- MarrProc(object=data, alpha = 0.05)

}
