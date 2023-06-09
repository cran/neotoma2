% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_datasets.R
\name{get_datasets.numeric}
\alias{get_datasets.numeric}
\title{Get Dataset Numeric}
\usage{
\method{get_datasets}{numeric}(x, ...)
}
\arguments{
\item{x}{Use a single number to extract site information}

\item{...}{Additional parameters to get_datasets}
}
\value{
\code{sites} object with full metadata up to the \code{dataset} level
}
\description{
Get Dataset Numeric
}
\examples{
\donttest{
allds <- get_datasets(1:3)
}
}
