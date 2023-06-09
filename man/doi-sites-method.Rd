% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/site-methods.R
\name{doi,sites-method}
\alias{doi,sites-method}
\title{Obtain dataset DOIs from records.}
\usage{
\S4method{doi}{sites}(x)
}
\arguments{
\item{x}{a Neotoma2 \code{site} object}
}
\value{
\code{data.frame} object with DOIs information.
}
\description{
Given complete dataset objects in Neotoma (must have used
\code{get_datasets()} or \code{get_downloads()}), return the dataset
DOI for the record.
}
\examples{
{
ds <- get_datasets(1)
doi(ds)
}
}
