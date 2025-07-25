% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getids.R
\name{getids.collunit}
\alias{getids.collunit}
\title{Get object IDs from a single collectionunit.}
\usage{
\method{getids}{collunit}(x, order = TRUE)
}
\arguments{
\item{x}{A Neotoma2 \code{collunit} object.}

\item{order}{sort items by siteid, collunitid, datasetid}
}
\value{
\code{data.frame} containing \code{siteid}, \code{datasetid}, and \code{collunitid}
}
\description{
From a \code{collectionunit} object, return the
collectionunit and dataset ids.
}
\examples{
\donttest{
marion <- get_sites(sitename = "Marion Lake") 
collunitids <- getids(collunits(marion)[[1]])
}
}
