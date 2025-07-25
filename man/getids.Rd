% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getids.R
\name{getids}
\alias{getids}
\title{Get object IDs}
\usage{
getids(x, order = TRUE)
}
\arguments{
\item{x}{A Neotoma2 \code{sites} object.}

\item{order}{sort items by siteid, collunitid, datasetid}
}
\value{
\code{data.frame} containing \code{siteid}, \code{datasetid}, and \code{collunitid}
}
\description{
This function parses a site object, from \code{site} to
\code{dataset} level and returns a \code{data.frame} that contains the
site, collectionunit and dataset IDs for each element within the site.
}
