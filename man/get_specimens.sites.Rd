% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_specimens.R
\name{get_specimens.sites}
\alias{get_specimens.sites}
\title{Get Specimen Sites}
\usage{
\method{get_specimens}{sites}(x, ...)
}
\arguments{
\item{x}{Use a single number to extract site information}

\item{...}{Other possible parameters such as datasetid}
}
\value{
The function returns a specimens list
}
\description{
Get Specimen Sites
}
\examples{
\donttest{
# To find specimen with ID 7:
my_site <- get_sites(13296)
# To find specimens in `my_site`
my_specimens <- get_specimens(my_site)
}
}
