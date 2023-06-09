% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/taxa.R
\name{taxa,site-method}
\alias{taxa,site-method}
\title{Extract taxonomic data from a single site.}
\usage{
\S4method{taxa}{site}(object)
}
\arguments{
\item{object}{A \code{site} object.}
}
\value{
A \code{data.frame} reporting the taxa/data objects, units,
elements and other features within a set of records.
}
\description{
Extract taxonomic data from a single site.
}
\examples{
\donttest{
somesites <- get_sites(datasettype = "pollen", limit = 3) \%>\%
  get_downloads()
diatomtaxa <- taxa(somesites[[1]])
}
}
