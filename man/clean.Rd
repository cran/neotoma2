% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clean.R
\name{clean}
\alias{clean}
\title{clean Neotoma objects to remove duplicates and empty objects.}
\usage{
clean(x = NA, verbose = TRUE, ...)
}
\arguments{
\item{x}{sites, datasets, collunits that may have duplicates.}

\item{verbose}{parameter to prints out progress bar}

\item{...}{Additional parameters associated with the call.}
}
\value{
clean \code{neotoma} objects without duplicates after concatenation
}
\description{
Function that removes duplicate objects such as sites,
datasets, or collection units. When we pull in a large number of objects,
or overlapping searches, we can run into a problem where we have multiple
instances of the same site, but with different datasets. This function
attempts to gather all objects together:
\itemize{
\item Before: \{site: 1, dataset: 1\}, \{site: 1, dataset: 2\}
\item After: \{site: 1, dataset: [1, 2]\}
So the site is gathered, and the datasets are now part of an array of datasets.
}
}
\examples{
\donttest{
clean_sites <- get_sites(sitename = "L\%", limit = 20)
more_sites <- get_sites(sitename = "La\%", limit = 20)
long_set <- c(clean_sites, more_sites)
length(long_set)
# By removing duplicates we get a smaller object.
length(clean(long_set))
# We can do the same thing with collection units:
clean_cols <- get_sites(sitename = "L\%", limit = 20) \%>\%
  collunits()
more_cols <- get_sites(sitename = "La\%", limit = 20) \%>\%
  collunits()
long_set <- c(clean_cols, more_cols)
length(long_set)
# By removing duplicates we get a smaller object.
length(clean(long_set))
# And datasets:
clean_ds <- get_sites(sitename = "L\%", limit = 20) \%>\%
  get_downloads() \%>\% datasets()
more_ds <- get_sites(sitename = "La\%", limit = 20) \%>\%
  get_downloads() \%>\% datasets()
long_set <- c(clean_ds, more_ds)
length(long_set)
# By removing duplicates we get a smaller object.
length(clean(long_set))
}
}
\author{
Simon Goring \email{goring@wisc.edu}
}
