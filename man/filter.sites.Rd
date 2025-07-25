% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filter.R
\name{filter.sites}
\alias{filter.sites}
\title{Apply a filter for Neotoma sites objects.}
\usage{
\method{filter}{sites}(x, ...)
}
\arguments{
\item{x}{A sites object.}

\item{...}{arguments to filter by.}
}
\value{
filtered \code{sites} object
}
\description{
The \code{filter} function takes a \code{sites} object
and allows a user to filter on a number of properties. Since a sites object
is a nested object (it contains collection units, datasets, samples, etc.)
the degree to which filtering occurs depends on the amount of data contained
within the sites object. Filtering parameters include:
\itemize{
\item \code{siteid} A numeric site identifier from the Neotoma Database.
\item \code{sitename} The character string sitename.
\item \code{lat} A numeric latitude value.
\item \code{long} A numeric longitude value.
\item \code{altitude} The elevation of the site. Note that some sites do not
include elevation information. For these an NA value appears, and they
would be removed when using an elevation filter.
\item \code{datasetid} A numeric datasetid from Neotoma.
\item \code{database} A character string naming the constituent database
from which the dataset is drawn.
\item \code{datasettype} A character string representing one of the many
dataset types within Neotoma.
\item \code{age_range_old} A dataset-level parameter indicating the oldest
date covered by the dataset chronology.
\item \code{age_range_young} A dataset-level parameter indicating the youngest
date covered by the dataset chronology.
\item \code{notes} Free-form dataset notes provided by the dataset PI(s),
analysts or data stewards.
\item \code{collectionunitid} A numeric collection unit identifier from
Neotoma.
\item \code{handle} A character string identifying the collection unit. These
are often shorter form names (originally a default 8 character length).
\item \code{collectionunitname} A character string identifying the collection
unit name.
\item \code{colldate} The date on which the collection unit was sampled. Many
of these are empty.
\item \code{location} A free-form character string indicating the location of
the collection unit within the site.
\item \code{waterdepth} A numeric depth at which the core was obtained.
\item \code{collunittype} A character string for the collection unit type.
\item \code{collectiondevice} A fixed vocabulary term for the collection
device.
\item \code{depositionalenvironment} A fixed vocabulary name for the
depositional environment.
}
}
\examples{
\donttest{
# Download 100 sites, but only keep the sites that are close to sea level.
some_sites <- get_sites(sitename = "Lake\%", limit = 3)
site_subset <- some_sites \%>\% filter(altitude < 100)
# Download 100 sites, get all associated datasets, but keep only
# sites/datasets that are of datasettype "pollen":
sites <- get_sites(limit = 1) \%>\%
  get_datasets()
pollen_subset <- sites \%>\% filter(datasettype == "pollen")
}
}
