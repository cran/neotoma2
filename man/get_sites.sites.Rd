% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_sites.R
\name{get_sites.sites}
\alias{get_sites.sites}
\title{Get Site Information for Fossil Sites from a Set of Sites}
\usage{
\method{get_sites}{sites}(x, ...)
}
\arguments{
\item{x}{The numeric site ID from Neotoma}

\item{...}{accepted arguments if numeric all_data}
}
\value{
The function returns either a single item of class \code{"try-error"}
describing the reason for failure (either misdefined parameters or an error
from the Neotoma API), or a table of sites, with rows corresponding to the
number of individual sites returned by the Neotoma API.
Each "site" object contains 6 parameters that can be accessed as well:
siteid, sitename, location, altitude, description,
limited collection units information.
\itemize{
\item \code{loc} An \code{sf} object that describes site's location.
\item \code{collunits} limited information on collunits
}
}
\description{
Get Site Information for Fossil Sites from a Set of Sites
}
\examples{
\donttest{
## Find all sites using a set of prior sites:
char_sites <- get_sites(taxa = "charcoal")
pollen_coloc <- get_sites(char_sites, datasettype = "pollen")
char_coloc <- char_sites \%>\% filter(siteid \%in\% getids(pollen_coloc)$siteid)
pol_char <- c(pollen_coloc, char_coloc)
}
}
