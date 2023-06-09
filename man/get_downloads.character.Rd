% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_downloads.R
\name{get_downloads.character}
\alias{get_downloads.character}
\title{get_downloads JSON}
\usage{
\method{get_downloads}{character}(x, verbose = TRUE, ...)
}
\arguments{
\item{x}{sites object}

\item{verbose}{Should text be printed during the download process?}

\item{...}{arguments in ellipse form}
}
\value{
The function returns either a single item of class
\code{"try-error"} describing the reason for failure
(either misdefined parameters or an error from the Neotoma API),
or a table of sites, with rows corresponding to the number of
individual sites and datasets returned by the Neotoma API.
}
\description{
get_downloads JSON
}
