% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_datasets.R
\name{get_datasets.site}
\alias{get_datasets.site}
\title{Get Dataset from a \code{site} object.}
\usage{
\method{get_datasets}{site}(x, ...)
}
\arguments{
\item{x}{An object of class \code{site}.}

\item{...}{additional arguments accepted by \code{get_datasets()}}
}
\value{
\code{sites} object with full metadata up to the \code{dataset} level
}
\description{
Get Dataset from a \code{site} object.
}
\examples{
\donttest{
random_sites <- get_sites(1)
allds <- get_datasets(random_sites, limit=3)
}
}
