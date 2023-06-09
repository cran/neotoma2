% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_contacts.R
\name{get_contacts}
\alias{get_contacts}
\title{Get contact information for Neotoma contributors}
\usage{
get_contacts(x = NA, ...)
}
\arguments{
\item{x}{integer A contact ID}

\item{...}{(\code{contactname})  A full or partial name for an individual
contributor to the database.
(\code{familyname})  The full or partial last name for an
individual contributor to the database.
(\code{status}) The current status of the contributor
(\code{active} or \code{retired})}
}
\value{
\code{contacts} object
}
\description{
Uses the Neotoma API to search and access
information about individuals who have contributed to
the data in the Neotoma Paleoecology Database
}
