% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_stats.R
\name{get_stats}
\alias{get_stats}
\title{get_stats}
\usage{
get_stats(start, end)
}
\arguments{
\item{start}{The starting month (from present == 0) for which to generate the summary.}

\item{end}{The ending month (from present == 0) for which to generate the summary.}
}
\value{
\code{data.frame} with summary statistics
}
\description{
Returns a count of sites, datasets, publications and other
objects added to Neotoma during the requested time period.
}
\details{
This function returns summaries about the data holdings
within Neotoma using the existing Neotoma API's \code{summary} endpoint. This can
provide information about recent uploads (the number of new sites uploaded within the
last month, for example), or can be used to provide information about the overall number
of sites/datasets (using an arbitrarily high value for \code{end}).
}
\examples{
\donttest{
last_month <- get_stats(start = 0, end = 1)
}
}
\author{
Socorro Dominguez \email{dominguezvid@wisc.edu}

Simon Goring \email{goring@wisc.edu}
}
