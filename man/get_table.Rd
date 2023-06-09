% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_table.R
\name{get_table}
\alias{get_table}
\title{Get table record from Neotoma}
\usage{
get_table(x, limit = 25, offset = 0)
}
\arguments{
\item{x}{Table name (consult \url{https://open.neotomadb.org/dbschema/}
for a complete list of table names.}

\item{limit}{Default 25 records}

\item{offset}{Default 0.}
}
\value{
selected \code{table} values from the Database
}
\description{
Call Neotoma and return a table
(with limits & offsets for large tables)
}
\examples{
{
# Returns only the first 25 specimen records.
someSpec <- get_table('specimens')
}
}
