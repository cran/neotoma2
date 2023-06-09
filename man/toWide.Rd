% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/toWide.R
\name{toWide}
\alias{toWide}
\title{toWide}
\usage{
toWide(
  x,
  variablenames = c(),
  ecologicalgroups = c(),
  elementtypes = c(),
  unit = c(),
  groupby = "age",
  operation = "prop"
)
}
\arguments{
\item{x}{dataframe object with samples}

\item{variablenames}{Optional vector to filter by specific variable names.}

\item{ecologicalgroups}{Vector stating the ecological groups to be filtered by, e.g "DIAT", "TRSH"}

\item{elementtypes}{Label of element type to filter by, e.g. "pollen", "valve"}

\item{unit}{Label stating which units to filter by, e.g. "NISP"}

\item{groupby}{Group by 'age' or 'depth'}

\item{operation}{label or vector of operations to be chosen from: 'prop', 'sum',
'presence'.}
}
\value{
wide \code{data.frame} obtained from long \code{samples} \code{data.frame}
}
\description{
Obtain a wide table with information regarding of samples grouped by
variablename and depth/age.
}
\examples{
\donttest{
fourcorners <- '{"type": "Polygon",
"coordinates": [[
[-109.36060497194846, 37.69552879956651],
[-107.813845732192, 37.69552879956651],
[-107.813845732192, 36.80303716260222],
[-109.36060497194846, 36.80303716260222], 
[-109.36060497194846, 37.69552879956651]
]]}'

# Download all vertebrate localities within a bounding box.
fc_sites <- neotoma2::get_sites(loc = fourcorners[1])
fc_ds <- neotoma2::get_datasets(fc_sites) \%>\% 
neotoma2::filter(datasettype=="vertebrate fauna")

fc_dl <- neotoma2::get_downloads(fc_ds)
fc_dl1 <- fc_dl[[1]]

fc_smp <- samples(fc_dl1)
toWide(fc_smp, ecologicalgroups=c('AVES', 'RODE'), 
elementtypes='bone/tooth', unit='present/absent')

}
}
