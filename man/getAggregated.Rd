% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lapis.R
\name{getAggregated}
\alias{getAggregated}
\title{Get aggregated data from LAPIS}
\usage{
getAggregated(
  session,
  fields = NULL,
  orderBy = NULL,
  limit = NULL,
  offset = NULL,
  ...
)
}
\arguments{
\item{session}{The current session}

\item{fields}{The fields to aggregate by. Valid field names can be found in \code{session$metadata$name}. If NULL, only the total count is returned.}

\item{orderBy}{The fields to order by; must be either "count" or included in \code{fields}}

\item{limit}{Maximum number of sequences to include}

\item{offset}{Number of sequences to skip}

\item{...}{Filters. Valid filter keys can be found with \code{getFilters(session)}}
}
\value{
Aggregated data
}
\description{
Makes a request to the /sample/aggregated endpoint of LAPIS and parses the response
}
\examples{
agg_data <- getAggregated(session, fields = "dateMonth", country = "Switzerland")
}
