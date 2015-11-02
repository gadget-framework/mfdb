\name{mfdb_aggregate_unaggregated}
\alias{mfdb_unaggregated}

\title{MareFrame DB unaggregated data}
\description{
    Tell mfdb functions not to aggregate this column, just return all values.
}

\usage{
mfdb_unaggregated(omitNA = FALSE)
}

\arguments{
    \item{omitNA}{Skip over rows where column is NA}
}

\examples{
## Currently we can't work out what to fill a group with,
## so returns placeholders
}
