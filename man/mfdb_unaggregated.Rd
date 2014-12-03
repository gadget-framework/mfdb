\name{mfdb_unaggregated}
\alias{mfdb_unaggregated}
\alias{as.list.mfdb_unaggregated}

\title{MareFrame DB unaggregated data}
\description{
    Tell mfdb functions not to aggregate this column, just return all values.
}

\usage{
mfdb_unaggregated()
\method{as.list}{mfdb_unaggregated}(x, ...)
}

\arguments{
}

\examples{
## Currently we can't work out what to fill a group with,
## so returns placeholders
as.list(mfdb_unaggregated())
\dontrun{
$X
[1] "X"
}}
