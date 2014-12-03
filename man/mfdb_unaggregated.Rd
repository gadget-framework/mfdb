\name{mfdb_unaggregated}
\alias{mfdb_unaggregated}
\alias{as.list.mfdb_unaggregated}

\title{MareFrame DB unaggregated data}
\description{
    Tell mfdb functions not to aggregate this column, just return all values.
}

\usage{
mfdb_unaggregated(omitNA = FALSE)
\method{as.list}{mfdb_unaggregated}(x, ...)
}

\arguments{
    \item{omitNA}{Skip over rows where column is NA}
    \item{x}{
        The \code{mfdb_unaggregated} to convert to a list for use in \code{gadget_file}
    }
    \item{...}{
        Ignored.
    }
}

\examples{
## Currently we can't work out what to fill a group with,
## so returns placeholders
as.list(mfdb_unaggregated())
\dontrun{
$X
[1] "X"
}}
