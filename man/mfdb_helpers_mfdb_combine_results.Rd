\name{mfdb_helpers_mfdb_combine_results}
\alias{mfdb_combine_results}

\title{MareFrame Query Utilities}

\description{
    Aggregate data from the database in a variety of ways
}

\usage{
mfdb_combine_results(...)
}

\arguments{
    \item{...}{Any number of data.frames produced by mfdb query functions with identical columns, e.g. \code{mfdb_sample_count}}
}

\value{
Given any number of data.frames from mfdb query functions with identical
columns, produces a combined data.frame, similar to \code{rbind} but
preserving the attributes required to produce aggregation files.
}
