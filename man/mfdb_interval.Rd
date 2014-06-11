\name{mfdb_interval}
\alias{mfdb_interval}
\alias{as.list.mfdb_interval}

\title{MareFrame DB intervals}
\description{
    Represent a uniform or non-uniform interval.
}

\usage{
mfdb_interval(prefix, vect)
\method{as.list}{mfdb_interval}(x, ...)
}

\arguments{
    \item{prefix}{
        (required) A character prefix to prepend to minimum to create list names
    }
    \item{vect}{
        (required) A vector representing the minimum for each group, and the maximum
    }
    \item{x}{
        The \code{mfdb_interval} to convert to a list for use in \code{gadget_file}
    }
    \item{...}{
        Ignored.
    }
}

\examples{
## Make groups of len40 (40--60), len60 (60--80)
g1 <- mfdb_interval("len", c(40, 60, 80))

## Use seq to make life easier
g2 <- mfdb_interval("len", seq(40, 80, by = 20))

## Convert to list representation
as.list(g2)
\dontrun{
$len40
[1] 40 60

$len60
[1] 60 80
}}
