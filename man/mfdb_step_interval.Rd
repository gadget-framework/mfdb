\name{mfdb_step_interval}
\alias{mfdb_step_interval}
\alias{as.list.mfdb_step_interval}

\title{MareFrame DB intervals}
\description{
    Represent a uniform or non-uniform interval.
}

\usage{
mfdb_step_interval(prefix, by, from = 0, to = NULL, open_ended = FALSE)
\method{as.list}{mfdb_step_interval}(x, ...)
}

\arguments{
    \item{prefix}{
        (required) A character prefix to prepend to minimum to create list names
    }
    \item{by}{
        (required) Increment of the sequence.
    }
    \item{from, to}{
        Start / end of the sequence. Defaults to 0 / infinity respectively.
    }
    \item{open_ended}{
        If true, there should be a final group representing everything above "to".
    }
    \item{x}{
        An \code{mfdb_step_interval} object.
    }
    \item{...}{
        Nothing should be specfied (required by generic signature).
    }
}

\examples{
## Make groups of len0 (0--5), len5 (5--10), ... len45(45--50)
g1 <- mfdb_step_interval("len", 5, to = 50)
}
