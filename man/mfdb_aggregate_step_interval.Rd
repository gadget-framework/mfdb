\name{mfdb_aggregate_step_interval}
\alias{mfdb_step_interval}

\title{MareFrame DB intervals}
\description{
    Groups data into uniform intervals
}

\usage{
mfdb_step_interval(prefix, by, from = 0, to = NULL, open_ended = FALSE)
}

\arguments{
    \item{prefix}{
        (required) A character prefix to prepend to minimum to create list names
    }
    \item{by}{
        (required) Increment of the sequence. NB: Must be an integer
    }
    \item{from, to}{
        Start / end of the sequence. Defaults to 0 / infinity respectively.
    }
    \item{open_ended}{
        If true, there should be a final group representing everything above "to".
    }
}

\examples{
## Make groups of len0 (0--5), len5 (5--10), ... len45(45--50)
g1 <- mfdb_step_interval("len", 5, to = 50)

## Make groups of len0 (0--5), len5 (5--10), ... len45(45--50), len50(50--inf)
g2 <- mfdb_step_interval("len", 5, to = 50, open_ended = TRUE)
}
