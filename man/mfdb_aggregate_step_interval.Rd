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
        If TRUE / c('upper'), the last group will ignore it's upper bound and include any value.
        If c('lower'), the first group will ignore it's lower bound include everything < the first value in vect.
        If c('upper', 'lower'), both the above occur.
        This is useful when creating plus groups for GADGET, as GADGET will still be presented a bounded group, but will contain all remaining data.
    }
}

\value{
An \code{mfdb_aggregate} object that can then be used in querying functions such as
\code{mfdb_sample_count}
}

\examples{
## Make groups of len0 (0--5), len5 (5--10), ... len45(45--50)
g1 <- mfdb_step_interval("len", 5, to = 50)

## Make groups of len0 (0--5), len5 (5--10), ... len45(45--50), len50(50--inf)
g2 <- mfdb_step_interval("len", 5, to = 50, open_ended = TRUE)
}
