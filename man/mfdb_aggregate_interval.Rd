\name{mfdb_aggregate_interval}
\alias{mfdb_interval}

\title{MareFrame DB intervals}
\description{
    Represent a uniform or non-uniform interval.
}

\usage{
mfdb_interval(prefix, vect, open_ended = FALSE)
}

\arguments{
    \item{prefix}{
        (required) A character prefix to prepend to minimum to create list names
    }
    \item{vect}{
        (required) A vector representing the minimum for each group, and the maximum
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
## Make groups of len40 (40--60), len60 (60--80)
g1 <- mfdb_interval("len", c(40, 60, 80))

## Use seq to make life easier
g2 <- mfdb_interval("len", seq(40, 80, by = 20))

## Create groups len40: [40, 60), len60: [60, inf) (but [60, 80) in the GADGET model)
g1 <- mfdb_interval("len", c(40, 60, 80), open_ended = c("upper"))
}
