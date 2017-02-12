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
        If TRUE / c('upper'), the last group will include everything >= the last value in vect.
        If c('lower'), the first group will include everything < the first value in vect.
        If c('upper', 'lower'), both the above occur.
    }
}

\examples{
## Make groups of len40 (40--60), len60 (60--80)
g1 <- mfdb_interval("len", c(40, 60, 80))

## Use seq to make life easier
g2 <- mfdb_interval("len", seq(40, 80, by = 20))
}
