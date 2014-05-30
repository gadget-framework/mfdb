\name{mfdb_group}
\alias{mfdb_group}
\alias{mfdb_group_numbered}
\alias{mfdb_bootstrap_group}
\alias{mfdb_interval_group}

\title{MareFrame DB groups}
\description{
    Represent a grouping of data
}

\usage {
    mfdb_group(...)

    mfdb_group_numbered(prefix, ...)

    mfdb_bootstrap_group(count, group)

    \method{mfdb}(db_connection = dbConnect(\dots), defaultparams = list(...))
}
\arguments {
    \item{db_connection}{
        An optional \code{DBI} connection object. If none given, will presume a
        PostgreSQL MareFrame database is available and running locally.
    }
    \item{defaultparams}{
        An optional \code{list()} of default options to use for any query, see
        \code{\link{mfdb_meanlength}()} for details of available options.
    }
}
