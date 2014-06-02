\name{mfdb}
\alias{mfdb}

\title{MareFrame DB class}
\description{
    Create a class representing a connection to a MareFrame DB
}

\usage{
    mfdb(db_connection = NULL, defaultparams = list())
}
\arguments{
    \item{db_connection}{
        An optional \code{DBI} connection object. If none given, will presume a
        PostgreSQL MareFrame database is available and running locally.
    }
    \item{defaultparams}{
        An optional \code{list()} of default options to use for any query, see
        \code{\link{mfdb_meanlength}()} for details of available options.
    }
}
\examples{\dontrun{
    mfdb(db_connection = dbConnect(\dots), defaultparams = list(years = 2000:2005))
}}