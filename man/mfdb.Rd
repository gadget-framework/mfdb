\name{mfdb}
\alias{mfdb}
\alias{mfdb_disconnect}

\title{MareFrame DB class}
\description{
    Create a class representing a connection to a MareFrame DB
}

\usage{
    mfdb(db_params = list(),
         save_temp_tables = FALSE,
         create_schema = FALSE,
         defaultparams = list())

    mfdb_disconnect(mdb)
}
\arguments{
    \item{db_params}{
        Extra parameters to supply to a \code{DBI} connection object. By
        default it will search for a PostgreSQL "mf" database locally.
    }
    \item{save_temp_tables}{
        Optional boolean. If true, any temporary tables will be made permanent
        for later inspection.
    }
    \item{create_schema}{
        Optional boolean. If true, will attempt to create tables if they're
        missing.
    }
    \item{defaultparams}{
        An optional \code{list()} of default options to use for any query, see
        \code{\link{mfdb_meanlength}()} for details of available options.
    }
    \item{mdb}{
        Database connection created by \code{mfdb()}.
    }
}
\examples{\dontrun{
    # Connect to local DB, all calls return data for years 2000..2005
    mdb <- mfdb(defaultparams = list(year = 2000:2005))

    # Connect to "mf2014" on remote DB server
    mdb <- mfdb(db_params = list(host = "db.mareframe-fp7.org", dbname = "mf2014")

    # Stop it and tidy up
    mfdb_disconnect(mdb)
}}
