\name{mfdb}
\alias{mfdb}
\alias{mfdb_disconnect}

\title{MareFrame DB class}
\description{
    Create a class representing a connection to a MareFrame DB
}

\usage{
    mfdb(case_study_name,
         db_params = list(),
         destroy_schema = FALSE,
         save_temp_tables = FALSE)

    mfdb_disconnect(mdb)
}
\arguments{
    \item{case_study_name}{
        The name of the case study you are working with, should be a valid
        name from \code{mfdb::case_study}.
    }
    \item{db_params}{
        Extra parameters to supply to \code{DBI::dbConnect}. By default it will
        search for a "mf" database locally, but you can override any of the parameters,
        in particular \code{host}, \code{dbname}, \code{user}, \code{password}.
        See \code{methods?RPostgreSQL::dbConnect} for more information.
    }
    \item{destroy_schema}{
        Optional boolean. If true, all mfdb tables will be dropped, allowing
        you to recreate your schema. Returns NULL, you need to call \code{mfdb}
        to connect and rebuild schema.
    }
    \item{save_temp_tables}{
        Optional boolean. If true, any temporary tables will be made permanent
        for later inspection.
    }
    \item{mdb}{
        Database connection created by \code{mfdb()}.
    }
}
\examples{\dontrun{
    # Connect to local DB, as the Iceland case study
    mdb <- mfdb('Iceland')

    # Connect to "mf2014" on remote DB server
    mdb <- mfdb('Iceland', db_params = list(host = "db.mareframe-fp7.org", dbname = "mf2014")

    # Stop it and tidy up
    mfdb_disconnect(mdb)

    # Destroy everything, since schema was out of date.
    mfdb('Iceland', destroy_schema = TRUE)
}}
