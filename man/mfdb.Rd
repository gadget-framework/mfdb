\name{mfdb}
\alias{mfdb}
\alias{mfdb_disconnect}

\title{MareFrame DB class}
\description{
    Create a class representing a connection to a MareFrame DB
}

\usage{
    mfdb(schema_name,
         db_params = list(),
         destroy_schema = FALSE,
         check_db_available = FALSE,
         save_temp_tables = FALSE)

    mfdb_disconnect(mdb)
}
\arguments{
    \item{schema_name}{
        This can be one of:

        \enumerate{
            \item Postgresql schema name
            \item "\code{:memory:}" to connect to a SQLite in-memory database
            \item A file path ending with "\code{.sqlite}" to connect to a SQLite file database
        }

        If connecting to a SQLite database, \var{db_params} should remain empty (
        \var{schema_name} will be used as a \code{dbname}).

        If connecting to a Postgres database, it can be used to
        store any number of case studies, by storing them in separate
        \emph{schemas}. This parameter defines the schema to connect to,
        and can contain any lower case characters or underscore.
    }
    \item{db_params}{
        Extra parameters to supply to \code{DBI::dbConnect}. By default it will
        search for a "mf" database locally, but you can override any of the parameters,
        in particular \code{host}, \code{dbname}, \code{user}, \code{password}.
        See \code{?RPostgres::Postgres} for more information.

        If \code{dbname} looks like a SQLite database filename, or "\code{:memory:}",
        then MFDB will use the \code{RSQLite} driver. Otherwise, \code{RPostgres} will
        be used.

        \var{db_params} can also be supplied by environment variable, for example
        if a \code{MFDB_DBNAME} environment variable is set then it will be used
        instead of any \code{dbname} supplied here.
    }
    \item{destroy_schema}{
        Optional boolean. If true, all mfdb tables will be destroyed when
        connecting. This allows you to start populating your case study from
        scratch if required. The function will return NULL, you need to call
        \code{mfdb} again to connect, at which point the mfdb tables will be
        recreated and you can populate with data again.
    }
    \item{save_temp_tables}{
        Optional boolean. If true, any temporary tables will be made permanent
        for later inspection.
    }
    \item{check_db_available}{
        Optionabl boolean. If true, we will only check that the datbase
        connection could be made. Mostly for use in examples.
    }
    \item{mdb}{
        Database connection created by \code{mfdb()}.
    }
}

\value{A 'mfdb' object representing the DB connection}

\examples{
# Connect to an in-memory SQLite database
mdb <- mfdb(':memory:')
mfdb_disconnect(mdb)

# Connect to a SQLite database file
mdb <- mfdb(tempfile(fileext = '.sqlite'))
mfdb_disconnect(mdb)

\dontrun{# NB: Requires a PostgreSQL installation, see README

# Connect to local DB, as the "examples" case study
mdb <- mfdb('examples')
mfdb_disconnect(mdb)

# Connect to remote server, will prompt for username/password
if (interactive()) {
    mdb <- mfdb('examples', db_params = list(host = "mfdb.rhi.hi.is"))
}

}
}
