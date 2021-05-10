\name{mfdb_bulk}
\alias{mfdb_cs_dump}
\alias{mfdb_cs_restore}

\title{MareFrame DB Dump / Restore}
\description{
    Dump / Restore entire case studies.
}

\usage{
mfdb_cs_dump(mdb, out_location)
mfdb_cs_restore(mdb, in_location)
}

\arguments{
    \item{mdb}{
        (required) A database connection created by \code{mfdb}()
    }
    \item{in_location, out_location}{
        (required) A filesystem directory or '.tar.gz' file to dump / restore database contents.
    }
}

\details{
Deprecated: These commands aren't strictly necessary any more. In most
situations it will be easier to use Postgres' \code{pg_dump} and \code{pg_restore}.
See \link{mfdb-package} for some examples of how to do it. These functions
don't offer much more functionality and much slower.

\code{mfdb_cs_dump} copies all data from the database/case-study that \code{mdb}
is connected to, and writes it out to files in \code{out_location}. If this
ends with 'tar.gz', then all files will be put into a tarball with the name

\code{mfdb_cs_restore} will remove any case-study data, and replaces it with the
content of \code{in_location}, a directory or tarball.
}

\value{NULL}

\examples{
# Copy data from one database to another, note they don't have to be the same type
mdb_out <- mfdb(tempfile(fileext = '.sqlite'))
mdb_in <- mfdb(tempfile(fileext = '.duckdb'))

dump_path <- tempfile(fileext='.tar.gz')
mfdb_cs_dump(mdb_out, dump_path)
mfdb_cs_restore(mdb_in, dump_path)

mfdb_disconnect(mdb_in)
mfdb_disconnect(mdb_out)
}
