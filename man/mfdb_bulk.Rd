\name{mfdb_bulk}
\alias{mfdb_cs_dump}
\alias{mfdb_cs_restore}

\title{MareFrame DB Dump / Restore}
\description{
    Dump / Restore entire case studies.
}

\usage{
mfdb_cs_dump(mdb, directory)
mfdb_cs_restore(mdb, directory)
}

\arguments{
    \item{mdb}{
        (required) A database connection created by \code{mfdb}()
    }
    \item{directory}{
        (required) A filesystem directory to dump / restore database contents.
    }
}

\details{
\code{mfdb_cs_dump} copies all data from the database/case-study that \code{mdb}
is connected to, and writes it out to files in \code{directory}.

\code{mfdb_cs_restore} will remove any case-study data, and replaces it with the
content of \code{directory}.
}

\examples{\dontrun{
# Copy case study data from one case study to another
mdb_out <- mfdb('Iceland')
mdb_in <- mfdb('Test')

mfdb_cs_dump(mdb_out, '/tmp/cs_dump')
mfdb_cs_restore(mdb_in, '/tmp/cs_dump')
}}
