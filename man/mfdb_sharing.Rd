\name{mfdb_sharing}
\alias{mfdb_share_with}

\title{MareFrame DB sharing options}
\description{
    Alter database privileges
}

\usage{
mfdb_share_with(mdb, user_or_role, query = TRUE, import = FALSE)
}

\arguments{
    \item{mdb}{
        (required) A database connection created by \code{mfdb}()
    }
    \item{user_or_role}{
        (required) Another database user, or a role, or 'public' to share with all users
    }
    \item{query}{
        Should the user be able to query the current case study?
    }
    \item{import}{
        Should the user be able to import more data current case study?
    }
}

\details{
This allows you to share case study data between users. This is most useful when using
a shared database. Only the owner of the schema (i.e. the user that created it)
will be able to change table structure (i.e. upgrade MFDB versions).

By default nothing is shared between users.
}

\value{NULL}

\examples{
\dontrun{# NB: Requires a PostgreSQL installation, and creation of extra users

mdb <- mfdb('examples')

mfdb_share_with(mdb, 'gelda') # Allow DB user gelda to query the 'examples' case study data
}}
