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
        (required) Another database user, or a role
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
the \link{mfdb-server}. Regardless, only the owner of the schema (i.e. the user that
created it) will be able to change table structure (i.e. upgrade MFDB versions).

By default nothing is shared between users.
}

\examples{\dontrun{
mdb <- mfdb('iceland')
mfdb_share_with(mdb, 'paul') # Allow DB user paul to query the iceland case study data
mfdb_share_with(mdb, 'mary') # Allow DB user mary to add data to the iceland case study
}}
