\name{mfdb-server}
\alias{mfdb-server}

\title{MareFrame DB server}
\description{
    Connecting to the remote MareFrame DB server
}

\details{
The University of Iceland host an internet-accessible Postgres database.
This can be used from your local computer in much the same way as a local
database, and case studies can be imported / exported locally.

Please send an an email to request access.
}

\section{Connecting to the MareFrame DB server}{
Connection is done with the \link{mfdb} function, as with a local database.
For example:-

\preformatted{
mdb <- mfdb('iceland', db_params = list(host = 'mfdb.rhi.hi.is'))
}

...and available case studies will be listed if \code{case_study_name} is not
provided.

\preformatted{
mdb <- mfdb(db_params = list(host = 'mfdb.rhi.hi.is'))
}

When connecting to a remote database, \code{username} and \code{password} are
essential. These could be specified in db_params, however, if not specified
you will be prompted to enter them.

Once connected, it can be used just like any other MFDB database.
}

\section{Import / Export to/from MFDB server}{
As well as issuing MFDB commands on the remote database, you can (depending on
permissions), import and export case study examples. This is useful to:

\enumerate{
    \item Get example data to get up and running
    \item Back up any data inserted locally
    \item Share data with others
}

To do this, you can use a combination of \code{pg_dump} and \code{pg_restore}
to copy a case study (or schema, in postgresql-speak):

\preformatted{
pg_dump --schema=example_iceland --format-c mf | pg_restore \
    -d mf --create --clean --host=mfdb.rhi.hi.is --username=(username)
}

If you want to copy from the MFDB server to your computer, put the \code{--host}
and \code{--username} parameters onto the \code{pg_dump} command.
}

\section{Sharing case studies with other users}{
By default, only you are able to share case study data. However the \link{mfdb_share_with}()
command allows you to let others read / modify your case study. For example:-

\preformatted{mfdb_share_with(mdb, 'anne')}

... will allow anne to read the case study you are currently connected to.
}


\section{Connecting directly to MFDB server}{
Finally, you can connect directly using psql if it proves useful. Note you
shouldn't need to do this for general use.

\preformatted{
psql "host=mfdb.rhi.hi.is user=(username) dbname=mf"
}
}
