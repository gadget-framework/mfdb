\name{mfdb_import}
\alias{mfdb_import_survey}

\title{MareFrame Import functions}
\description{
    Functions to import data into MareFrame DB
}

\usage{
    mfdb_import_survey(mdb, data_in, ...)
}
\arguments{
    \item{mdb}{
        Database connection created by \code{mfdb()}.
    }
    \item{data_in}{
        Survey data to import
    }
    \item{...}{
        Metadata about the survey data, see details.
    }
}
\details{
    TODO:
}
\examples{\dontrun{
    mfdb_import_survey(mdb, read.csv("data/cod-1998.csv"), data_source = "cod-1998")
}}
