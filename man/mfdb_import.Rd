\name{mfdb_import}
\alias{mfdb_import_taxonomy}
\alias{mfdb_import_survey}

\title{MareFrame Import functions}
\description{
    Functions to import data into MareFrame DB
}

\usage{
    mfdb_import_taxonomy(mdb, table_name, new_data)

    mfdb_import_survey(mdb, data_in, ...)
}
\arguments{
    \item{mdb}{
        Database connection created by \code{mfdb()}.
    }
    \item{table_name}{
        The data to import. Can be one of "institute", "fleet", "gear", "vessel",
        "market_category", "sampling_type", "species".
    }
    \item{new_data}{
        New data. A CSV file with a "name" and "description" column. "name" is
        the expected identifier in other data, "description" is a longer description
        if required.
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
    # species.csv contains:-
    # name,description
    # COD,Cod
    # HAD,Haddock
    mfdb_import_taxonomy(mdb, "species", read.csv('species.csv'))

    mfdb_import_survey(mdb, read.csv("data/cod-1998.csv"), data_source = "cod-1998")
}}
