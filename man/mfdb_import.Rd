\name{mfdb_import}
\alias{mfdb_import_identifiers}

\title{MareFrame Import functions}
\description{
    Functions to import data into MareFrame DB
}

\usage{
    mfdb_import_identifiers(mfdb, table_name, new_data)
}
\arguments{
    \item{mfdb}{
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
}
\examples{\dontrun{
    # species.csv contains:-
    # name,description
    # COD,Cod
    # HAD,Haddock
    mfdb_import_identifiers(mfdb, "species", read.csv('species.csv'))
}}
