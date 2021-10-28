\name{mfdb_dplyr}
\alias{mfdb_dplyr_table}
\alias{mfdb_dplyr_survey_index}
\alias{mfdb_dplyr_division}
\alias{mfdb_dplyr_sample}
\alias{mfdb_dplyr_predator}
\alias{mfdb_dplyr_prey}

\title{MareFrame DB dplyr interface}

\description{
    Use mfdb tables with dplyr
}

\usage{
mfdb_dplyr_table(mdb, table_name, include_cols = all_cols)
mfdb_dplyr_survey_index(mdb, include_cols = all_cols)
mfdb_dplyr_division(mdb, include_cols = all_cols)
mfdb_dplyr_sample(mdb, include_cols = all_cols)
mfdb_dplyr_predator(mdb, include_cols = all_cols)
mfdb_dplyr_prey(mdb, include_cols = all_cols)
}

\arguments{
    \item{mdb}{An object created by \code{mfdb}()}
    \item{table_name}{A table name to query in}
    \item{include_cols}{Any additonal columns to include in output, see details.}
}

\details{
Warning: Whilst these might be handy for exploration, there is no guarantee
that code using these will continue to work from one version of MFDB to the
next.

There is one function for each measurement table. By default every possible
taxonomy column is included. However this is somewhat inefficient if you do
not require the data, in which case specify the columns requred with
\code{include_cols}. See \code{mfdb::mfdb_taxonomy_tables} for possible values.

To query taxonomy tables, use \code{mfdb_dplyr_table}, which works for any supplied table name.
See \code{mfdb::mfdb_taxonomy_tables} for possible values for \var{table_name}.
}

\value{
A dplyr table object, for you to do as you please.
}

\examples{
mdb <- mfdb(tempfile(fileext = '.duckdb'))

# Include as many columns as possible
mfdb_dplyr_sample(mdb)

# Only include 'data_source' and 'species' columns, as well as measurements
mfdb_dplyr_sample(mdb, c('data_source', 'species'))

# Query the sampling_type table
mfdb_dplyr_table(mdb, 'sampling_type')

mfdb_disconnect(mdb)
}
