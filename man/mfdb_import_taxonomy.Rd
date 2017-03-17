\name{mfdb_import_taxonomy}
\alias{mfdb_import_area}
\alias{mfdb_import_division}
\alias{mfdb_import_sampling_type}
\alias{mfdb_import_cs_taxonomy}
\alias{mfdb_import_gear_taxonomy}
\alias{mfdb_import_tow_taxonomy}
\alias{mfdb_import_vessel_taxonomy}
\alias{mfdb_import_species_taxonomy}
\alias{mfdb_empty_taxonomy}

\title{MareFrame Taxonomy import functions}
\description{
    Functions to import taxonomy data into MareFrame DB
}

\usage{
    mfdb_import_area(mdb, data_in)
    mfdb_import_division(mdb, data_in)
    mfdb_import_sampling_type(mdb, data_in)
    mfdb_import_tow_taxonomy(mdb,  data_in)
    mfdb_import_vessel_taxonomy(mdb,  data_in)

    mfdb_empty_taxonomy(mdb, taxonomy_name)

    mfdb_import_cs_taxonomy(mdb, taxonomy_name, data_in)
}

\arguments{
    \item{mdb}{
        Database connection created by \code{mfdb()}.
    }
    \item{taxonomy_name}{
        The name of the taxonomy to import, if there isn't a special function for it.
        See \code{mfdb:::mfdb_cs_taxonomy} for possible values.
    }
    \item{data_in}{
        A \code{data.frame} of data to import, see details.
    }
}

\details{
MFDB taxonomies define the values you can use when importing / querying for data.
They need to be populated with the values you need before data is imported. Most
taxonomies are pre-populated by the MFDB package, so you should use the predefined
values. Others however this does not make sense, so should be done separately. This
is what these functions do.

\code{mfdb_import_division} is a special case, which imports groupings of areacells
into divisions, if you haven't already done this with \code{mfdb_import_area} or your
divisions are too complicated to represent this way. The \code{data_in} should be a
list of areacell vectors, with division names. For example,
\code{list(divA = c('45G01', '45G02', '45G03'))}

Beyond this, all functions accept the following columns:

\describe{
    \item{id}{Optional. A numeric ID to use internally, defaults to 1..n}
    \item{name}{Required. A vector of short names to use in data, e.g. "SEA"}
    \item{t_group}{Optional. A vector of the that groups together a set of values}
}

Note that the database doesn't use your short names internally. This means you can
rename items by changing what \code{name} is set to. \code{t_group} allows taxonomy
values to be grouped together. For example, giving all vessels in a fleet the same
t_group you can then query the entire fleet as well as individually.

\code{mfdb_import_area} imports areacell information for your case study.
Beyond the above, you can also provide the following:

\describe{
    \item{size}{The size of the areacell}
    \item{division}{The name of the division this areacell is part of}
}

\code{mfdb_import_vessel_taxonomy} imports names of vessels into the
taxonomy table, so they can be used when importing samples. As well as the above, you can
also specify:

\describe{
    \item{full_name}{Optional. The full name of this vessel}
    \item{length}{Optional. The length of the vessel in meters}
    \item{power}{Optional. The vessel's engine power in KW}
    \item{tonnage}{Optional. The vessel's gross tonnage}
}

\code{mfdb_import_vessel_taxonomy} imports names of vessels into the
taxonomy table, so they can be used when importing samples. As well as the
above, you can also specify:

\describe{
    \item{latitude}{Optional.}
    \item{longitude}{Optional.}
    \item{depth}{Optional. Depth in meters}
    \item{length}{Optional. Length in meters}
}

\code{mfdb_import_sampling_type} imports sampling types so that you can then use
these against records in the \code{sample} table. You can also provide:

\describe{
    \item{description}{Optional. A vector of descriptive names, e.g. "sea sampling"}
}

\code{mfdb_empty_taxonomy} allows you to empty out a taxonomy of previous data.
The import functions insert or update values that already exist, based on the
numeric ID for these values. They do not delete anything, as it may be impossible
to remove rows without destroying existing data.

However, if e.g. you want to replace the species taxonomy with an entirely different
one you will need to flush it first, before you import any data. Use this function,
then \code{mfdb_import_species_taxonomy} to import the new taxonomy.

NB: This won't be possible if there is some data already using any of the terms. It
is best used before your database is populated.
}
