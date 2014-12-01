\name{mfdb_import}
\alias{mfdb_import_survey}
\alias{mfdb_import_area}
\alias{mfdb_import_division}
\alias{mfdb_import_temperature}
\alias{mfdb_import_sampling_type}

\title{MareFrame Import functions}
\description{
    Functions to import data into MareFrame DB
}

\usage{
    mfdb_import_survey(mdb, data_in, ...)
    mfdb_import_area(mdb, data_in)
    mfdb_import_division(mdb, data_in)
    mfdb_import_temperature(mdb, data_in)
    mfdb_import_sampling_type(mdb, data_in)
}
\arguments{
    \item{mdb}{
        Database connection created by \code{mfdb()}.
    }
    \item{data_in}{
        A \code{data.frame} of survey data to import, see details.
    }
    \item{...}{
        Metadata about the survey data, see details.
    }
}
\details{
For \code{mfdb_import_survey}, the metadata can contain:

\describe{
    \item{data_source}{A name for this data, e.g. it's filename. Used so you can replace it later without disturbing other data}
    \item{institute}{Optional. An institute name, see mfdb::institute for possible values}
    \item{gear}{Optional. Gear name, see mfdb::gear for possible values}
    \item{vessel}{Optional. Vessel name, see mfdb::vessel for possible values}
    \item{sampling_type}{Optional. A sampling_type, see mfdb::sampling_type for possible values}
}

And \code{data_in} should be a data.frame with the following columns:

\describe{
    \item{year}{Required. Year each sample was taken, e.g. \code{c(2000,2001)}}
    \item{month}{Required. Month (1--12) each sample was taken, e.g. \code{c(1,12)}}
    \item{areacell}{Required. Areacell sample was taken within}
    \item{species}{Optional, default \code{c(NA)}. Species of sample, see mfdb::species for possible values}
    \item{age}{Optional, default \code{c(NA)}. Age of sample, or mean age}
    \item{sex}{Optional, default \code{c(NA)}. Sex of sample, see mfdb::sex for possible values}
    \item{length}{Optional, default \code{c(NA)}. Length of sample / mean length of all samples}
    \item{length_var}{Optional, default \code{c(NA)}. Sample variance, if data is already aggregated}
    \item{length_min}{Optional, default \code{c(NA)}. Minimum theoretical length, if data is already aggregated}
    \item{weight}{Optional, default \code{c(NA)}. Weight of sample / mean weight of all samples}
    \item{weight_var}{Optional, default \code{c(NA)}. Sample variance, if data is already aggregated}
    \item{count}{Optional, default \code{c(1)}. Number of samples this row represents (i.e. if the data is aggregated)}
}

\code{mfdb_import_area} imports areacell information for your case study. The
\code{data_in} should be a data.frame with the following columns:

\describe{
    \item{id}{A numeric ID for this areacell (will be combined with the case study number internally)}
    \item{name}{The name of the areacell, e.g. '45G01'}
    \item{size}{The size of the areacell}
}

\code{mfdb_import_division} imports groupings of areacells into divisions. The
\code{data_in} should be a list of areacell vectors, with division names.
For example, \code{list(divA = c('45G01', '45G02', '45G03'))}

\code{mfdb_import_temperature} imports temperature time-series data for
areacells. The \code{data_in} should be a data.frame with the following columns:

\describe{
    \item{id}{A numeric ID for this areacell (will be combined with the case study number internally)}
    \item{year}{Required. Year each sample was taken, e.g. \code{c(2000,2001)}}
    \item{month}{Required. Month (1--12) each sample was taken, e.g. \code{c(1,12)}}
    \item{areacell}{Required. Areacell sample was taken within}
    \item{temperature}{The temperature at given location/time}
}

\code{mfdb_import_sampling_type} imports names of sampling types for this case
study. The \code{data_in} should be a data.frame with the following columns:

\describe{
    \item{id}{A numeric ID for this sampling type}
    \item{name}{Required. A vector of short names to use in data, e.g. "SEA"}
    \item{description}{Optional. A vector of descriptive names, e.g. "sea sampling"}
}

}
\examples{\dontrun{
# data/cod-1998.csv contains:-
# year,month,areacell,species,age,sex,length
# 1998,1,35F1,COD,3,M,140
# 1998,1,35F1,COD,3,M,150
# 1998,1,35F1,COD,3,F,150

mfdb_import_survey(mdb,
    data_source = 'cod-1998',
    case_study = 'Iceland',
    institute = 'MRI',
    gear = 'GIL',
    vessel = '2.RSH',
    sampling_type = 'HAR',
    read.csv("data/cod-1998.csv"))
}}
