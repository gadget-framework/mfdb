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
    \item{case_study}{A case study name, see mfdb::case_study for possible values}
    \item{institute}{An institute name, see mfdb::institute for possible values}
    \item{gear}{A gear name, see mfdb::gear for possible values}
    \item{vessel}{A vessel name, see mfdb::vessel for possible values}
    \item{sampling_type}{A sampling_type name, see mfdb::sampling_type for possible values}
}

And \code{data_in} should contain the following columns:

\describe{
    \item{year}{Required. Year each sample was taken, e.g. \code{c(2000,2001)}}
    \item{month}{Required. Month (1--12) each sample was taken, e.g. \code{c(1,12)}}
    \item{areacell}{Required. Areacell sample was taken within}
    \item{species}{Optional, default \code{c(NA)}. Species of sample, see mfdb::species for possible values}
    \item{age}{Optional, default \code{c(NA)}. Age of sample, or mean age}
    \item{sex}{Optional, default \code{c(NA)}. Sex of sample, see mfdb::sex for possible values}
    \item{length}{Optional, default \code{c(NA)}. Length of sample / mean length of all samples}
    \item{weight}{Optional, default \code{c(NA)}. Weight of sample / mean weight of all samples}
    \item{count}{Optional, default \code{c(1)}. Number of samples this row represents (i.e. if the data is aggregated)}
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
