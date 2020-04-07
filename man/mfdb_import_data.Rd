\name{mfdb_import_data}
\alias{mfdb_import_temperature}
\alias{mfdb_import_survey}
\alias{mfdb_import_survey_index}
\alias{mfdb_import_stomach}

\title{MareFrame Data Import functions}
\description{
    Functions to import data into MareFrame DB
}

\usage{
    mfdb_import_temperature(mdb, data_in)
    mfdb_import_survey(mdb, data_in, data_source = 'default_sample')
    mfdb_import_survey_index(mdb, data_in, data_source = 'default_index')
    mfdb_import_stomach(mdb, predator_data, prey_data, data_source = "default_stomach")
}
\arguments{
    \item{mdb}{
        Database connection created by \code{mfdb()}.
    }
    \item{data_in, predator_data, prey_data}{
        A \code{data.frame} of survey data to import, see details.
    }
    \item{data_source}{
        A name for this data, e.g. the filename it came from. Used so you can replace it later without disturbing other data.
    }
}
\details{
All functions will replace existing data in the case study with new data, unless you specify a \code{data_source},
in which case then only existing data with the same \code{data_source} will be replaced.

If you want to remove the data, import empty data.frames with the same \code{data_source}.

\code{mfdb_import_temperature} imports temperature time-series data for
areacells. The \code{data_in} should be a data.frame with the following columns:

\describe{
    \item{id}{A numeric ID for this areacell (will be combined with the case study number internally)}
    \item{year}{Required. Year each sample was taken, e.g. \code{c(2000,2001)}}
    \item{month}{Required. Month (1--12) each sample was taken, e.g. \code{c(1,12)}}
    \item{areacell}{Required. Areacell sample was taken within}
    \item{temperature}{The temperature at given location/time}
}

\code{mfdb_import_survey} imports institution surveys and commercial sampling for your case study. The
\code{data_in} should be a data.frame with the following columns:

\describe{
    \item{institute}{Optional. An institute name, see mfdb::institute for possible values}
    \item{gear}{Optional. Gear name, see mfdb::gear for possible values}
    \item{vessel}{Optional. Vessel defined previously with \code{mfdb_import_vessel_taxonomy(...)}}
    \item{tow}{Optional. Tow defined previously with \code{mfdb_import_tow_taxonomy(...)}}
    \item{sampling_type}{Optional. A sampling_type, see mfdb::sampling_type for possible values}
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
    \item{weight_total}{Optional, default \code{c(NA)}. Total weight of all samples, can be used with count = NA to represent an unknown number of samples}
    \item{count}{Optional, default \code{c(1)}. Number of samples this row represents (i.e. if the data is aggregated)}
}

\code{mfdb_import_survey_index} adds indicies that can be used as abundance information, for example.
Before using \code{mfdb_import_survey_index}, make sure that the \code{index_type} you intend to use
exists by using \link{mfdb_import_cs_taxonomy}. The \code{data_in} should be a data.frame with the
following columns:

\describe{
    \item{index_type}{Required. the name of the index data you are storing, e.g. 'acoustic'}
    \item{year}{Required. Year each sample was taken, e.g. \code{c(2000,2001)}}
    \item{month}{Required. Month (1--12) each sample was taken, e.g. \code{c(1,12)}}
    \item{areacell}{Required. Areacell sample was taken within}
    \item{value}{Value of the index at this point in space/time}
}

\code{mfdb_import_stomach} imports data on predators and prey. The predator and prey
data are stored separately, however they should be linked by the \code{stomach_name} column.
If a prey has a stomach name that doesn't match a predator, then an error will be returned.

The \code{predator_data} should be a data.frame with the following columns:
\describe{
    \item{stomach_name}{Required. An arbitary name that provides a link between the predator and prey tables}

    \item{institute}{Optional. An institute name, see mfdb::institute for possible values}
    \item{gear}{Optional. Gear name, see mfdb::gear for possible values}
    \item{vessel}{Optional. Vessel defined previously with \code{mfdb_import_vessel_taxonomy(mdb, ...)}}
    \item{tow}{Optional. Tow defined previously with \code{mfdb_import_tow_taxonomy(...)}}
    \item{sampling_type}{Optional. A sampling_type, see mfdb::sampling_type for possible values}

    \item{year}{Required. Year each sample was taken, e.g. \code{c(2000,2001)}}
    \item{month}{Required. Month (1--12) each sample was taken, e.g. \code{c(1,12)}}
    \item{areacell}{Required. Areacell sample was taken within}

    \item{species}{Optional, default \code{c(NA)}. Species of sample, see mfdb::species for possible values}
    \item{age}{Optional, default \code{c(NA)}. Age of sample, or mean age}
    \item{sex}{Optional, default \code{c(NA)}. Sex of sample, see mfdb::sex for possible values}
    \item{maturity_stage}{Optional, default \code{c(NA)}. Maturity stage of sample, see mfdb::maturity_stage for possible values}
    \item{stomach_state}{Optional, default \code{c(NA)}. Stomach state of sample, see mfdb::stomach_state for possible values}

    \item{length}{Optional, default \code{c(NA)}. Length of sample}
    \item{weight}{Optional, default \code{c(NA)}. Weight of sample}
}

The \code{prey_data} should be a data.frame with the following columns:
\describe{
    \item{stomach_name}{Required. The stomach name of the predator this was found in}
    \item{species}{Optional, default \code{c(NA)}. Species of sample, see mfdb::species for possible values}
    \item{digestion_stage}{Optional, default \code{c(NA)}. Stage of digestion of the sample, see mfdb::digestion_stage for possible values}

    \item{length}{Optional, default \code{c(NA)}. Length of sample / mean length of all samples}
    \item{weight}{Optional, default \code{c(NA)}. Weight of sample / mean weight of all samples}
    \item{weight_total}{Optional, default \code{c(NA)}. Total weight of all samples}
    \item{count}{Optional, default \code{c(NA)}. Number of samples this row represents (i.e. if the data is aggregated), count = NA represents an unknown number of samples}
}

}

\value{NULL}

\examples{\donttest{
mdb <- mfdb('examples-import-data')

# We need to set-up vocabularies first
mfdb_import_area(mdb, data.frame(
    id = c(1,2,3),
    name = c('35F1', '35F2', '35F3'),
    size = c(5)))
mfdb_import_vessel_taxonomy(mdb, data.frame(
    name = c('1.RSH', '2.COM'),
    stringsAsFactors = FALSE))
mfdb_import_sampling_type(mdb, data.frame(
    name = c("RES", "LND"),
    description = c("Research", "Landings"),
    stringsAsFactors = FALSE))

data_in <- read.csv(text = '
year,month,areacell,species,age,sex,length
1998,1,35F1,COD,3,M,140
1998,1,35F1,COD,3,M,150
1998,1,35F1,COD,3,F,150
')

data_in$institute <- 'MRI'
data_in$gear <- 'GIL'
data_in$vessel <- '1.RSH'
data_in$sampling_type <- 'RES'
mfdb_import_survey(mdb, data_in, data_source = 'cod-1998')
}}
