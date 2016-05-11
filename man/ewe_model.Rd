\name{ewe_model}
\alias{ewe_stanza_group}
\alias{ewe_stanzas}
\alias{ewe_model}
\alias{ewe_diet}
\alias{ewe_pedigree}

\title{MareFrame DB EwE/Rpath interface}

\description{
    Transform the results of MFDB queries for use in an EwE model
}

\usage{
ewe_stanza_group(survey_data)
ewe_stanzas(survey_data)
ewe_model(area_data, survey_data, catch_data = NULL)
ewe_diet(consumption_data)
ewe_pedigree(survey_data, catch_data = NULL)
}

\arguments{
    \item{area_data}{Results of an \code{mfdb_area_size} query, aggregating the whole area}
    \item{survey_data}{Results of an \code{mfdb_sample_totalweight} query, normally for one year, aggregated by the model's functional groups}
    \item{catch_data}{Results of an \code{mfdb_sample_totalweight} query, normally for one year, aggregated by the model's functional groups and 'vessel'}
    \item{consumption_data}{Results of an \code{mfdb_stomach_preyweightratio} query, aggregated by functional groups}
}

\details{
EwE requires stanzas and groups of stanzas, these are made up using the first
and any other groupings in MFDB. For example, if \code{survey_data} was made
with a query like \code{mfdb_sample_totalweight(mdb, c('species', 'age'), ...)},
then the species will make up the generated stanza_groups, and age will make up
the stanzas within those groups.

\code{catch_data} requires data that is also aggregated by vessel, this will be
ignored for the purposes of deciding the stanza/stanza_group.

\code{consumption_data} treats prey groupings seperate to predator groupings,
and all will be added to the diet matrix. 

See \link{mfdb_sample_totalweight} for more information on how groupings can be used in
queries.
}

\value{
    All return data.frame objects matching the EwE output, apart from diet which
    returns the diet matrix as a matrix.
}

\examples{# See demo/example-ewe.R for a full-length example}
