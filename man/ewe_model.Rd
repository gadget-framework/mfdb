\name{ewe_model}
\alias{mfdb_rpath_params}

\title{MareFrame DB Rpath interface}

\description{
    Transform the results of MFDB queries for use in an Rpath model
}

\usage{
mfdb_rpath_params(area_data,
    survey_data,
    catch_data,
    consumption_data,
    create_rpath_params = stop("Set create_rpath_params = Rpath::create.rpath.params"),
    living_groups = character(0),
    detritus_groups = c("Detritus"))
}

\arguments{
    \item{area_data}{Results of an \code{mfdb_area_size} query, aggregating the whole area}
    \item{survey_data}{Results of an \code{mfdb_sample_totalweight} query, normally for one year, aggregated by the model's functional groups}
    \item{catch_data}{Results of an \code{mfdb_sample_totalweight} query, normally for one year, aggregated by the model's functional groups and 'vessel'}
    \item{consumption_data}{Results of an \code{mfdb_stomach_preyweightratio} query, aggregated by functional groups}
    \item{living_groups}{Additional Rpath groups of "Living" type}
    \item{detritus_groups}{Additional Rpath groups of "Detritus" type}
    \item{create_rpath_params}{
      RPath isn't currently in a public repository, so to avoid depending on it you need to
      give mfdb_rpath_params the Rpath function, i.e. \code{Rpath::create.rpath.params}.
    }
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
   Returns an Rpath.params object populated with the provided data.
}

\examples{# See demo/example-ewe.R for a full-length example}
