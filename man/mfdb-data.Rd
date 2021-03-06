\name{mfdb-data}
\alias{mfdb-data}
\alias{case_study}
\alias{digestion_stage}
\alias{gear}
\alias{institute}
\alias{market_category}
\alias{maturity_stage}
\alias{sex}
\alias{species}
\alias{stomach_state}
\alias{vessel_type}

\title{MareFrame DB Datasets}
\description{
    Data sets representing the content of taxonomies used in the database.
}

\usage{
case_study
gear
institute
market_category
maturity_stage
sex
species
vessel_type
}

\details{
  All of these tables represent acceptible values for use when importing
  data. You can see the content of an individual taxonomy at the R command
  line, e.g. \kbd{mfdb::gear}

  Each of the datasets will have the following columns.
  \describe{
    \item{id}{A numeric ID to be used internally}
    \item{name}{
      An alphanumeric ID to be used when importing and reporting data.
    }
    \item{description}{
      Some text describing the option.
    }
    \item{t_group}{
      Groups together several items to query all in one go. e.g. for
      institutes you can query 'NOR' to get all institutes in Norway.
    }
  }

  The taxonomies are used in the following locations:

  \describe{
    \item{case_study}{
      Possible case studies, use when connecting with \link{mfdb}()
    }
    \item{gear, institute, vessel_type}{
      Used to describe the dataset being imported with \link{mfdb_import_survey}()
    }
    \item{sex, maturity_stage, species}{
      Used for individual sample points when using \link{mfdb_import_survey}()
    }
  }

}
