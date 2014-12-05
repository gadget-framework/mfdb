\name{mfdb-data}
\alias{mfdb-data}
\alias{gear}
\alias{case_study}
\alias{institute}
\alias{maturity_stage}
\alias{sex}
\alias{species}
\alias{vessel}

\title{MareFrame DB Datasets}
\description{
    Data sets representing the content of taxonomies used in the database.
}

\usage{
gear
case_study
institute
maturity_stage
sex
species
vessel
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
      Can contain multiple sections separated by dots to represent a tree,
      for example: "FARMYARD.COWS.DAISY".
    }
    \item{description}{
      Some text describing the option.
    }
  }

  The taxonomies are used in the following locations:

  \describe{
    \item{case_study}{
      Possible case studies, use when connecting with \link{mfdb}()
    }
    \item{gear, institute, vessel}{
      Used to describe the dataset being imported with \link{mfdb_import_survey}()
    }
    \item{sex, maturity_stage, species}{
      Used for individual sample points when using \link{mfdb_import_survey}()
    }
  }

}
