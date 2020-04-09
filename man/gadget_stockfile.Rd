\name{gadget_stockfile}
\alias{gadget_stockfile_extremes}
\alias{gadget_stockfile_refweight}
\alias{gadget_stockfile_initialconditions}
\alias{gadget_stockfile_recruitment}

\title{Gadget stock files}
\description{
    Structures representing a GADGET stock file
}

\usage{
gadget_stockfile_extremes(stock_name, data)
gadget_stockfile_refweight(stock_name, data)
gadget_stockfile_initialconditions(stock_name, data)
gadget_stockfile_recruitment(stock_name, data)
}

\arguments{
    \item{stock_name}{
        A name, e.g. \code{cod.imm}, used as the name for the stockfile
    }
    \item{data}{
        A data.frame used to generate the data. See details.
    }
}

\details{
The columns required in the \code{data} varies depends on which function you
are using.

\code{gadget_stockfile_extremes} requires \code{age} and \code{length} columns
and populates \code{minlength}, \code{minage}, \code{maxlength}, \code{maxage}.
The values are obtained by the grouping used, rather than the
maximum values in the data. If you want the minimum and maximum from the data,
query with \code{length = NULL, age = NULL}, so the table contains "all"
and the grouping contains the actual minimum and maximum.

\code{gadget_stockfile_refweight} requires a \code{length} column and a
\code{mean} column representing mean weight for that length group. It populates
the \code{refweightfile} and \code{dl}.

\code{gadget_stockfile_initialconditions} requires \code{area}, \code{age},
\code{length}, \code{number} and \code{mean} (weight) columns. Populates
initialconditions \code{minlength}, \code{minage}, \code{maxlength}, \code{maxage},
\code{dl} and the \code{numberfile}. As before, the min/max values are populated
using the groupings you specify, not the min/max available data.

\code{gadget_stockfile_recruitment} requires \code{year}, \code{step}, \code{area},
\code{age}, \code{length}, \code{number} and \code{mean} (weight) columns. Populates
\code{doesrenew}, \code{minlength}, \code{maxlength}, \code{dl}, \code{numberfile}.
}

\value{
The return value is a \code{gadget_stockfile} object that can be written
to the filesystem with \code{gadget_dir_write}.
}

\examples{
\dontrun{  # NB: Requires a PostgreSQL installation, see README

mdb <- mfdb('examples')

imm_data <- mfdb_sample_meanweight(mdb, c('age', 'length'), list(
    age = NULL, # The age column will say 'all', but will know the min/max
    length = mfdb_step_interval('', 10, to = 100),
    species = 'COD'))

# Write both min/max and refweighfile into our gadget directory
component <- gadget_stockfile_extremes('cod.imm', imm_data[[1]])
component

component <- gadget_stockfile_refweight('cod.imm', imm_data[[1]])
component

gadget_dir_write(gadget_directory(tempfile()), component)
}}
