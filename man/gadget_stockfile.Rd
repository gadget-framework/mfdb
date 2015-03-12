\name{gadget_stockfile}
\alias{gadget_stockfile_extremes}
\alias{gadget_stockfile_refweight}

\title{Gadget stock files}
\description{
    Structures representing a GADGET stock file
}

\usage{
gadget_stockfile_extremes(stock_name, data)
gadget_stockfile_refweight(stock_name, data)
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

\code{gadget_stockfile_extremes} requires \code{age} and \code{length} columns.
However, the data does not have to be aggregated by the \code{mfdb_*} function.
Just an "all" column is fine, as the min/max values will also be fetched.

\code{gadget_stockfile_refweight} requires a \code{length} column and a
\code{mean} column representing mean weight for that length group.
}

\value{
The return value is a \code{gadget_stockfile} object that can be written
to the filesystem with \code{gadget_dir_write}.
}

\examples{
# Write min/max values and refweight file 
\dontrun{
gd <- gadget_directory('./out')
imm_data <- mfdb_sample_meanweight(mdb, c('age', 'length'), list(
    age = NULL, # The age column will say 'all', but will know the min/max
    length = mfdb_step_interval('', 10, to = 100),
    species = 'COD',
    maturity_stage = '1',
    null = NULL))

# Write both min/max and refweighfile into our gadget directory
gadget_dir_write(gd, gadget_stockfile_extremes('cod.imm', imm_data[[1]]))
gadget_dir_write(gd, gadget_stockfile_refweight('cod.imm', imm_data[[1]]))
}
}
