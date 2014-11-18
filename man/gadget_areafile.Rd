\name{gadget_areafile}
\alias{gadget_areafile}

\title{Gadget area files}
\description{
    Structures representing a GADGET area file
}

\usage{
gadget_areafile(size, temperature)
}

\arguments{
    \item{size}{
        data.frame as produced by \code{mfdb_area_size}
    }
    \item{temperature}{
        data.frame as produced by \code{mfdb_area_temperature}
    }
}

\details{
Once formed, you can then use \code{gadget_dir_write} to write this out to a
GADGET areafile.
}

\examples{
# Any example could be added to a file with the following:-
\dontrun{
gd <- gadget_directory('./out')
agg_data <- gadget_areafile(
    mfdb_area_size(...),
    mfdb_area_temperature(...))
gadget_dir_write(gd, agg_data[[1]])
}
}