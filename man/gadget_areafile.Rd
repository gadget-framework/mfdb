\name{gadget_areafile}
\alias{gadget_areafile}

\title{Gadget area files}
\description{
    Structures representing a GADGET area file
}

\usage{
gadget_areafile(size, temperature, area = attr(size, 'area'))
}

\arguments{
    \item{size}{
        data.frame as produced by \code{mfdb_area_size}
    }
    \item{temperature}{
        data.frame as produced by \code{mfdb_temperature}
    }
    \item{area}{
        Optional. \code{mfdb_group} that you used to specify area. By default
        pulls it from annotations on the \code{size} object.
    }
}

\details{
Once formed, you can then use \code{gadget_dir_write} to write this out to a
GADGET areafile.
}

\value{
    List of class 'gadget_areafile' that represents the area file contents.
}

\examples{
\dontrun{# NB: Requires a PostgreSQL installation, see README

# Any example could be added to a file with the following:-
mdb <- mfdb('examples')
area_group <- mfdb_group(
    divA = c("divA"),
    divB = c("divB"),
    divAB = c("divA", "divB"))

# Create an areafile from 2 mfdb queries
areafile <- gadget_areafile(
    mfdb_area_size(mdb, list(
        area = area_group))[[1]],
    mfdb_temperature(mdb, list(
        year = 1998:2000,
        timestep = mfdb_timestep_quarterly,
        area = area_group))[[1]])
areafile

# Write this to a gadget_directory
gadget_dir_write(gadget_directory(tempfile()), areafile)
}
}
