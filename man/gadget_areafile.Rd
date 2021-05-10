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

# Open a temporary database connection
mdb <- mfdb(tempfile(fileext = '.duckdb'))

# Define 2 areacells of equal size
mfdb_import_area(mdb, data.frame(name=c("divA", "divB"), size=1))

# We want to have 3 area groups, 2 for original cells, one aggregating across the lot
area_group <- mfdb_group(
    divA = c("divA"),
    divB = c("divB"),
    divAB = c("divA", "divB"))

# Make up temperature data
temps <- expand.grid(year=c(1998,2000), month=c(1:12), areacell=c("divA", "divB"))
temps$temperature <- runif(nrow(temps), 5, 10)
mfdb_import_temperature(mdb, temps)

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

# Check data in file matches input data
stopifnot(identical(
    areafile$size,
    c(divA=1, divB=1, divAB=2)))
stopifnot(all.equal(
    mean(areafile$temperature[areafile$temperature$area == 1, 'mean']),
    mean(temps[temps$areacell == 'divA', 'temperature']),
    tolerance = 1e-2))
stopifnot(all.equal(
    mean(areafile$temperature[areafile$temperature$area == 2, 'mean']),
    mean(temps[temps$areacell == 'divB', 'temperature']),
    tolerance = 1e-2))
stopifnot(all.equal(
    mean(areafile$temperature[areafile$temperature$area == 3, 'mean']),
    mean(temps[,'temperature']),
    tolerance = 1e-2))

mfdb_disconnect(mdb)
}
