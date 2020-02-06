\name{mfdb-package}
\alias{mfdb-package}
\docType{package}

\title{
MareFrame DB querying library
}

\description{
Tools to query a MareFrame DB and reformat results in forms
    useful for GADGET and EwE models.
}

\details{
\tabular{ll}{
Package: \tab mfdb\cr
Type: \tab Package\cr
Version: \tab 6.0-0\cr
Date: \tab 2014-04-23\cr
License: \tab GPL-3\cr
Depends: \tab R (>= 3.1.1)\cr
Imports: \tab 
logging (>= 0.7-103),
DBI (>= 0.3.1),
RPostgreSQL (>= 0.4)\cr
Suggests: \tab unittest\cr
LazyData: \tab true\cr
Built: \tab R 3.1.1; ; 2014-12-04 13:31:31 UTC; unix\cr
}

Index:
\preformatted{
gadget_areafile         Gadget area files
gadget_directory        Gadget directory objects
gadget_file             Gadget file objects
gadget_likelihood_component
                        Gadget likelihood components
mfdb                    MareFrame DB class
mfdb-data               MareFrame DB Datasets
mfdb-package            MareFrame DB querying library
mfdb_area_size          MareFrame DB queries
mfdb_group              MareFrame DB groups
mfdb_import_survey      MareFrame Import functions
mfdb_interval           MareFrame DB intervals
mfdb_step_interval      MareFrame DB intervals
mfdb_unaggregated       MareFrame DB unaggregated data
}

}

\section{Introduction & Schema description}{
Before doing anything with \pkg{mfdb}, it is worth knowing a bit about how data
is stored. Broadly, there are 2 basic types of table in \pkg{mfdb},
\emph{taxonomy} and \emph{measurement} tables.

The measurement tables store all forms of sample data supported, at the finest
available detail. These are then aggregated when using any of the mfdb query
functions. All measurement data is separated by case study, so multiple case
studies can be loaded into a database without conflicts.

Taxonomy tables store all possible values for terms and their meaning, to
ensure consistency in the data. For example, \samp{species} stores short-names
and full latin names of all known species to MFDB, to ensure consistency in naming.

Most Taxonomies have defaults which are populated when the database is created,
and their definitions are stored as data attached to this package. See \link{mfdb-data} for more
information on these. Others, such as \samp{areacell} and \samp{sampling_type} are
case study specific, and you will need to define your terms before you can
import data.
}

\section{Importing data}{
Unless you are working with a remote database, you will need to populate the
database at least once before you are able to do any querying. The steps your
script needs to do are:

\subsection{Connect to database}{
Use the \link{mfdb}() function. This will create tables / populate taxonomies if necessary.
}

\subsection{Define areas & divisions}{
\pkg{mfdb} models space in the following way:
\describe{
  \item{areacell}{
    The finest level of detail stored in the database. Every measurement (e.g.
    temperature, length sample) is assigned to an areacell. This will generally
    correspond to ICES gridcells, however there is no requirement to do so. You
    might augment gridcell information with depth, or include divisions when the
    measurement doesn't correlate to a specific areacell.
  }
  \item{division}{
    Collections of areacells, e.g. ICES subdivisions, or whatever is appropriate.
  }
}
Finally, when querying, divisions are grouped together into named collections,
for instance \code{mfdb_group(north = 1:3, south = 4:6)} will put anything in
divisions 1--3 under an area named "north", 4--5 under an area named "south".

Before you can upload any measurements, you have to define the areacells
that they will use. You do this using the \link{mfdb_import_area}() function. This
allows you to import tables of area/division information, such as:

\code{
mfdb_import_area(mdb, data.frame(
    area = c('101', '102', '103', '401','402', '403'),
    division = c('1', '1', '1', '4', '4', '4'),
    ))
}

If you want areas to be part of multiple divisions, then you can use
\link{mfdb_import_division}() to import extra revisions.
}

\subsection{Define sampling types}{
Any survey data can have a sampling type defined, which then can be used when
querying data. If you want to use a sampling type, then define it using
\link{mfdb_import_sampling_type}().
}

\subsection{Import temperature data}{
At this point, you can start uploading actual measurements. The easiest of
which is temperature. Upload a table of areacell/month/temperature data
using \link{mfdb_import_temperature}().
}

\subsection{Import survey data}{
Finally, import any survey data using \link{mfdb_import_survey}(). Ideally
upload your data in separate chunks. For example, if you have length and
age-length data, don't combine them in R, upload them separately and both
will be used when querying for length data. This keeps the process simple,
and allows you to swap out data as necessary.
}

\subsection{Import stomach survey}{
Stomach surveys are imported in much the same way, however there are 2 data.frames,
one representing predators, one preys. The column \samp{stomach_name} links the
two, which can contain any numeric / character value, as long as it is unique
for predators and prey measurements are assigned to the correct stomach.
}

See \link{mfdb_import_survey} for more information or 
\href{../demo}{the demo directory} for concrete examples.

\subsection{Dumping / Restoring a DB}{
You can also dump/import a dump from another host using the postgres pg_dump
and pg_restore commands. You can dump/restore indvidual schemas (i.e. the 
case study you give to the mfdb() command), to list all the schemas installed
run \code{SELECT DISTINCT(table_schema) FROM information_schema.tables} from
psql. Note that if you use \code{mfdb('Baltic')}, the Postgres schema name will
be lower-cased.

Create a dump of your chosen schema with the following command:

\preformatted{
pg_dump --schema=baltic -F tar mf > baltic.tar
}

This will make a dump of the "baltic" case study into "baltic.tar". It can
then be restored onto another computer with the following:

\preformatted{
pg_restore --clean -d mf baltic.tar
}

If you already have a baltic schema you wish to preserve, you can rename it
first by issuing \code{ALTER SCHEMA baltic RENAME TO baltic_o} in psql. Once
the restore is done you can rename the new schema and put the name of the old
schema back.
}

\subsection{Using the MFDB server}{
There is also a semi-public Postgres database available, hosted by the
University of Iceland. This can be used from your local computer in much the
same way as a local database, see the \link{mfdb-server} page for more
information.
}
}

\section{Querying data}{
There are a selection of querying functions available, all of which work
same way. You give a set of parameters, each of which can be a vector of
data you wish returned, for instance \code{year = 1998:2000} or
\code{species = c('COD')}.

If also grouping by this column (i.e. 'year', 'timestep', 'area' and any
other columns given, e.g. 'age'), then the parameter will control how this
grouping works, e.g. \code{maturity_stage = mfdb_group(imm = 1, mat = 2:5)}
will result in the maturity_stage column having either 'imm' or 'mat'.
These will also be used to generate GADGET aggregation files later.

For example, the following queries the temperature table:

\preformatted{
defaults <- list(
    area = mfdb_group("101" = ),
    timestep = mfdb_timestep_quarterly, # Group months to create 2 timesteps for each year
    year = 1996:2005)
agg_data <- mfdb_temperature(mdb, defaults)
}

All functions will result in a list of data.frame result tables (generally
only one, unless you requested bootstrapping). Each are suitable for
feeding into a gadget function to output into model files.

See \link{mfdb_sample_count} for more information or 
\href{../demo}{the demo directory} for concrete examples.
}

\section{Creating GADGET files}{
Finally, there are a set of functions that turn the output of queries into
GADGET model files. These work on a \link{gadget_directory} object, which
can either be an existing GADGET model to alter, or an empty / nonexistant
directory.

Generally, the result of an mfdb query will be enough to create a
corresponding GADGET file, for instance, the following will create a GADGET
area file in your gadget directory:

\preformatted{
gadget_dir_write(gd,gadget_areafile(
    size = mfdb_area_size(mdb, defaults)[[1]],
    temperature = mfdb_temperature(mdb, defaults)[[1]]))
}

See \link{gadget_areafile} or \link{gadget_likelihood_component} for more
information or \href{../demo}{the demo directory} for concrete examples.

\subsection{Stock and fleet files}{
Stocks and fleets aren't explicitly defined in the database. Instead, they are
definied by querying on a column that differentiates them. For example, if
your "immature cod" stock is definied as cod that is between maturity stages 1
and 2, then if querying for a stockdistribution component, one could do:

\preformatted{
    mfdb_sample_count(mdb, c('maturity_stage', 'age', 'length'), list(
        species = 'COD',
        maturity_stage = c(imm = 1:2, mat = 3:5),
        . . .
    )
}

...and the maturity_stage column will be treated as the stock.

}

}

\section{Acknowledgements}{
This project has received funding from the European Union's Seventh Framework
Programme for research, technological development and demonstration under grant
agreement no.613571.
}

\author{
Jamie Lentin

Maintainer: Jamie Lentin <jamie.lentin@shuttlethread.com>
}

\seealso{
\href{https://github.com/bthe/rgadget}{rgadget},
\href{https://hafro.github.io/gadget2}{Gadget user guide}
}
