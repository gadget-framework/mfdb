library(DBI, lib.loc="./Rpackages/")
library(RPostgreSQL, lib.loc="./Rpackages/")
library(devtools, lib.loc="./Rpackages/")
load_all()

# Connect to the given dst2dw database, and set some default
# parameters to use when querying
mdb <- mfdb(dbConnect(dbDriver("PostgreSQL"), dbname="dw0605", host="/tmp/"),
    defaultparams = list(
        areas = c("101", "102", "103", "104", "105"),
        timestep = mfdb_group("ts", c(1,2,3,4,5,6), c(7,8,9,10,11,12)), # Group months to create 2 timesteps for each year
        areastep = mfdb_group("area", 'area1' = c('101', '102'), 'area2' = c('103', 104)))) # Group areas (NB: We've added a name)
# NB: Any of these parameters can be overriden when performing a query

# Initalise a gadget directory for output.
gd <- gadget_directory("./out")

# Fetch sizes and temperatures for areas, and turn them into an areafile.
sizes <- mfdb_area_sizes(mdb)
temps <- mfdb_temperatures(mdb)
gadget_areafile(gd,
        sizes = sizes,
        temperatures = temps)

# Add a likelihood component for penalty, based on a data.frame
# NB: We're not supporting more than one likelihood file, just dumping everything ehre.
gadget_likelihood_component(gd, "penalty",
        name = "bounds",
        weight = "0.5",
        data = data.frame(
            switch = c("default"),
            power = c(2),
            stringsAsFactors = FALSE))

# Get the mean length data from the database, override the areas set in
# defaultparams above and add a few more.
ml <- mfdb_meanlength(mdb,
        generate_stddev = TRUE,
        params = list(
            years = c(2000),
            areas = c("101", "102", "103"),
            species = "COD",
            lengthcellmin = 250,
            lengthcellmax = 500,
            agestep = mfdb_group('age', 'young' = c(1,2,3), 'old' = c(4,5,6)),
            # NB: We could just keep it unaggregated with mfdb_group("age", 4:6),
            lengthcell = 30)) # NB: I don't like specifying lengthcell, but I don't see how to derive it from the existing database.

# NB: At this point mean_len is essentially a data.frame that contains the final
# data. The database will be a lot faster if all aggregation happens before the
# data leaves the database, especially when the aggregation is lost. That said,
# adding unaggregated versions would also be reasonably easy. We could allow
# SQL to be tweaked, but we then risk not being able to abstract the DB.

# NB: Any modification to the data frame could be done here, the area and age
# fields are using raw values at this point

# Write this data out into a datafile, as well as age and area files. Add the
# component entry to the bottom of the likelihood file
# NB: function is worked out from the input data, although can be overridden.
gadget_likelihood_component(gd, "catchstatistics",
        name = "codstatistics",
        weight = 0.8,
        data = ml)

# Create a mainfile with everything that has been created so far
gadget_mainfile(gd)
