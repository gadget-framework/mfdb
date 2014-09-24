# options(error=utils::recover)
library(mfdb)
library(logging)
library(DBI)
library(RPostgreSQL)

options(error=utils::recover)
addHandler(writeToConsole, logger='mfdb', level='DEBUG')

# Define options we need to add to get catch data
opt_catch <- list()
#    samplingtype = c(130, 131, 133))

# Connect to the given dst2dw database, and set some default
# parameters to use when querying
mdb <- mfdb(dbConnect(dbDriver("PostgreSQL"), dbname="dw0605", host="/tmp/"),
    defaultparams = list(
        areas = mfdb_group(
            "101" = c(1011, 1012, 1013, 1014, 1015),
            "102" = c(1021, 1022, 1023)),
        timestep = mfdb_group_numbered("ts", c(1,2,3,4,5,6), c(7,8,9,10,11,12)), # Group months to create 2 timesteps for each year
        null = NULL))
# NB: Any of these parameters can be overriden when performing a query

# Initalise a gadget directory for output.
gd <- gadget_directory("./out")
# NB: Could have a "remove everything" option here, but it doesn't yet exist.

# Fetch sizes and temperatures for areas, and turn them into an areafile.
# sizes <- mfdb_area_sizes(mdb)
# temps <- mfdb_temperatures(mdb)
# gadget_dir_write(gadget_areafile(gd,
#         sizes = sizes,
#        temperatures = temps))
# NB: I haven't implemented the above, but it's an example of how it might work

# Add a likelihood component for penalty, based on a data.frame
# NB: Currently this just appends components to the likelihood file, however
# an improvement would be to parse the likelihood file, find any existing components
# with the same name/type combination, and replace them if they exist.
# This allows you to regenerate the sections that need regenerating, and leave the
# others untouched.
gadget_dir_write(gd, gadget_likelihood_component("penalty",
        name = "bounds",
        weight = "0.5",
        data = data.frame(
            switch = c("default"),
            power = c(2),
            stringsAsFactors = FALSE)))

# Get the mean length data from the database, override the areas set in
# defaultparams above and add a few more.
agg_data <- mfdb_meanlength_stddev(mdb,
        params = c(list(
            years = c(1990, 1991, 1992, 1993),
            areas = mfdb_bootstrap_group(2, mfdb_group(
                "101" = c(1011, 1012, 1013, 1014, 1015),
                "102" = c(1021, 1022, 1023))), # NB: Add one more & bootstrap, overriding default
            ages = mfdb_group('young' = c(1,2,3), 'old' = c(4,5,6)),
            lengths = mfdb_interval("len", seq(0, 500, by = 50)),
            # species = "COD",
            null = NULL), opt_catch))

# NB: Since we requested 10 bootstrap samples, agg_data is a vector of 10 items,
# each is essentially a data.frame that contains the final data. The data here
# has been wrapped up into divisions, ready to be outputted to a gadget file.
# This is for speed, although there's no reason not to specify each individual
# subdivision and then do the aggregation within R.

# Write this data out into a datafile, as well as age and area files. Add the
# component entry to the bottom of the likelihood file
# NB: function is worked out from the input data, although can be overridden.
gadget_dir_write(gd, gadget_likelihood_component("catchstatistics",
        name = "codstatistics",
        weight = 0.8,
        data = agg_data[[1]],
        fleetnames = c("igfs"),
        stocknames = c("codimm", "codmat")))
rm(agg_data) # Free up memory before moving on to the next component

agg_data <- mfdb_meanweight(mdb,
        params = c(list(
            years = c(1990, 1991, 1992, 1993),
            ages = mfdb_group_numbered("age", c(1), c(2), c(3), c(4), c(5)),
            lengths = mfdb_interval("len", seq(0, 500, by = 50)),
            # species = "COD",
            null = NULL), opt_catch))

gadget_dir_write(gd, gadget_likelihood_component("catchstatistics",
        name = "weightstatistics",
        weight = 0.8,
        data = agg_data[[1]]))
rm(agg_data) # Free up memory before moving on to the next component

agg_data <- mfdb_agelength(mdb,
        params = c(list(
            years = c(1990, 1991),
            lengths = mfdb_interval("len", seq(0, 500, by = 50)),
            ages = mfdb_group('young' = c(1,2,3)),
            null = NULL), opt_catch))
gadget_dir_write(gd, gadget_likelihood_component("catchdistribution",
        name = "cdist",
        weight = 0.9,
        data = agg_data[[1]]))
rm(agg_data) # Free up memory before moving on to the next component

# Create a mainfile with everything that has been created so far
# gadget_mainfile(gd)
