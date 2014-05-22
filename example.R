# options(error=utils::recover)
library(mfdb)
library(logging)
library(DBI)
library(RPostgreSQL)

options(error=utils::recover)
addHandler(writeToConsole, logger='mfdb', level='DEBUG')

# Define options we need to add to get catch data
opt_catch <- list()
    samplingtype = c(130, 131, 133))

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
mean_len <- mfdb_meanlength_stddev(mdb,
        params = c(list(
            years = c(1990, 1991, 1992, 1993),
            areas = mfdb_bootstrap_group(10, mfdb_group(
                "101" = c(1011, 1012, 1013, 1014, 1015),
                "102" = c(1021, 1022, 1023))), # NB: Add one more & bootstrap, overriding default
            ages = mfdb_group('young' = c(1,2,3), 'old' = c(4,5,6)),
            # NB: We could just keep it unaggregated with mfdb_group("age", c(4), c(5), c(6)), possibly this needs a shortcut
            lengths = mfdb_group_interval("len", 0, 50000, 30), # NB: I don't like specifying lengthcell, but I don't see how to derive it from the existing database.
            # species = "COD",
            null = NULL), opt_catch))

# NB: At this point mean_len is essentially a data.frame that contains the final
# data. The database will be a lot faster if all aggregation happens before the
# data leaves the database, especially when the aggregation is lost. That said,
# adding unaggregated versions would also be reasonably easy. Another option is
# allowing SQL to be tweaked, but we then risk not being able to abstract the DB.

# NB: The groups used for aggregation are attributes attached to mean_len

# Write this data out into a datafile, as well as age and area files. Add the
# component entry to the bottom of the likelihood file
# NB: function is worked out from the input data, although can be overridden.

gadget_dir_write(gd, gadget_likelihood_component("catchstatistics",
        name = "codstatistics",
        weight = 0.8,
        data = mean_len,
        area = attr(mean_len, "areas"),
        age = attr(mean_len, "ages")))
rm(mean_len) # Free up memory before moving on to the next component

# Create a mainfile with everything that has been created so far
gadget_mainfile(gd)
