library(mfdb)
library(logging)

# Stop on errors
options(error=utils::recover)

# See as much detail as we can about what mfdb is up to
addHandler(writeToConsole, logger='mfdb', level='DEBUG')

# Store the default params we use when querying
defaults <- list(
    area = mfdb_group(
        "101" = c(1011, 1012, 1013, 1014, 1015),
        "102" = c(1021, 1022, 1023)),
    timestep = mfdb_group_numbered("ts", c(1,2,3,4,5,6), c(7,8,9,10,11,12)), # Group months to create 2 timesteps for each year
    null = NULL)

# Connect to a local database, don't keep temporary tables
mdb <- mfdb(save_temp_tables = FALSE)

# Initalise a gadget directory for output, will create it if it doesn't exist
gd <- gadget_directory("out")

# Fetch sizes and temperatures for area, and turn them into an areafile.
gadget_dir_write(gadget_areafile(gd,
    size = mfdb_area_size(mdb, defaults),
    temperature = mfdb_temperature(mdb, defaults)))

# Add a likelihood component for penalty, based on a data.frame
# If there is already a penalty component called "bounds", then this will replace it
gadget_dir_write(gd, gadget_likelihood_component("penalty",
        name = "bounds",
        weight = "0.5",
        data = data.frame(
            switch = c("default"),
            power = c(2),
            stringsAsFactors = FALSE)))

# Get the mean length data from the database, override the areas set in
# defaults above and add a few more.
agg_data <- mfdb_meanlength_stddev(mdb,
        params = c(list(
            year = c(1990, 1991, 1992, 1993),
            area = mfdb_bootstrap_group(2, mfdb_group(
                "101" = c(1011, 1012, 1013, 1014, 1015),
                "102" = c(1021, 1022, 1023))), # NB: Add one more & bootstrap, overriding default
            age = mfdb_group('young' = c(1,2,3), 'old' = c(4,5,6)),
            length = mfdb_interval("len", seq(0, 500, by = 50)),
            species = "COD",
            null = NULL), defaults))
# NB: Since we requested 10 bootstrap samples, agg_data is a vector of 10 data.frames,
# each is annotated with the age/area grouping information to allow us to write
# a likelihood component out to the gadget directory.
# NB: Any additional data manipulation could be performed at this stage, as
# long as the resulting data structure matches.

# Write this component out. In one step, this:-
# * Writes datafile, agefile, areafile to GADGET directory
# * Add / replace codstatistics component in likelihood file
# * Ensure likelihood file is in main data file
gadget_dir_write(gd, gadget_likelihood_component("catchstatistics",
        name = "codstatistics",
        weight = 0.8,
        data = agg_data[[1]],
        fleetnames = c("igfs"),
        stocknames = c("codimm", "codmat")))
# NB: The catchstatistics function is guessed, based on the input data.

# The data.frames are potentially quite large data structures. Free up
# memory before moving on to the next component
rm(agg_data)

# Another likelihood compoment
agg_data <- mfdb_meanweight(mdb,
        params = c(list(
            year = c(1990, 1991, 1992, 1993),
            age = mfdb_group_numbered("age", c(1), c(2), c(3), c(4), c(5)),
            length = mfdb_interval("len", seq(0, 500, by = 50)),
            # species = "COD",
            null = NULL), defaults))

gadget_dir_write(gd, gadget_likelihood_component("catchstatistics",
        name = "weightstatistics",
        weight = 0.8,
        data = agg_data[[1]]))
rm(agg_data) # Free up memory before moving on to the next component

# Another likelihood compoment
agg_data <- mfdb_agelength(mdb,
        params = c(list(
            year = c(1990, 1991),
            length = mfdb_interval("len", seq(0, 500, by = 50)),
            age = mfdb_group('young' = c(1,2,3)),
            null = NULL), defaults))
gadget_dir_write(gd, gadget_likelihood_component("catchdistribution",
        name = "cdist",
        weight = 0.9,
        data = agg_data[[1]]))
rm(agg_data) # Free up memory before moving on to the next component
