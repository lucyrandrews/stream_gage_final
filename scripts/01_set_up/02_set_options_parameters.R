## SET GLOBAL OPTIONS AND ANALYTIC PARAMETERS

# This scripts sets global options and analytic parameters so that all options
# and parameters are located in a single place.

# Global options ----

# import strings unfactored
options(stringsAsFactors = FALSE)

# display digits rather than default to scientific notation for most numbers
options(scipen = 999)

# set a high timeout limit in case large file downloads are slow
options(timeout = 30000)



# Computational options ----

# specify the number of cores to use and leave one free for CPU management
n_cores <- detectCores() - 1

# register parallel processing
registerDoParallel(cores = n_cores)



# Analytic parameters ----

# CRS for all spatial data
global_crs_epsg <- 4269
global_crs <- st_crs(global_crs_epsg)

# NHD ftypes to drop from flowlines objects
drop_ftypes <- c("Coastline")

# minimum total upstream drainage area to drop from flowlines objects
flowlines_min_dasqkm <- 5

# threshold for drainage area change analysis
threshold_upstream <- 0.5 # drainage area below 50% of initial drainage area
threshold_downstream <- 1.5 # drainage area above 150% of initial drainage area

# parameters for dam-flowlines association with nearest neighbor search
min_nid_dasqkm <- 0 # minimum drainage area a dam can have
nn_search_radius <- 0.01 # roughly 1.1km
# absolute difference max between dam drainage area and flowlines drainage area
da_dif_max <- 25
# relative differences between dam drainage area and flowline drainage area
# outside of which a dam will not be associated with a flowline
da_dif_ratio_min <- 0.5
da_dif_ratio_max <- 2