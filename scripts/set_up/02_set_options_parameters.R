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
drop_min_da_sqkm <- 5

# threshold for drainage area change analysis
threshold_upstream <- 0.5 # drainage area below 50% of initial drainage area
threshold_downstream <- 1.5 # drainage area above 150% of initial drainage area