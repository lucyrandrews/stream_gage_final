## SET GLOBAL OPTIONS AND ANALYTIC PARAMETERS

# This scripts sets global options and analytic parameters so that all options
# and parameters are located in a single place.

# GLOBAL OPTIONS ----

options(stringsAsFactors = FALSE)

options(scipen = 999)

options(timeout = 30000)



# ANALYTIC PARAMETERS ----

# CRS for all spatial data
global_crs_epsg <- 4269
global_crs <- st_crs(global_crs_epsg)

# NHD ftypes to drop from flowlines objects
drop_ftypes <- c("Coastline")

# minimum total upstream drainage area to drop from flowlines objects
drop_min_da_sqkm <- 0