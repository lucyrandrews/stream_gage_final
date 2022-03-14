## LOAD PACKAGES

# This scripts loads all packages needed for this stream gage analysis.
# It also builds `StrmAnlyzeR`, an unpublished package, if it is not already
# built on a user's computer.

# CRAN packages ----

# tidy syntax
library(tidyverse)
library(magrittr)

# data cleaning and presentation
library(janitor)
library(glue)
library(units) 
library(knitr)
library(kableExtra)

# file import and management
library(here)
library(devtools)
library(tools)
library(readxl)
library(googledrive)
library(foreign)
library(archive)

# spatial data
library(sf)
library(rgdal)
library(tigris)
library(rmapshaper)
library(maptools)

# computation
library(foreach)
library(doParallel)
library(RcppGreedySetCover)

# census data
library(tidycensus)

# visualization
library(ggnewscale)
library(scales)
library(ggsn)
library(ggspatial)

# hydrologic data
library(nhdR)
library(nhdplusTools)



# Non-CRAN packages ----

# install StrmAnlyzeR package, if needed
if(!"StrmAnlyzeR" %in% rownames(installed.packages())) {
  
  unzip(zipfile = here("packages_unpublished", "StrmAnlyzeR_forked.zip"),
        exdir = here("packages_unpublished"))
  
  build(here("packages_unpublished", "StrmAnlyzeR_master"))
  
  install.packages(here("packages_unpublished", "StrmAnlyzeR_0.1.0.0.tar.gz"),
                   repos = NULL)
  
}

# load StrmAnlyzeR
library(StrmAnlyzeR)