## LOAD PACKAGES

# This scripts loads all packages needed for this stream gage analysis.
# It also builds `StrmAnlyzeR`, an unpublished package, if it is not already
# built on a user's computer.

# CRAN packages ----

# reproducibility
library(roxygen2)
library(here)
library(devtools)

# tidy syntax
library(tidyverse)
library(magrittr)

# data cleaning and presentation
library(janitor)
library(glue)
library(units)
library(convertr)
library(knitr)
library(kableExtra)

# file import and management
library(tools)
library(readxl)
library(googledrive)
library(foreign)
library(archive)

# spatial data
library(sf)
library(rgdal)
library(USAboundaries)
library(rmapshaper)
try(library(maptools))
library(tmap)
library(tmaptools)

# computation
library(foreach)
library(doParallel)
library(RcppGreedySetCover)

# census data
library(tidycensus)

# visualization
library(ggnewscale)
library(gridExtra)
library(scales)
library(ggsn)
library(ggspatial)
library(ragg)
library(RColorBrewer)
library(forcats)
library(lemon)
library(svglite)

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