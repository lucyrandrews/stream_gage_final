## IMPORT AND CLEAN ACE BIODIVERSITY DATA

# This scripts downloads, imports, and cleans the California Department of Fish
# and Wildlife's Areas of Conservations Emphasis (ACE) aquatic biodiversity
# dataset. This dataset identifies high-priority conservation areas based on
# endemic species richness.

# Create directories ----

# create ACE directory
if(!dir.exists(here("data", "raw_data", "ace"))) {
  dir.create(here("data", "raw_data", "ace"))
}



# Download ACE ----

# download ACE data as a zipped file
if(!file.exists(here("data", "raw_data", "ace", "ds2768.zip"))) {
  
  ace_url <- "https://filelib.wildlife.ca.gov/Public/BDB/GIS/BIOS/Public_Datasets/2700_2799/ds2768.zip"
  
  download.file(url = ace_url,
                destfile = here("data", "raw_data", "ace", "ds2768.zip"))
  
  rm(ace_url)
  
}

# unzip ACE zipped file
unzip(zipfile = here("data", "raw_data", "ace", "ds2768.zip"),
      exdir = here("data", "raw_data", "ace"))



# Import and clean ACE sf object ----

# read in ACE as an sf object and clean
ace <- st_read(dsn = here("data", "raw_data", "ace", "ds2768.gdb")) %>%
  st_transform(crs = global_crs) %>%
  rename(huc12_id = HUC12,
         ace_aq_biodiv_value = BioAqSumSW) %>%
  select(huc12_id, ace_aq_biodiv_value) %>%
  rename_geometry(g = ., name = "geometry")