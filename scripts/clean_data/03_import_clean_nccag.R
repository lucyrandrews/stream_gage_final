## IMPORT AND CLEAN NCCAG DATA

# This scripts downloads, imports, and cleans the California Department of Water
# Resources's Natural Communities Commonly Associated with Groundwater (NCCAG)
# dataset, which estimates the spatial locations of vegetation and wetlands that
# signal locations of ecologically significant surface water-groundwater
# interaction and ecosystems that are groundwater-dependent.

# Create directories ----

# create NCCAG directory
if(!dir.exists(here("data", "raw_data", "nccag"))) {
  dir.create(here("data", "raw_data", "nccag"))
}



# Download NCCAG ----

# download NCCAG as a zipped file
if(!file.exists(here("data", "raw_data", "nccag",
                     "i02_naturalcommunitiescommonlyassociatedwithgroundwater.zip"))) {
  
  nccag_url <- "https://data.cnra.ca.gov/dataset/98a26204-354d-44ee-820f-bcef29438c05/resource/748e6895-4954-466c-97d2-1cf3a584e7e9/download/i02_naturalcommunitiescommonlyassociatedwithgroundwater.zip"
  
  download.file(url = nccag_url,
                destfile = here("data", "raw_data", "nccag",
                                "i02_naturalcommunitiescommonlyassociatedwithgroundwater.zip"))
  
  rm(nccag_url)
  
}

# unzip NCCAG
unzip(zipfile = here("data", "raw_data", "nccag",
                     "i02_naturalcommunitiescommonlyassociatedwithgroundwater.zip"),
      exdir = here("data", "raw_data", "nccag"))



# Import and clean NCCAG sf objects ----

# import and clean vegetation layer
nccag_vegetation <- st_read(here("data", "raw_data", "nccag",
                                 "i02_naturalcommunitiescommonlyassociatedwithgroundwater"),
                            layer = "i02_NCCAG_Vegetation") %>%
  st_transform(crs = global_crs) %>%
  mutate(is_valid = st_is_valid(geometry)) %>%
  filter(is_valid) %>%
  select(geometry)

# import and clean wetland layer
nccag_wetland <- st_read(here("data", "raw_data", "nccag",
                              "i02_naturalcommunitiescommonlyassociatedwithgroundwater"),
                         layer = "i02_NCCAG_Wetlands") %>%
  st_transform(crs = global_crs) %>%
  mutate(is_valid = st_is_valid(geometry)) %>%
  filter(is_valid) %>%
  select(geometry)

# create a single object
nccag <- rbind(nccag_vegetation, nccag_wetland) %>%
  mutate(nccag = TRUE)

# clean up
rm(nccag_vegetation, nccag_wetland)


