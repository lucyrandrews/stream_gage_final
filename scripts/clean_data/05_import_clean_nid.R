## IMPORT AND CLEAN NATIONAL INVENTORY OF DAMS

# This script downloads, imports, and cleans the United States Army Corps of
# Engineers National Inventory of Dams (NID) dataset, which gives the locations
# and attribute of regulated dams. Dams listed in the NID have a high hazard
# potential for loss of human life and/or economic and environmental assets, 
# are taller than 25 feet and store more than 15 acre-feet of water, or are
# taller than 6 feet and store more than 50 acre-feet of water.

# Create directories ----

# create directory
if(!dir.exists(here("data", "raw_data", "nid"))) {
  dir.create(here("data", "raw_data", "nid"))
}



# Download NID California subset of dams ----

# download NID California subset as .xlsx
if(!file.exists(here("data", "raw_data", "nid", "nid_ca.csv"))) {

  nid_url <- "https://nid.usace.army.mil/api/nation/csv"
  
  download.file(url = nid_url,
                destfile = here("data", "raw_data", "nid", "nid_ca.csv"))
  
  rm(nid_url)
  
}



# Import and clean NID ----

# import the NID as a dataframe and filter to only retain records in California
# identified by an NID ID that starts with 'CA' and clean records with duplicate
# NID IDs, which indicate multiple structures at a single project
nid <- read_csv(file = here("data", "raw_data", "nid", "nid_ca.csv"),
                skip = 1) %>%
  rename(nid_id = 'NID ID',
         nid_drainage_area_sqmi = 'Drainage Area (Sq Miles)') %>%
  filter(grepl("CA", nid_id)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"),
           crs = global_crs) %>%
  select(nid_id, nid_drainage_area_sqmi) %>%
  group_by(nid_id) %>%
  summarize(nid_drainage_area_sqmi = max(nid_drainage_area_sqmi))
